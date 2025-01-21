/*
 * Copyright 2024-2025 Ossum Inc.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

import scala.xml.{XML, Node, Elem, NodeSeq}
import scala.io.Source
import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.util.{Try, Success, Failure}

object SVGToLaminar:
  // Domain models
  case class ConversionResult(
    fileName: String,
    laminarCode: String,
    warnings: List[String] = Nil,
    errors: List[String] = Nil
  )
  
  enum SVGTransform:
    case Matrix(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double)
    case Translate(x: Double, y: Option[Double] = None)
    case Scale(x: Double, y: Option[Double] = None)
    case Rotate(angle: Double, cx: Option[Double] = None, cy: Option[Double] = None)
    case SkewX(angle: Double)
    case SkewY(angle: Double)

  object SVGTransform:
    def parse(transform: String): List[SVGTransform] =
      val TransformPattern = """(\w+)\s*\((.*?)\)""".r
      TransformPattern.findAllMatchIn(transform).map { m =>
        val function = m.group(1)
        val params = m.group(2).split(""",\s*""").map(_.trim).map(_.toDoubleOption.getOrElse(0.0))
        
        function match
          case "matrix" if params.length == 6 => 
            Matrix(params(0), params(1), params(2), params(3), params(4), params(5))
          case "translate" if params.nonEmpty => 
            Translate(params(0), params.lift(1))
          case "scale" if params.nonEmpty => 
            Scale(params(0), params.lift(1))
          case "rotate" if params.nonEmpty => 
            Rotate(params(0), params.lift(1), params.lift(2))
          case "skewX" if params.nonEmpty => 
            SkewX(params(0))
          case "skewY" if params.nonEmpty => 
            SkewY(params(0))
          case _ => 
            Translate(0, None) // Default transform for unknown functions
      }.toList

  // Constants
  private val complexAttributes = Set(
    "transform", "d", "points", "viewBox", "preserveAspectRatio",
    "gradientTransform", "patternTransform", "maskContentUnits"
  )

  // Extension methods for better syntax
  extension (node: Node)
    def extractViewBox: (String, List[String]) =
      val warnings = List.newBuilder[String]
      val viewBox = node.attribute("viewBox").map(_.text).getOrElse:
        warnings += "No viewBox attribute found, this may affect scaling"
        ""
      (viewBox, warnings.result())

    def extractDefs: String =
      val defsNode = node \\ "defs"
      if defsNode.nonEmpty then convertNode(defsNode.head) + "," else ""

  // Main conversion methods
  def convertDirectory(directoryPath: String, outputPath: String): Seq[ConversionResult] =
    val directory = File(directoryPath)
    if !directory.isDirectory then
      throw IllegalArgumentException(s"$directoryPath is not a directory")

    val svgFiles = directory.listFiles().filter(_.getName.toLowerCase.endsWith(".svg"))
    svgFiles.map { file =>
      Try(convert(file.getPath))
        .map { code =>
          val outputFile = File(outputPath, s"${file.getName.replaceAll("\\.svg$", "")}.scala")
          Files.write(outputFile.toPath, code.getBytes)
          ConversionResult(file.getName, code)
        }
        .recover { case e: Exception =>
          ConversionResult(file.getName, "", errors = List(e.getMessage))
        }
        .get
    }.toSeq

  def convert(svgFile: String): String =
    val svg = XML.loadFile(svgFile)
    val fileName = File(svgFile).getName.replaceAll("\\.svg$", "")
    
    val (viewBox, warnings) = svg.extractViewBox
    val defs = svg.extractDefs
    
    val laminarContent = convertNode(svg, includeRoot = false)
    
    s"""import com.raquo.laminar.api.L.*
       |
       |object ${fileName.capitalize}:
       |  // Generated from SVG file: $fileName.svg
       |  ${generateScalaDocs(svg)}
       |  def apply(
       |    size: Int = 48,
       |    className: Option[String] = None,
       |    customAttributes: Seq[svg.SVGAttr[_]] = Seq.empty
       |  ) =
       |    svg.svg(
       |      svg.width := size.toString,
       |      svg.height := size.toString,
       |      ${if viewBox.nonEmpty then s"""svg.viewBox := "$viewBox",""" else ""}
       |      className.map(svg.cls := _).toSeq,
       |      customAttributes,
       |      $defs
       |      $laminarContent
       |    )
       |
       |  ${generateConstants(svg)}""".stripMargin

  private def convertNode(node: Node, includeRoot: Boolean = true): String = node match
    case elem: Elem =>
      val children = elem.child.filterNot(_.text.trim.isEmpty).map(convertNode(_,false))
      val attributes = convertAttributes(elem)
      
      val childrenStr = if children.isEmpty then "" else children.mkString(",\n      ")
      val attributesStr = if attributes.isEmpty then "" else attributes.mkString(",\n      ")
      
      val separator = if children.nonEmpty && attributes.nonEmpty then "," else ""
      
      if !includeRoot && elem.label == "svg" then
        s"""$attributesStr$separator
           |      $childrenStr""".stripMargin
      else
        s"""svg.${elem.label}(
           |      $attributesStr$separator
           |      $childrenStr
           |    )""".stripMargin
         
    case _ => ""
  
  private def convertAttributes(elem: Elem): Seq[String] =
    elem.attributes.asAttrMap.map { (key, value) =>
      key match
        case "class" => s"""svg.cls := "$value""""
        case "style" => convertStyle(value)
        case attr if complexAttributes.contains(attr) => 
          convertComplexAttribute(attr, value)
        case _ => s"""svg.$key := "$value""""
    }.toSeq

  private def convertStyle(style: String): String =
    val styleMap = style.split(";")
      .map(_.trim)
      .filter(_.nonEmpty)
      .map { s =>
        val parts = s.split(":")
        if parts.length == 2 then
          s""""${parts(0).trim}" -> "${parts(1).trim}""""
        else ""
      }
      .filter(_.nonEmpty)
      .mkString(", ")
    
    s"""svg.style := Map($styleMap)"""

  private def convertComplexAttribute(attr: String, value: String): String = attr match
    case "transform" => 
      val transforms = SVGTransform.parse(value)
      val transformStr = transforms.map {
        case SVGTransform.Matrix(a, b, c, d, e, f) => 
          s"matrix($a,$b,$c,$d,$e,$f)"
        case SVGTransform.Translate(x, Some(y)) => 
          s"translate($x,$y)"
        case SVGTransform.Translate(x, None) => 
          s"translate($x)"
        case SVGTransform.Scale(x, Some(y)) => 
          s"scale($x,$y)"
        case SVGTransform.Scale(x, None) => 
          s"scale($x)"
        case SVGTransform.Rotate(angle, Some(x), Some(y)) => 
          s"rotate($angle,$x,$y)"
        case SVGTransform.Rotate(angle, _, _) => 
          s"rotate($angle)"
        case SVGTransform.SkewX(angle) => 
          s"skewX($angle)"
        case SVGTransform.SkewY(angle) => 
          s"skewY($angle)"
      }.mkString(" ")
      s"""svg.transform := "$transformStr""""
    case "d" => s"""svg.d := "$value""""
    case "points" => s"""svg.points := "$value""""
    case "viewBox" => s"""svg.viewBox := "$value""""
    case "preserveAspectRatio" => s"""svg.preserveAspectRatio := "$value""""
    case _ => s"""svg.$attr := "$value""""

  private def generateScalaDocs(svg: Node): String =
    val title = (svg \\ "title").headOption.map(_.text).getOrElse("")
    val desc = (svg \\ "desc").headOption.map(_.text).getOrElse("")
    
    s"""/**
       | * ${if title.nonEmpty then title else "SVG Component"}
       | * ${if desc.nonEmpty then s"\n * Description: $desc" else ""}
       | *
       | * @param size The size of the SVG (both width and height)
       | * @param className Optional CSS class name to apply to the SVG
       | * @param customAttributes Additional SVG attributes to apply
       | */""".stripMargin

  private def generateConstants(svg: Node): String =
    val colors = extractColors(svg)
    if colors.isEmpty then return ""

    s"""  // Color constants used in this SVG
       |  object Colors:
       |    ${colors.map { (name, value) =>
         s"""val $name = "$value""""
       }.mkString("\n    ")}""".stripMargin

  private def extractColors(node: Node): Map[String, String] =
    val colorPattern = """#[0-9A-Fa-f]{3,8}|rgb\([^)]+\)|rgba\([^)]+\)""".r
    val colors = collection.mutable.Map[String, String]()
    
    def processNode(n: Node): Unit =
      n.attributes.asAttrMap.foreach { (_, value) =>
        colorPattern.findAllIn(value).foreach { color =>
          val colorName = generateColorName(color)
          colors += (colorName -> color)
        }
      }
      n.child.foreach(processNode)
    
    processNode(node)
    colors.toMap

  private def generateColorName(color: String): String =
    val sanitized = color.replaceAll("[^0-9A-Fa-f]", "")
    s"color_$sanitized"

  @main def run(args: String*): Unit =
    args.toList match
      case inputPath :: outputPath :: Nil =>
        val results = convertDirectory(inputPath, outputPath)
        reportResults(results)
      
      case inputPath :: Nil =>
        val output = convert(inputPath)
        println(output)
      
      case _ =>
        println("Usage: SVGToLaminarConverter <input-file-or-directory> [output-directory]")
        System.exit(1)

  private def reportResults(results: Seq[ConversionResult]): Unit =
    results.foreach { result =>
      println(s"\nProcessed: ${result.fileName}")
      if result.warnings.nonEmpty then
        println("Warnings:")
        result.warnings.foreach(w => println(s"  - $w"))
      if result.errors.nonEmpty then
        println("Errors:")
        result.errors.foreach(e => println(s"  - $e"))
    }
