/*
 * Copyright 2025-2025 Ossum Inc.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.ossuminc.svg2laminar

import scala.xml.{Comment, Elem, MetaData, NamespaceBinding, Node, NodeSeq, ProcInstr, XML}
import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.util.Try
import scala.collection.immutable.Seq
import scala.reflect.ClassTag

object SvgToLaminar:
  case class ValidationResult(
    isValid: Boolean,
    differences: List[String] = Nil
  )

  case class ConversionResult(
    fileName: String,
    laminarCode: String,
    originalSvg: String,
    validation: ValidationResult,
    warnings: List[String] = Nil,
    errors: List[String] = Nil,
    conversionLog: List[String] = Nil
  )

  // Helper methods for XML namespace handling
  extension (ns: NamespaceBinding)
    private def toSeq: Seq[NamespaceBinding] =
      if ns == null then Seq.empty
      else ns +: Option(ns.parent).map(_.toSeq).getOrElse(Seq.empty)
      end if
  end extension

  // Helper methods for MetaData handling
  extension (md: MetaData)
    private def asMap: Map[String, String] =
      if md == null then Map.empty
      else
        md match
          case pa: scala.xml.PrefixedAttribute =>
            val head = Map(s"${pa.pre}:${pa.key}" -> pa.value.text)
            val tail = Option(pa.next).map(_.asMap).getOrElse(Map.empty)
            head ++ tail
          case a: scala.xml.UnprefixedAttribute =>
            Map(a.key -> a.value.text) ++ Option(a.next).map(_.asMap).getOrElse(Map.empty)
          case _ => Option(md.next).map(_.asMap).getOrElse(Map.empty)
        end match
      end if
    end asMap
  end extension

  private class ConversionContext:
    private val logBuffer = collection.mutable.ListBuffer[String]()
    private val warningBuffer = collection.mutable.ListBuffer[String]()
    private val errorBuffer = collection.mutable.ListBuffer[String]()

    def log(message: String): Unit =
      logBuffer += s"[INFO] $message"

    def warn(message: String): Unit =
      warningBuffer += message
      logBuffer += s"[WARN] $message"

    def error(message: String): Unit =
      errorBuffer += message
      logBuffer += s"[ERROR] $message"

    def getLog: List[String] = logBuffer.toList
    def getWarnings: List[String] = warningBuffer.toList
    def getErrors: List[String] = errorBuffer.toList
  end ConversionContext

  private def generateScalaDocs(node: Node): String =
    val title = (node \\ "title").headOption.map(_.text).getOrElse("")
    val desc = (node \\ "desc").headOption.map(_.text).getOrElse("")
    s"""/**
       | * ${if title.nonEmpty then title else "SVG Component"}
       | * ${if desc.nonEmpty then s"\n * Description: $desc" else ""}
       | * @param size The size of the SVG (both width and height)
       | */""".stripMargin
  end generateScalaDocs

  private def extractViewBox(node: Node): (String, List[String]) =
    val warnings = List.newBuilder[String]
    val viewBox = node.attribute("viewBox").map(_.text).getOrElse {
      val width = node.attribute("width").map(_.text)
      val height = node.attribute("height").map(_.text)
      (width, height) match
        case (Some(w), Some(h)) => s"0 0 $w $h"
        case _ =>
          warnings += "No viewBox attribute found and no width/height to create one"
          ""
      end match
    }
    (viewBox, warnings.result())
  end extractViewBox

  private def extractDefs(node: Node, context: ConversionContext): String =
    val defsNodes = node \\ "defs"
    if defsNodes.isEmpty then
      context.log("No defs elements found in SVG")
      ""
    else
      context.log(s"Found ${defsNodes.length} defs elements")
      defsNodes.map(defs => convertNode(defs, includeRoot = true, context)).mkString(",\n      ")
    end if
  end extractDefs

  private def convertAttributes(elem: Elem, context: ConversionContext): Seq[String] =
    // Handle namespaces using extension method
    val nsDecls = elem.scope.toSeq.filter(_.uri != null).map { ns =>
      val prefix = Option(ns.prefix).map(p => s"_$p").getOrElse("")
      context.log(s"Processing namespace: ${if prefix.nonEmpty then ns.prefix else "default"}")
      s"""svg.xmlns$prefix := "${ns.uri}""""
    }

    // Handle attributes using extension method
    val attrs = elem.attributes.asMap.map { (key, value) =>
      context.log(s"Converting attribute: $key")
      key match
        case "class" =>
          s"""svg.cls := "$value""""
        case "style" =>
          convertStyle(value, context)
        case "fill" if value.startsWith("url(") =>
          s"""svg.fill := "$value""""
        case attr if attr.contains(":") =>
          s"""svg.attr("$attr") := "$value""""
        case "d" =>
          context.log("Processing path data")
          s"""svg.d := "$value""""
        case "points" =>
          context.log("Processing polygon/polyline points")
          s"""svg.points := "$value""""
        case "transform" =>
          context.log("Processing transform attribute")
          s"""svg.transform := "$value""""
        case "viewBox" =>
          s"""svg.viewBox := "$value""""
        case "preserveAspectRatio" =>
          s"""svg.preserveAspectRatio := "$value""""
        case "id" =>
          s"""svg.idAtter := "$value"""
        case "stop-color" =>
          s"""svg.stopColor := "$value"""
        case attr =>
          s"""svg.$attr := "$value""""
    }
    nsDecls ++ attrs.toSeq
  end convertAttributes

  private def convertStyle(style: String, context: ConversionContext): String =
    context.log("Converting style attribute")
    val styleMap = style
      .split(";")
      .map(_.trim)
      .filter(_.nonEmpty)
      .map { s =>
        val parts = s.split(":")
        if parts.length == 2 then
          context.log(s"Processing style property: ${parts(0).trim}")
          s""""${parts(0).trim}" -> "${parts(1).trim}""""
        else
          context.warn(s"Invalid style declaration: $s")
          ""
      }
      .filter(_.nonEmpty)
      .mkString(", ")
    s"""svg.style := Map($styleMap)"""
  end convertStyle

  private def convertNode(
    node: Node,
    includeRoot: Boolean = true,
    context: ConversionContext
  ): String =
    node match
      case elem: Elem =>
        context.log(s"Converting ${elem.label} element")
        val textContent = elem.child
          .collect {
            case text if text.text.trim.nonEmpty => text.text.trim
          }
          .mkString(" ")

        val children = elem.child.collect {
          case e: Elem =>
            convertNode(e, includeRoot = true, context)
          case Comment(text) =>
            s"""// $text"""
          case ProcInstr(target, proctext) =>
            s"""// <?$target $proctext?>"""
        }

        val attributes = convertAttributes(elem, context)

        val textAttr = if textContent.nonEmpty then
          context.log(s"Preserving text content: $textContent")
          Seq(s"""svg.textContent := "$textContent"""")
        else Seq.empty

        val allAttrs = attributes ++ textAttr
        val attributesStr = if allAttrs.isEmpty then "" else allAttrs.mkString(",\n      ")
        val childrenStr = if children.isEmpty then "" else children.mkString(",\n      ")

        val separator = if children.nonEmpty && attributesStr.nonEmpty then "," else ""

        if !includeRoot && elem.label == "svg" then s"""$attributesStr$separator
             |      $childrenStr""".stripMargin
        else s"""svg.${elem.label}(
             |      $attributesStr$separator
             |      $childrenStr
             |    )""".stripMargin

      case Comment(text) =>
        context.log(s"Preserving comment: $text")
        s"""// $text"""

      case ProcInstr(target, proctext) =>
        context.log(s"Preserving processing instruction: <?$target $proctext?>")
        s"""// <?$target $proctext?>"""

      case _ => ""
    end match
  end convertNode

  private def extractComments(node: Node): Seq[String] =
    node.child.collect { case Comment(text) =>
      text.trim
    }.toSeq
  end extractComments

  private def convertWithContext(inputFile: String, context: ConversionContext): String =
    val svg = XML.loadFile(inputFile)
    val fileName = new File(inputFile).getName.replaceAll("\\.svg$", "")

    context.log("Extracting SVG attributes and content")
    val (viewBox, viewBoxWarnings) = extractViewBox(svg)
    viewBoxWarnings.foreach(context.warn)

    val defs = extractDefs(svg, context)
    val comments = extractComments(svg).map(c => s"// $c").mkString("\n  ")

    context.log("Converting SVG content to Laminar code")
    val laminarContent = convertNode(svg, includeRoot = false, context)

    s"""package synapify.ui.components.icons
       |
       |import com.raquo.laminar.api.L.*
       |
       |object ${fileName.capitalize}:
       |  $comments
       |  // Generated from SVG file: $fileName.svg
       |  ${generateScalaDocs(svg)}
       |  def apply(
       |    size: Int = 48,
       |  ) =
       |    svg.svg(
       |      svg.width := size.toString,
       |      svg.height := size.toString,
       |      ${if viewBox.nonEmpty then s"""svg.viewBox := "$viewBox",""" else ""}
       |      $defs
       |      $laminarContent
       |    )""".stripMargin
  end convertWithContext

  def convertFile(inputPath: String, outputPath: String): ConversionResult =
    val input = Paths.get(inputPath)
    val outDir = Paths.get(outputPath)
    require(Files.isReadable(input) && Files.isWritable(outDir))
    val context = new ConversionContext()
    val code = convertWithContext(inputPath, context)
    if outputPath.nonEmpty && Files.isDirectory(outDir) && Files.isWritable(outDir) then
      val inputFileName = input.getFileName.toString
      val outputFileName =
        if inputFileName.endsWith(".svg") then inputFileName.dropRight(4) + ".scala"
        else inputFileName
      val outputFilePath = Path.of(outputPath, outputFileName)
      Files.write(outputFilePath, code.getBytes)
    else println(code)
    end if
    val originalSvg = new String(Files.readAllBytes(input))
    val validation = validateConversion(originalSvg, code)
    ConversionResult(
      inputPath,
      code,
      originalSvg,
      validation,
      context.getWarnings,
      context.getErrors,
      context.getLog
    )
  end convertFile

  def convertDirectory(directoryPath: String, outputPath: String): Seq[ConversionResult] =
    val directory = new File(directoryPath)
    if !directory.isDirectory then
      throw IllegalArgumentException(s"$directoryPath is not a directory")

    val svgFiles = directory.listFiles().filter(_.getName.toLowerCase.endsWith(".svg"))
    svgFiles.map { file =>
      val context = new ConversionContext()
      context.log(s"Processing file: ${file.getName}")

      Try(convertWithContext(file.getPath, context))
        .map { code =>
          val outputFile = new File(outputPath, s"${file.getName.replaceAll("\\.svg$", "")}.scala")
          Files.write(outputFile.toPath, code.getBytes)
          context.log(s"Generated Scala code written to: ${outputFile.getAbsolutePath}")

          val originalSvg = new String(Files.readAllBytes(file.toPath))
          val validation = validateConversion(originalSvg, code)
          if !validation.isValid then
            context.warn("Validation found differences between original and generated SVG")
            validation.differences.foreach(d => context.warn(s"  - $d"))

          ConversionResult(
            file.getName,
            code,
            originalSvg,
            validation,
            context.getWarnings,
            context.getErrors,
            context.getLog
          )
        }
        .recover { case e: Exception =>
          context.error(s"Failed to convert file: ${e.getMessage}")
          ConversionResult(
            file.getName,
            "",
            "",
            ValidationResult(false),
            context.getWarnings,
            context.getErrors :+ e.getMessage,
            context.getLog
          )
        }
        .get
    }.toSeq
  end convertDirectory

  private def validateConversion(originalSvg: String, generatedCode: String): ValidationResult =
    val differences = collection.mutable.ListBuffer[String]()

    val originalElements = extractElements(originalSvg)
    val generatedElements = extractElements(generatedCode)

    val missingElements = originalElements.diff(generatedElements)
    if missingElements.nonEmpty then
      differences += s"Missing elements in generated code: ${missingElements.mkString(", ")}"

    val originalAttrs = extractAttributes(originalSvg)
    val generatedAttrs = extractAttributes(generatedCode)

    val missingAttrs = originalAttrs.diff(generatedAttrs)
    if missingAttrs.nonEmpty then
      differences += s"Missing attributes in generated code: ${missingAttrs.mkString(", ")}"

    ValidationResult(
      isValid = differences.isEmpty,
      differences = differences.toList
    )
  end validateConversion

  private def extractElements(content: String): Set[String] =
    val ElementPattern = """<(\w+)[ >]""".r
    ElementPattern.findAllMatchIn(content).map(_.group(1)).toSet
  end extractElements

  private def extractAttributes(content: String): Set[String] =
    val AttrPattern = """\b(\w+)=["']""".r
    AttrPattern.findAllMatchIn(content).map(_.group(1)).toSet
  end extractAttributes

  @main def run(args: String*): Unit =
    args.toList match
      case inputPath :: outputPath :: Nil =>
        val path = Paths.get(inputPath)
        if Files.isRegularFile(path) then
          val results = convertFile(inputPath, outputPath)
          reportResults(Seq(results))
        else
          val results = convertDirectory(inputPath, outputPath)
          reportResults(results)
      case inputPath :: Nil =>
        val results = convertFile(inputPath, "")
        reportResults(Seq(results))
      case _ =>
        println("Usage: svg2laminar <input-file-or-directory> [output-directory]")
        System.exit(1)

  private def reportResults(results: Seq[ConversionResult]): Unit =
    results.foreach { result =>
      println("\n=== Processing Report ===")
      println(s"File: ${result.fileName}")

      println("\nConversion Log:")
      result.conversionLog.foreach(println)

      if result.validation.differences.nonEmpty then
        println("\nValidation Differences:")
        result.validation.differences.foreach(d => println(s"  - $d"))

      if result.warnings.nonEmpty then
        println("\nWarnings:")
        result.warnings.foreach(w => println(s"  - $w"))

      if result.errors.nonEmpty then
        println("\nErrors:")
        result.errors.foreach(e => println(s"  - $e"))

      println("\nValidation Status: " + (if result.validation.isValid then "PASSED" else "FAILED"))
    }
  end reportResults
end SvgToLaminar
