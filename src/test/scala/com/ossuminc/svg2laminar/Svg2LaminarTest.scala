/*
 * Copyright 2025-2025 Ossum Inc.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.ossuminc.svg2laminar

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers

import java.nio.file.{Paths, Files, Path}

class Svg2LaminarTest extends AnyWordSpec with Matchers {

  "SvgLaminar" must {
    "convert a single file" in {
      val inputDir = "src/test/input/"
      val outputDir = "target/test"
      val fileName = "electron.svg"
      val input = inputDir + "/electron.svg"
      val result = SvgToLaminar.convertFile(input, outputDir)
      result.fileName must be(input)
      if result.errors.nonEmpty then fail(result.errors.mkString("\n"))
      end if

      if Files.exists(Paths.get(outputDir)) then
        val outFile = Path.of(outputDir, "electron.scala")
        if Files.exists(outFile) then succeed else fail(s"File $outFile does not exist")
      else fail(s"Directory $outputDir does not exist")
    }

    "convert a directory" in {
      val inputDir = "src/test/input/"
      val outputDir = "target/test"
      val results = SvgToLaminar.convertDirectory(inputDir, outputDir)
      for {
        result <- results
      } {
        if result.errors.nonEmpty then fail(result.errors.mkString("\n"))
        end if
        if Files.exists(Paths.get(outputDir)) then
          val outFile = Path.of(outputDir, "electron.scala")
          if Files.exists(outFile) then succeed else fail(s"File $outFile does not exist")
        else fail(s"Directory $outputDir does not exist")
      }
      succeed
    }
  }
}
