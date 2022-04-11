/*
 * Copyright 2016-2019 E257.FI
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package fi.e257.testing

import java.nio.file.Path

import better.files.File
import org.scalactic.TripleEquals._
import org.scalatest.StreamlinedXmlEquality._

/**
 * Default validators for test cases. Validator is selected
 * by [[DirSuite.selectValidator]]-method, which can be overloaded
 * test-by-test class basis.
 *
 * Validator interface is:
 *
 * (testname: Path, reference: Path, output:Path) => Option[String]
 *
 * When None is successfull validation, and Some(ErrMsg)
 * is validation error.
 */
@SuppressWarnings(Array("org.wartremover.warts.ToString"))
object TestValidator {

  /**
   * Validate text based output against reference.
   * This validation doesn't check eol-changes.
   *
   * @param testname full path to test's exec-file
   * @param reference full path to reference file
   * @param output full path to output file
   * @return Either None or Some(Validation error message)
   */
  def txtValidator(testname: Path, reference: Path, output: Path): Option[String] = {

    val txtFirst = File(output).lines.mkString

    val txtSecond = File(reference).lines.mkString

    if (txtFirst === txtSecond) {
      None
    } else {
      Some("test vectors are different")
    }
  }

  /**
   * Validate XML-based output against reference.
   * This validation doesn't check exact match of
   * XML, but compares DOMs. For example, pretty-printed
   * and non-pretty printed validates same.
   *
   * Also, if one file is missing xml-declaration,
   * they still validates same!
   *
   * @param testname full path to test's exec-file
   * @param reference full path to reference file
   * @param output full path to output file
   * @return Either None or Some(Validation error message)
   */
  def xmlValidator(testname: Path, reference: Path, output: Path): Option[String] = {
    val xmlReference = scala.xml.XML.loadFile(reference.toString)
    val xmlOutput = scala.xml.XML.loadFile(output.toString)

    if (xmlReference === xmlOutput) {
      None
    } else {
      Some("test vectors are different")
    }
  }
}
