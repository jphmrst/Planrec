// Copyright (C) 2021 John Maraist
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.generaltest
import scala.language.adhocExtensions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.maraist.planrec.yappr.Yappr

class YapprTests extends AnyFlatSpec with Matchers {

  import org.maraist.planrec.rules.{HTNLib, All, One, Act}
  import org.maraist.planrec.terms.String.StringAsTerm

  def verifyBadLibrary(
    blurb: String,
    lib: HTNLib[String, String, Unit]
  ): Unit =

    blurb `should` "be detected as invalid for Yappr" in {
      (Yappr.isValidLibrary(lib)) `should` be (false)
    }

  verifyBadLibrary(
    "Simple loop in All rule",
    HTNLib[String, String, Unit](
      Set(
        All("A", IndexedSeq("A"), Array.empty[(Int, Int)]),
      ),
      Seq("A"),
      Seq[Double](1.0)
    ))
  verifyBadLibrary(
    "Simple loop in One rule",
    HTNLib[String, String, Unit](
      Set(
        One("A", Seq("A", "B"), Seq[Double](0.3, 0.7)),
        Act("B", s"B"),
      ),
      Seq("A"),
      Seq[Double](1.0)
    ))
}
