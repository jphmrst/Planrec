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
import scala.collection.immutable.{Seq,IndexedSeq}

object RuleItemSamples1 {
  import org.maraist.planrec.rules.{One,All,Act}
  import org.maraist.planrec.terms.Term.StringAsTerm

  val aa = Act[String, String]("A", "a")
  val bb = Act[String, String]("B", "b")
  val cc = Act[String, String]("C", "c")
  val nn = One[String, String]("N", Seq("A", "B"), Seq(0.4, 0.6))
  val pp = One[String, String]("P", Seq("A", "C"), Seq(0.3, 0.7))
  val mm = All[String, String]("M", IndexedSeq("N", "P"), Array[(Int,Int)]((0,1)))
  val rr = All[String, String]("R", IndexedSeq("N", "P"), Array[(Int,Int)]())
}

