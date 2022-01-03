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
import org.maraist.planrec.terms.Term.
  {RenderCharAsTerm, RenderStringAsTerm}

object RuleItemSamples1 {
  import org.maraist.planrec.rules.{One,All,Act,HTNLib}
  import org.maraist.planrec.yr.{Item, AllItem, OneItem, ActItem}
  import org.maraist.planrec.terms.Term.{StringAsTerm,CharAsTerm}

  val aa = Act[String, String, Unit]("A", "a")
  val bb = Act[String, String, Unit]("B", "b")
  val cc = Act[String, String, Unit]("C", "c")
  val dd = Act[String, String, Unit]("D", "d")
  val nn = One[String, String, Unit]("N", IndexedSeq("A", "B"), Seq(0.4, 0.6))
  val pp = One[String, String, Unit]("P", IndexedSeq("A", "C"), Seq(0.3, 0.7))
  val qq = One[String, String, Unit]("Q", IndexedSeq("B", "C"), Seq(0.3, 0.7))
  val mm = All[String, String, Unit]("M", IndexedSeq("N", "P"), Array((0, 1)))
  val rr = All[String, String, Unit]("R", IndexedSeq("N", "P"), Array())
  val ss = All[String, String, Unit]
              ("S", IndexedSeq("N", "P", "Q"), Array((1,2)))
  val tt = All[String, String, Unit](
    "T",
    IndexedSeq("A", "B", "C", "D"),
    Array((0, 2), (0, 3), (1, 2), (1, 3)))

  val nInitial = OneItem(nn, false)
  val nComplete = OneItem(nn, true)

  val rInitial = AllItem(rr, Set(0, 1), Set())
  val rThenN = AllItem(rr, Set(1), Set(0))
  val rThenP = AllItem(rr, Set(0), Set(1))
  val rComplete = AllItem(rr, Set(), Set(0, 1))

  val mInitial = AllItem(mm, Set(0))

  val aBefore = ActItem(aa, false)
  val aAfter = ActItem(aa, true)

  val sInitial = AllItem(ss, Set(0, 1), Set())
  val sAfterN = AllItem(ss, Set(1), Set(0))
  val sAfterP = AllItem(ss, Set(0, 2), Set(1))
  val sAfterNP = AllItem(ss, Set(2), Set(0, 1))
  val sAfterPS = AllItem(ss, Set(0), Set(1, 2))
  val sComplete = AllItem(ss, Set(), Set(0, 1, 2))

  val tInitial = AllItem(tt, Set(0, 1))
  val tAfterA = AllItem(tt, Set(1), Set(0))
  val tAfterB = AllItem(tt, Set(0), Set(1))
  val tAfterAB = AllItem(tt, Set(2, 3), Set(0, 1))
  val tAfterABC = AllItem(tt, Set(3), Set(0, 1, 2))
  val tAfterABD = AllItem(tt, Set(2), Set(0, 1, 3))
  val tAfterABCD = AllItem(tt, Set(), Set(0, 1, 2, 3))

  /**
    * Was `'a0` in the old Lisp `yr/specs.lisp` file.
    */
  val lib0 = HTNLib(
    Set(
      One('M', IndexedSeq('A', 'X'), Seq(0.5, 0.6)),
      All('X', IndexedSeq('A', 'B', 'C'),
        Array((0, 1), (0, 2))),
      Act('A', 'a'),
      Act('B', 'b'),
      Act('C', 'c')),
    Seq('M'),
    Seq(1.0)
  )
}
