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
  import org.maraist.planrec.rules.{One,All,Act,HTNLib}
  // import org.maraist.planrec.yr.table.{Item, AllItem, OneItem, ActItem}
  // import org.maraist.planrec.yr.table.Item.{all, one, act}
  import org.maraist.planrec.terms.Term.{StringAsTerm,CharAsTerm}

  val aa = Act[String, String, Unit]("A", "a")
  val bb = Act[String, String, Unit]("B", "b")
  val cc = Act[String, String, Unit]("C", "c")
  val nn = One[String, String, Unit]("N", Seq("A", "B"), Seq(0.4, 0.6))
  val pp = One[String, String, Unit]("P", Seq("A", "C"), Seq(0.3, 0.7))
  val mm = All[String, String, Unit]("M", IndexedSeq("N", "P"), Array[(Int,Int)]((0,1)))
  val rr = All[String, String, Unit]("R", IndexedSeq("N", "P"), Array[(Int,Int)]())

  // val rInitial = AllItem(rr, Set(0, 1))
  // val rThenN = AllItem(rr, Set(1))
  // val rThenP = AllItem(rr, Set(0))
  // val rComplete = AllItem(rr, Set())
  //
  // val mInitial = AllItem(mm, Set(0))
  //
  // val nInitial = OneItem(nn, false)
  //
  // val aBefore = ActItem(aa, false)
  // val aAfter = ActItem(aa, true)

  /**
    * Was `'a0` in the old Lisp `yr/specs.lisp` file.
    */
  val lib0 = HTNLib(
    Set(
      One('M', Seq('A', 'X'), Seq(0.5, 0.6)),
      All('X', IndexedSeq('A', 'B', 'C'),
        Array((0,1), (0,2))),
      Act('A', 'a'),
      Act('B', 'b'),
      Act('C', 'c')),
    Seq('M'),
    Seq(1.0)
  )

}

