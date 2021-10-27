// Copyright (C) 2021 John Maraist
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.samples
import scala.collection.immutable.{Seq,IndexedSeq}
import org.maraist.planrec.terms.Term.
  {RenderCharAsTerm, RenderStringAsTerm}

object HTNs {
  import org.maraist.planrec.rules.{
    One,All,FullAll,UnordAll,Act,HTNLib}
  // import org.maraist.planrec.yr.table.{Item, AllItem, OneItem, ActItem}
  // import org.maraist.planrec.yr.table.Item.{all, one, act}
  import org.maraist.planrec.terms.Term.{StringAsTerm,CharAsTerm}

  def load: Unit = ()

  val aa = Act[String, String, Unit]("A", "a")
  val bb = Act[String, String, Unit]("B", "b")
  val cc = Act[String, String, Unit]("C", "c")
  val nn = One[String, String, Unit]("N", IndexedSeq("A", "B"), Seq(0.4, 0.6))
  val pp = One[String, String, Unit]("P", IndexedSeq("A", "C"), Seq(0.3, 0.7))
  val mm = All[String, String, Unit](
    "M", IndexedSeq("N", "P"), Array[(Int,Int)]((0,1)))
  val rr = All[String, String, Unit](
    "R", IndexedSeq("N", "P"), Array[(Int,Int)]())

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
    * Example with a shift-reduce conflict (when taken as a CFG).  Was
    * `'a0` in the old Lisp `yr/examples/specs.lisp` file.
    */
  val a0 = Sample(
    "a0",
    HTNLib(
      Set(
        One('M', IndexedSeq('A', 'X'), Seq(0.4, 0.6)),
        All('X', IndexedSeq('A', 'B', 'C'), Array((0,1), (0,2))),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c')
      ),
      Seq('M'),
      Seq(1.0)
    ),
    "shift-reduce",
    Seq(
      Seq('a', 'b', 'c')
    ),
    "Taken as a CFG, has a shift-reduce conflict",
    dw = "6.5in"
  )

  /**
    * Was `'a1` in the old Lisp `yr/examples/specs.lisp` file.
    */
  val a1 = Sample(
    "a1",
    HTNLib(
      Set(
        One("M", IndexedSeq("X", "Y"), Seq(0.4, 0.6)),
        All("X", IndexedSeq("A1", "B"), Array((0,1))),
        All("Y", IndexedSeq("A2", "C"), Array((0,1))),
        Act("A1", "a1"),
        Act("A2", "a2"),
        Act("B", "b"),
        Act("C", "c"),
      ),
      Seq("M"),
      Seq(1.0)
    ),
    "reduce-reduce",
    Seq(
      Seq("a", "b")
    ),
    "As CFG, shows a reduce-reduce conflict",
    nw = "7.75in",
    dw = "5in"
  )

  val a2 = Sample(
    "a2",
    HTNLib(
      Set(
        All('M', IndexedSeq('N', 'C'), Array((0,1))),
        One('N', IndexedSeq('A', 'X'), Seq(0.4, 0.6)),
        All('X', IndexedSeq('A', 'B'), Array((0,1))),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c')
      ),
      Seq('M'),
      Seq(1.0)
    ),
    "As CFG, shows a shift-reduce conflict.",
    Seq(
      Seq('a', 'b', 'c')
    ),
    nw = "7.75in",
    dw = "5in"
  )

  val a3 = Sample(
    "a3",
    HTNLib(
      Set(
        One('S', IndexedSeq('L', 'M', 'N'), Seq(0.2, 0.3, 0.5)),
        FullAll('L', IndexedSeq('A', 'B', 'D')),
        FullAll('M', IndexedSeq('A', 'C', 'D')),
        FullAll('N', IndexedSeq('B', 'C')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('S'),
      Seq(1.0)
    ),
    "For basic probability/picks debugging",
    Seq(
      Seq('a', 'b'),
      Seq('a', 'b', 'a', 'b')
    ),
    nw = "7.75in",
    dw = "5.5in"
  )

  val a4 = Sample(
    "a4",
    HTNLib(
      Set(
        FullAll('S', IndexedSeq('M', 'N')),
        One('M', IndexedSeq('A', 'C', 'D'), Seq(0.2, 0.3, 0.5)),
        One('N', IndexedSeq('B', 'C'), Seq(0.6, 0.4)),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('S'),
      Seq(1.0)
    ),
    "For basic probability/picks debugging",
    Seq(
      Seq('a', 'b'),
      Seq('a', 'b', 'a', 'b')
    ),
    nw = "7.75in",
    dw = "5.5in"
  )

  val a5 = Sample(
    "a5",
    HTNLib(
      Set(
        FullAll('M', IndexedSeq('A', 'B', 'C', 'N')),
        FullAll('N', IndexedSeq('D', 'E', 'F')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd'),
        Act('E', 'e'),
        Act('F', 'f')
      ),
      Seq('M'),
      Seq(1.0)
    ),
    "Two-level fully-ordered and-rules",
    Seq(
      Seq('a', 'b', 'c', 'd', 'e', 'f')
    ),
    nw = "7.75in"
  )

  val a6 = Sample(
    "a6",
    HTNLib(
      Set(
        FullAll('L', IndexedSeq('M', 'N', 'P')),
        FullAll('M', IndexedSeq('A', 'B', 'C')),
        FullAll('N', IndexedSeq('D', 'E', 'F')),
        FullAll('P', IndexedSeq('G', 'H', 'I')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd'),
        Act('E', 'e'),
        Act('F', 'f'),
        Act('G', 'g'),
        Act('H', 'h'),
        Act('I', 'i')
      ),
      Seq('L'),
      Seq(1.0)
    ),
    "balanced two-level fully-ordered and-rules",
    Seq(
      Seq('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')
    )
  )

  val ap0 = Sample(
    "ap0",
    HTNLib(
      Set(
        FullAll('M', IndexedSeq('L', 'A')),
        FullAll('N', IndexedSeq('B', 'C')),
        One('L', IndexedSeq('B', 'D'), Seq(0.4, 0.6)),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('M', 'N'),
      Seq(0.3, 0.7)
    ),
    "",
    Seq(
      Seq('b', 'a'),
      Seq('b')
    ),
    "Could be hard to get the disjunction probabilities right",
    nw = "7.5in",
    dw = "4.5in"
  )

  // val ap1 = Sample(
  //   "ap1",
  //   HTNLib(
  //     Set(
  //       FullAll('L', IndexedSeq('A', 'M', 'C')),
  //       One('M', IndexedSeq('N', 'B'), Seq(0.4, 0.6)),
  //       All('N', Seq()),
  //       Act('A', 'a'),
  //       Act('B', 'b'),
  //       Act('C', 'c')
  //     ),
  //     Seq('L'),
  //     Seq(1.0)
  //   ),
  //   "With an epsilon rule --- may not be possible in new basic YR",
  //   Seq(
  //     Seq('a', 'c'),
  //     Seq('a', 'b', 'c')
  //   )
  // )

  val ap2 = Sample(
    "ap2",
    HTNLib(
      Set(
        FullAll('M', IndexedSeq('A', 'B')),
        FullAll('N', IndexedSeq('A', 'C')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c')
      ),
      Seq('M', 'N'),
      Seq(0.4, 0.6)
    ),
    "simple multi-goal",
    Seq(
      Seq('a', 'c'),
      Seq('a', 'b'),
      Seq('a')
    )
  )

  val b0 = Sample(
    "b0",
    HTNLib(
      Set(
        All('S', IndexedSeq('M', 'N'), Array.empty),
        FullAll('M', IndexedSeq('A', 'B')),
        FullAll('N', IndexedSeq('C', 'D')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('S'),
      Seq(1.0)
    ),
    "simple top-level interleaving",
    Seq(
      Seq('a', 'b', 'c', 'd'),
      Seq('a', 'c', 'b', 'd'),
      Seq('a', 'a', 'a', 'b')
    ),
    "This example shows a very simple, top-level case of allowing subgoal actions to interleave.  State 0 corresponds to the two-insertion point item $S(M N)$; upon entering State 0 the machine should spawn concurrent stacks capped with States 1 and 2 generated from items $M (A)$ and $N (C)$ respectively. State 0 and its successors are controller states. State 1 heads a sequence of states corresponding to single-intersection point items recognizing first an $A$ and then a $B$, and similarly from State 2 for $N (C)$.",
    dw = "4.5in"
  )

  val b1 = Sample(
    "b1",
    HTNLib(
      Set(
        All('U', IndexedSeq('M', 'P'), Array()),
        FullAll('M', IndexedSeq('A', 'B')),
        FullAll('P', IndexedSeq('A', 'C')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c')
      ),
      Seq('U'),
      Seq(1.0)
    ),
    "interleaving subgoals with common prefix",
    Seq(
      Seq('a', 'a', 'b', 'c')
    ),
    "This library has interleaving subgoals which share their initial lower-level goal $A$.",
    nw = "7.5in",
    dw = "4.5in"
  )

  val b2 = Sample(
   "b2",
    HTNLib(
      Set(
        FullAll('R', IndexedSeq('D', 'S')),
        All('S', IndexedSeq('M', 'N'), Array()),
        FullAll('M', IndexedSeq('A', 'B')),
        FullAll('N', IndexedSeq('C', 'D')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq(),
      Seq()
    ),
    "multiple insertion points in item set closure",
    Seq(
      Seq('d', 'a', 'c', 'b', 'd')
    ),
    "Table construction for this library encounters a multiple-insertion point item in the closure of an item set.",
    nw = "7.5in",
    dw = "4in"
  )

// // TODO Removing for now --- raises runtime error
//
//   val b3 = Sample(
//     "b3",
//     HTNLib(
//       Set(
//         All('H', IndexedSeq('A', 'B', 'C'), Array((0,2))),
//         Act('A', 'a'),
//         Act('B', 'b'),
//         Act('C', 'c')
//       ),
//       Seq('H'),
//       Seq(1.0)
//     ),
//     "In this library, completing one of two interleavable subgoals triggers a third.",
//     Seq(
//       Seq('a', 'b', 'c'),
//       Seq('a', 'a', 'b', 'c'),
//       Seq('b', 'a', 'c')
//     )
//   )

  val b4 = Sample(
    "b4",
    HTNLib(
      Set(
        All('L', IndexedSeq('A', 'B', 'C', 'D'),
          Array((0,2), (0,3), (1,2), (1,3))),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('L'),
      Seq(1.0)
    ),
    "triggering additional interleaved goals",
    Seq(
      Seq('a', 'b', 'c', 'd'),
      Seq('a', 'a', 'b', 'c', 'd')
    ),
    "In this library, completing two of four interleavable subgoals triggers the other two."
  )

  val b5 = Sample(
    "b5",
    HTNLib(
      Set(
        All('L', IndexedSeq('A', 'B', 'C'), Array((0,1), (0,2))),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c')
      ),
      Seq('L'),
      Seq(1.0)
    ),
    "triggering additional interleaved goals",
    Seq(
      Seq('a', 'b', 'c'),
      Seq('a', 'c', 'b'),
      Seq('a', 'a', 'b', 'c')
    ),
    "In this library, completing one subgoal enables two interleavable ones."
  )

  val b6 = Sample(
    "b6",
    HTNLib(
      Set(
        All('S', IndexedSeq('M', 'N'), Array()),
        FullAll('M', IndexedSeq('A', 'B')),
        FullAll('N', IndexedSeq('A', 'B')),
        Act('A', 'a'),
        Act('B', 'b')
      ),
      Seq('S'),
      Seq(1.0)
    ),
    "duplicated concurrent goals",
    Seq(
      Seq('a', 'b')
    )
  )

  val b7 = Sample(
    "b7",
    HTNLib(
      Set(
        UnordAll('M', IndexedSeq('N', 'D')),
        One('N', IndexedSeq('P', 'Q'), Seq(0.4, 0.6)),
        FullAll('P', IndexedSeq('A')),
        FullAll('Q', IndexedSeq('A', 'B', 'C')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('M'),
      Seq(1.0)
    ),
    "",
    Seq(
      Seq('a', 'b', 'c', 'd'),
      Seq('d', 'a', 'b', 'c')
    )
  )

  val b8 = Sample(
    "b8",
    HTNLib(
      Set(
        UnordAll('L', IndexedSeq('M', 'A')),
        UnordAll('M', IndexedSeq('B', 'C')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c')
      ),
      Seq('L'),
      Seq(1.0)
    ),
    "",
    Seq(
      Seq('a', 'b', 'c'),
      Seq('c', 'b', 'a')
    ),
    dw = "4in"
  )

  val b9 = Sample(
    "b9",
    HTNLib(
      Set(
        UnordAll('L', IndexedSeq('M', 'A')),
        UnordAll('M', IndexedSeq('N', 'B')),
        UnordAll('N', IndexedSeq('C', 'D')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('L'),
      Seq(1.0)
    ),
    "",
    Seq(
      Seq('a', 'b', 'c', 'd'),
      Seq('d', 'c', 'b', 'a')
    ),
    dw = "4in"
  )

  val b10 = Sample(
    "b10",
    HTNLib(
      Set(
        FullAll('L', IndexedSeq('A', 'M')),
        UnordAll('M', IndexedSeq('B', 'N')),
        UnordAll('N', IndexedSeq()),
        Act('A', 'a'),
        Act('B', 'b')
      ),
      Seq('L'),
      Seq(1.0)
    ),
    "Concurrent epsilon rule",
    Seq(
      Seq('a', 'b')
    )
  )

  val bp0 = Sample(
    "bp0",
    HTNLib(
      Set(
        All('S', IndexedSeq('A', 'B', 'M'), Array((0,2), (1,2))),
        UnordAll('M', IndexedSeq('C', 'D')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('S'),
      Seq(1.0)
    ),
    "",
    Seq(
      Seq('a', 'b', 'c', 'd')
    )
  )

  val bp1 = Sample(
    "bp1",
    HTNLib(
      Set(
        All('S', IndexedSeq('A', 'B', 'C', 'D'),
          Array((0,1), (1,2), (1,3))),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('S'),
      Seq(1.0)
    ),
    "",
    Seq(
   Seq('a', 'b', 'c', 'd'),
    )
  )

  val c0 = Sample(
    "c0",
    HTNLib(
      Set(
        One('S', IndexedSeq('F', 'G'), Seq(0.4, 0.6)),
        FullAll('F', IndexedSeq('C', 'B')),
        UnordAll('G', IndexedSeq('C', 'D')),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('S'),
      Seq(1.0)
    ),
    "Simplification of C1",
    Seq(
      Seq('c', 'd', 'b'),
      Seq('c', 'b'),
      Seq('c', 'd')
    ),
    dw = "4in"
  )

  val c1 = Sample(
    "c1",
    HTNLib(
      Set(
        One('S', IndexedSeq('F', 'G'), IndexedSeq(0.4, 0.6)),
        FullAll('F', IndexedSeq('A', 'C', 'B')),
        All('G', IndexedSeq('A', 'C', 'D'), Array((0,1), (0,2))),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('S'),
      Seq(1.0)
    ),
    "variation of B8",
    Seq(
      Seq('a', 'd'),
      Seq('a', 'c', 'b'),
      Seq('a', 'd', 'c'),
      Seq('a', 'c', 'd')
    )
  )

  val c2 = Sample(
    "c2",
    HTNLib(
      Set(
        FullAll('L', IndexedSeq('D', 'P')),
        One('P', IndexedSeq('Q', 'S'), Seq(0.4, 0.6)),
        FullAll('Q', IndexedSeq('A', 'D')),
        UnordAll('S', IndexedSeq('M', 'N')),
        FullAll('M', IndexedSeq('A', 'B')),
        FullAll('N', IndexedSeq('C', 'D')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('L'),
      Seq(1.0)
    ),
    "combining several features",
    Seq(
      Seq('d', 'a', 'd'),
    )
  )

  val c3 = Sample(
    "c3",
    HTNLib(
      Set(
        One('S', IndexedSeq('F', 'G'), IndexedSeq(0.4, 0.6)),
        All('F', IndexedSeq('A', 'B', 'C'), Array((0,1), (0,2))),
        All('G', IndexedSeq('A', 'C', 'D'), Array((0,1), (0,2))),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('S'),
      Seq(1.0)
    ),
    "shift to an empty base set",
    Seq(
      Seq('a', 'c', 'b'),
    ),
    "Shift leads to a state that could have an empty base set. This case shows why the \\texttt{itemSet} and \texttt{baseSet} must be distinct."
  )

  val c4 = Sample(
    "c4",
    HTNLib(
      Set(
        One('P', IndexedSeq('Q', 'S'), Seq(0.4, 0.6)),
        FullAll('Q', IndexedSeq('A', 'D')),
        UnordAll('S', IndexedSeq('M', 'N')),
        FullAll('M', IndexedSeq('A', 'B')),
        FullAll('N', IndexedSeq('C', 'D')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('P'),
      Seq(1.0)
    ),
    "disjunction of one subgoal with interleaving, one without",
    Seq(
      Seq('a', 'd'),
    ),
    "In this library the top-level intention is a disjunction of a subgoal with interleaving, and one without."
  )

  val c5 = Sample(
    "c5",
    HTNLib(
      Set(
        UnordAll('X', IndexedSeq('D', 'Y')),
        One('Y', IndexedSeq('Z', 'K', 'F'), Seq(0.2, 0.3, 0.5)), // Dotty crash if 3rd arg missing
        UnordAll('Z', IndexedSeq('J', 'C')),
        FullAll('K', IndexedSeq('A', 'B')),
        UnordAll('F', IndexedSeq('A', 'B', 'D')),
        FullAll('J', IndexedSeq('B', 'A')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('X'),
      Seq(1.0)
    ),
    "disjunction of interleaving and non-interleaving subgoals in closure",
    Seq(
      Seq('d', 'b', 'a', 'c'),
      Seq('d', 'c')
    ),
    "Here the disjunction of both interleaving and non-interleaving subgoals arises in the closure of an item set.  Closure leads to disjunction of heterogeneous content."
  )

  val c6 = Sample(
    "c6",
    HTNLib(
      Set(
        One('S', IndexedSeq('L', 'M', 'N'), Seq(0.2, 0.3, 0.5)),
        FullAll('L', IndexedSeq('A', 'B')),
        FullAll('M', IndexedSeq('C', 'D')),
        UnordAll('N', IndexedSeq('F', 'G')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd'),
        Act('F', 'f'),
        Act('G', 'g')
      ),
      Seq('S'),
      Seq(1.0)
    ),
    "",
    Seq(
      Seq('a', 'c')
    )
  )

  val c7 = Sample(
    "c7",
    HTNLib(
      Set(
        FullAll('M', IndexedSeq('N', 'D')),
        One('N', IndexedSeq('P', 'Q'), Seq(0.4, 0.6)),
        UnordAll('P', IndexedSeq('A')),
        UnordAll('Q', IndexedSeq('A', 'B', 'C')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('M'),
      Seq(1.0)
    ),
    "",
    Seq(
      Seq('a', 'b', 'c', 'd'),
      Seq('a')
    )
  )

  val c8 = Sample(
    "c8",
    HTNLib(
      Set(
        UnordAll('X', IndexedSeq('C', 'Y')),
        One('Y', IndexedSeq('Z', 'F'), Seq(0.4, 0.6)),
        FullAll('Z', IndexedSeq('B', 'A')),
        UnordAll('F', IndexedSeq('A', 'B')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c')
      ),
      Seq('X'),
      Seq(1.0)
    ),
    "simplification of C5 to isolate a bug",
    Seq(
      Seq('b', 'a')
    )
  )

  val cp0 = Sample(
    "cp0",
    HTNLib(
      Set(
        All('F', IndexedSeq('A', 'B', 'C'), Array((0,1), (0,2))),
        All('G', IndexedSeq('A', 'C', 'D'), Array((0,1), (0,2))),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('F', 'G'),
      Seq(0.4, 0.6)
    ),
    "variation of B8",
    Seq(
      Seq('a', 'c', 'b'),
      Seq('a', 'c'),
      Seq('a')
    ),
    "Variation of B8 with no $K$, and both $F$ and $G$ intended."
  )

  val cp1 = Sample(
    "cp1",
    HTNLib(
      Set(
        UnordAll('F', IndexedSeq('B', 'C')),
        UnordAll('G', IndexedSeq('C', 'D')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('F', 'G'),
      Seq(0.4, 0.6)
    ),
    "variation of B8",
    Seq(
      Seq('c', 'b'),
      Seq('c')
    ),
    "Variation of B8 with the split at the very top level.",
    nw = "6in",
    dw = "4in"
  )

  val cp2 = Sample(
    "cp2",
    HTNLib(
      Set(
        UnordAll('F', IndexedSeq('B', 'C')),
        UnordAll('G', IndexedSeq('C', 'D')),
        FullAll('H', IndexedSeq('A', 'B')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('F', 'G', 'H'),
      Seq(0.2, 0.3, 0.5)
    ),
    "variation of B8 comparing split and alt",
    Seq(
      Seq('a', 'b'),
      Seq('c')
    ),
    dw = "4in"
  )

  val cp3 = Sample(
    "cp3",
    HTNLib(
      Set(
        One('M', IndexedSeq('P', 'Q'), Seq(0.4, 0.6)),
        All('N', IndexedSeq('A', 'B', 'C'), Array((0,1), (0,2))),
        FullAll('P', IndexedSeq('A', 'D')),
        UnordAll('Q', IndexedSeq('B', 'C')),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('M', 'N'),
      Seq(0.3, 0.7)
    ),
    "",
    Seq(
      Seq('c'),
      Seq('a', 'b'),
      Seq('a', 'd')
    )
  )

  val cp4 = Sample(
    "cp4",
    HTNLib(
      Set(
        FullAll('F', IndexedSeq('B', 'C')),
        UnordAll('G', IndexedSeq('C', 'D')),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('F', 'G'),
      Seq(0.4, 0.6)
    ),
    "C0 without the single top-level goal",
    Seq(
      Seq('c'),
      Seq('c', 'b'),
      Seq('c', 'd', 'b')
    ),
    dw = "4in"
  )

  val cp5 = Sample(
    "cp5",
    HTNLib(
      Set(
        FullAll('M', IndexedSeq('A', 'B', 'C')),
        All('N', IndexedSeq('A', 'B', 'D', 'E'),
          Array((0,1), (1,2), (1,3))),
          Act('A', 'a'),
          Act('B', 'b'),
          Act('C', 'c'),
          Act('D', 'd'),
          Act('E', 'e')
      ),
      Seq('M', 'N'),
      Seq(0.4, 0.6)
    ),
    "",
    Seq(
      Seq('a', 'b', 'c'),
      Seq('a', 'b', 'd', 'e'),
      Seq('a')
    )
  )

  val cp6 = Sample(
    "cp6",
    HTNLib(
      Set(
        FullAll('M', IndexedSeq('A', 'B', 'C')),
        All('N', IndexedSeq('A', 'B', 'D', 'E', 'F', 'G'),
          Array((0,1), (1,2), (1,3), (2,4), (2,5), (3,4), (3,5))),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd'),
        Act('E', 'e'),
        Act('F', 'f'),
        Act('G', 'g')
      ),
      Seq('M', 'N'),
      Seq(0.4, 0.6)
    ),
    "",
    Seq(
      Seq('a', 'b', 'c'),
      Seq('a', 'b', 'd', 'e', 'f', 'g'),
      Seq('a', 'b', 'e', 'd', 'f', 'g'),
      Seq('a')
    )
  )
}
