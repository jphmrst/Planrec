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

object HTNs {
  import org.maraist.planrec.rules.{One,All,FullAll,Act,HTNLib}
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
    * Example with a shift-reduce conflict (when taken as a CFG).  Was
    * `'a0` in the old Lisp `yr/examples/specs.lisp` file.
    */
  val a0 = Sample(
    HTNLib(
      Set(
        One('M', Seq('A', 'X'), Seq(0.4, 0.6)),
        All('X', IndexedSeq('A', 'B', 'C'), Array((0,1), (0,2))),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c')
      ),
      Seq('M'),
      Seq(1.0)
    ),
    "Taken as a CFG, has a shift-reduce conflict",
    Seq(
      Seq('a', 'b', 'c')
    )
  )

  /**
    * Was `'a1` in the old Lisp `yr/examples/specs.lisp` file.
    */
  val a1 = Sample(
    HTNLib(
      Set(
        One("M", Seq("X", "Y"), Seq(0.4, 0.6)),
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
    "reduce-reduce conflict",
    Seq(
      Seq("a", "b")
    )
  )

  val a2 = Sample(
    HTNLib(
      Set(
        All('M', IndexedSeq('N', 'C'), Array((0,1))),
        One('N', Seq('A', 'X'), Seq(0.4, 0.6)),
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
    )
  )

  val a3 = Sample(
    HTNLib(
      Set(
        One('S', Seq('L', 'M', 'N'), Seq(0.2, 0.3, 0.5)),
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
    "For basic probability/picks debugging.",
    Seq(
      Seq('a', 'b'),
      Seq('a', 'b', 'a', 'b')
    )
  )

  val a4 = Sample(
    HTNLib(
      Set(
        FullAll('S', IndexedSeq('M', 'N')),
        One('M', Seq('A', 'C', 'D'), Seq(0.2, 0.3, 0.5)),
        One('N', Seq('B', 'C'), Seq(0.6, 0.4)),
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
    )
  )

  val a5 = Sample(
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
    "Two-level fully-ordered and-rules.  Originally intended for ELEXIR testing, although we've dropped that.",
    Seq(
      Seq('a', 'b', 'c', 'd', 'e', 'f')
    )
  )

  val a6 = Sample(
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
    "Balanced two-level fully-ordered and-rules.  Originally intended for ELEXIR testing, although we've dropped that.",
    Seq(
      Seq('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')
    )
  )

  val ap0 = Sample(
    HTNLib(
      Set(
        FullAll('M', IndexedSeq('L', 'A')),
        FullAll('N', IndexedSeq('B', 'C')),
        One('L', Seq('B', 'D'), Seq(0.4, 0.6)),
        Act('A', 'a'),
        Act('B', 'b'),
        Act('C', 'c'),
        Act('D', 'd')
      ),
      Seq('M', 'N'),
      Seq(0.3, 0.7)
    ),
    "Could be hard to get the disjunction probabilities right.",
    Seq(
      Seq('b', 'a'),
      Seq('b')
    )
  )

  // Gives Dotty core dump when compiling
  //
  // val ap1 = Sample(
  //   HTNLib(
  //     Set(
  //       FullAll('L', IndexedSeq('A', 'M', 'C')),
  //       One('M', Seq('N', 'B'), Seq(0.4, 0.6)),
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
    "Simple multi-goal",
    Seq(
      Seq('a', 'c'),
      Seq('a', 'b'),
      Seq('a')
    )
  )

  val a = Sample(
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
    "This library shows a very simple, top-level case of allowing subgoal actions to interleave.",
    Seq(
      Seq('a', 'b', 'c', 'd'),
      Seq('a', 'c', 'b', 'd'),
      Seq('a', 'a', 'a', 'b')
    ),
    essay = "State 0 corresponds to the two-insertion point item $S(M N)$; upon entering State 0 the machine should spawn concurrent stacks capped with States 1 and 2 generated from items $M (A)$ and $N (C)$ respectively. State 0 and its successors are controller states. State 1 heads a sequence of states corresponding to single-intersection point items recognizing first an $A$ and then a $B$, and similarly from State 2 for $N (C)$."
  )

  /*  Examples from Lisp code `yr/examples/specs.lisp`.

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'b1
    (list (ord-and-rule +symterm-htn+ 'U' () 'M', 'P')
          (ord-and-rule +symterm-htn+ 'M', '(('A', 'B')) 'A', 'B')
          (ord-and-rule +symterm-htn+ 'P', '(('A', 'C')) 'A', 'C')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c'))
  :long-blurb "This library has interleaving subgoals which share their initial lower-level goal $A$."
  :inputs '((('a', 'a', 'b', 'c') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'b2
    (list (ord-and-rule +symterm-htn+ 'R', '(('D', 'S')) 'D', 'S')
          (ord-and-rule +symterm-htn+ 'S' () 'M', 'N')
          (ord-and-rule +symterm-htn+ 'M', '(('A', 'B')) 'A', 'B')
          (ord-and-rule +symterm-htn+ 'N', '(('C', 'D')) 'C', 'D')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :long-blurb "Table construction for this library encounters a multiple-insertion point item in the closure of an item set."
  :inputs '((('d', 'a', 'c', 'b', 'd') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'b3
    (list (ord-and-rule +symterm-htn+ 'H', '(('A', 'C')) 'A', 'B', 'C')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c'))
  :long-blurb "In this library, completing one of two interleavable subgoals triggers a third."
  :inputs '((('a', 'b', 'c') :each-action t)
            (('a', 'a', 'b', 'c') :each-action t)
            (('b', 'a', 'c') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'b4
    (list (ord-and-rule +symterm-htn+ 'L', '(('A', 'C') ('A', 'D')
                                             ('B', 'C') ('B', 'D'))
                        'A', 'B', 'C', 'D')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :long-blurb "In this library, completing two of four interleavable subgoals triggers the other two."
  :inputs '((('a', 'b', 'c', 'd') :each-action t)
            (('a', 'a', 'b', 'c', 'd') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'b5
    (list (ord-and-rule +symterm-htn+ 'L', '(('A', 'B') ('A', 'C'))
                        'A', 'B', 'C')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c'))
  :long-blurb "In this library, completing one subgoal enables two interleavable ones."
  :inputs '((('a', 'b', 'c') :each-action t)
            (('a', 'a', 'b', 'c') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'b6
    (list (ord-and-rule +symterm-htn+ 'S' () 'M', 'N')
          (ord-and-rule +symterm-htn+ 'M', '(('A', 'B')) 'A', 'B')
          (ord-and-rule +symterm-htn+ 'N', '(('A', 'B')) 'A', 'B')
          Act('A', 'a')
          Act('B', 'b'))
  :short-blurb "duplicated concurrent goals"
  :inputs '((('a', 'b') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'b7
    (list (unord-and-rule +symterm-htn+ 'M', 'N', 'D')
          One('N', 'P', 'Q')
          FullAll('P', IndexedSeq('A'))
          FullAll('Q', IndexedSeq('A', 'B', 'C'))
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :inputs '((('a', 'b', 'c', 'd') :each-action t)
            (('d', 'a', 'b', 'c') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'b8
    (list (unord-and-rule +symterm-htn+ 'L', 'M', 'A')
          (unord-and-rule +symterm-htn+ 'M', 'B', 'C')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c'))
  :inputs '((('a', 'b', 'c') :each-action t)
            (('c', 'b', 'a') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'b9
    (list (unord-and-rule +symterm-htn+ 'L', 'M', 'A')
          (unord-and-rule +symterm-htn+ 'M', 'N', 'B')
          (unord-and-rule +symterm-htn+ 'N', 'C', 'D')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :inputs '((('a', 'b', 'c', 'd') :each-action t)
            (('d', 'c', 'b', 'a') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'b10
    (list FullAll('L', IndexedSeq('A', 'M'))
          (unord-and-rule +symterm-htn+ 'M', 'B', 'N')
          (unord-and-rule +symterm-htn+ 'N')
          Act('A', 'a')
          Act('B', 'b'))
  :inputs '((('a', 'b') :each-action t))
  :short-blurb "a concurrent epsilon rule")

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'bp0
    (list (ord-and-rule 'S', IndexedSeq('(('A', 'M') ('B', 'M'))
                        'A', 'B', 'M')
          (unord-and-rule +symterm-htn+ 'M', 'C', 'D')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :inputs '((('a', 'b', 'c', 'd') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'bp1
    (list (ord-and-rule 'S', IndexedSeq('(('A', 'B') ('B', 'C') ('B', 'D'))
                        'A', 'B', 'C', 'D')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :inputs '((('a', 'b', 'c', 'd') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'c0
    (list (or-rule 'S', IndexedSeq('F', 'G')
          (ord-and-rule +symterm-htn+ 'F', '(('C', 'B')) 'B', 'C')
          (ord-and-rule +symterm-htn+ 'G' nil 'C', 'D')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :inputs '((('c', 'd', 'b') :each-action t)
            (('c', 'b') :each-action t)
            (('c', 'd') :each-action t))
  :short-blurb "simplification of C1")

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'c1
    (list (or-rule 'S', IndexedSeq('F', 'G')
          (ord-and-rule +symterm-htn+ 'F', '(('A', 'C') ('C', 'B'))
                        'A', 'B', 'C')
          (ord-and-rule +symterm-htn+ 'G', '(('A', 'C') ('A', 'D'))
                        'A', 'C', 'D')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :short-blurb "variation of B8."
  :inputs '((('a', 'd') :each-action t)
            (('a', 'c', 'b') :each-action t)
            (('a', 'd', 'c') :each-action t)
            (('a', 'c', 'd') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'c2
    (list (ord-and-rule +symterm-htn+ 'L', '(('D', 'P')) 'D', 'P')
          One('P', 'Q', 'S')
          (ord-and-rule +symterm-htn+ 'Q', '(('A', 'D')) 'A', 'D')
          (ord-and-rule +symterm-htn+ 'S' () 'M', 'N')
          (ord-and-rule +symterm-htn+ 'M', '(('A', 'B')) 'A', 'B')
          (ord-and-rule +symterm-htn+ 'N', '(('C', 'D')) 'C', 'D')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :short-blurb "combining several features"
  :inputs '((('d', 'a', 'd') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'c3
    (list (or-rule 'S', IndexedSeq('F', 'G')
          (ord-and-rule +symterm-htn+ 'F', '(('A', 'B') ('A', 'C'))
                        'A', 'B', 'C')
          (ord-and-rule +symterm-htn+ 'G', '(('A', 'C') ('A', 'D'))
                        'A', 'C', 'D')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :long-blurb "Shift leads to a state that could have an empty base set. This case shows why the \\texttt{itemSet} and \texttt{baseSet} must be distinct.
"
  :inputs '((('a', 'c', 'b') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'c4
    (list One('P', 'Q', 'S')
          (ord-and-rule +symterm-htn+ 'Q', '(('A', 'D')) 'A', 'D')
          (ord-and-rule +symterm-htn+ 'S' () 'M', 'N')
          (ord-and-rule +symterm-htn+ 'M', '(('A', 'B')) 'A', 'B')
          (ord-and-rule +symterm-htn+ 'N', '(('C', 'D')) 'C', 'D')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :long-blurb "In this library the top-level intention is a disjunction of a subgoal with interleaving, and one without."
  :inputs '((('a', 'd') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'c5
    (list (ord-and-rule +symterm-htn+ 'X' () 'D', 'Y')
          One('Y', 'Z', 'K', 'F')
          (ord-and-rule +symterm-htn+ 'Z' () 'J', 'C')
          (ord-and-rule +symterm-htn+ 'K', '(('A', 'B')) 'A', 'B')
          (ord-and-rule +symterm-htn+ 'F' () 'A', 'B', 'D')
          (ord-and-rule +symterm-htn+ 'J', '(('B', 'A')) 'B', 'A')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :long-blurb "Here the disjunction of both interleaving and non-interleaving subgoals arises in the closure of an item set.  Closure leads to disjunction of heterogeneous content."
  :inputs '((('d', 'b', 'a', 'c') :each-action t)
            (('d', 'c') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'c6
    (list (or-rule 'S', IndexedSeq('L', 'M', 'N')
          (ord-and-rule +symterm-htn+ 'L', '(('A', 'B')) 'A', 'B')
          (ord-and-rule +symterm-htn+ 'M', '(('C', 'D')) 'C', 'D')
          (ord-and-rule +symterm-htn+ 'N', '() 'F', 'G')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd')
          Act('F', 'f')
          Act('G', 'g'))
  :inputs '((('a', 'c') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'c7
    (list FullAll('M', IndexedSeq('N', 'D'))
          One('N', 'P', 'Q')
          (unord-and-rule +symterm-htn+ 'P', 'A')
          (unord-and-rule +symterm-htn+ 'Q', 'A', 'B', 'C')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :inputs '((('a', 'b', 'c', 'd') :each-action t)
            (('a') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'c8
    (list (unord-and-rule +symterm-htn+ 'X', 'C', 'Y')
          One('Y', 'Z', 'F')
          FullAll('Z', IndexedSeq('B', 'A'))
          (unord-and-rule +symterm-htn+ 'F', 'A', 'B')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c'))
  :long-blurb "Simplification of C5 to isolate a bug."
  :inputs '((('b', 'a') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'cp0
    (list (ord-and-rule +symterm-htn+ 'F', '(('A', 'B') ('A', 'C'))
                        'A', 'B', 'C')
          (ord-and-rule +symterm-htn+ 'G', '(('A', 'C') ('A', 'D'))
                        'A', 'C', 'D')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :intended '('F', 'G')
  :long-blurb "Variation of B8 with no $K$, and both $F$ and $G$ intended."
  :inputs '((('a', 'c', 'b') :each-action t)
            (('a', 'c') :each-action t)
            (('a') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'cp1
    (list (ord-and-rule +symterm-htn+ 'F', '() 'B', 'C')
          (ord-and-rule +symterm-htn+ 'G', '() 'C', 'D')
          ;; Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :intended '('F', 'G')
  :long-blurb "Variation of B8 with the split at the very top level."
  :inputs '((('c', 'b') :each-action t)
            (('c') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'cp2
    (list (ord-and-rule +symterm-htn+ 'F', '() 'B', 'C')
          (ord-and-rule +symterm-htn+ 'G', '() 'C', 'D')
          (ord-and-rule +symterm-htn+ 'H', '(('A', 'B')) 'A', 'B')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :intended '('F', 'G', 'H')
  :long-blurb "Variation of B8 comparing split and alt."
  :inputs '((('a', 'b') :each-action t)
            (('c') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'cp3
    (list One('M', 'P', 'Q')
          (ord-and-rule +symterm-htn+ 'N', '(('A', 'C') ('B', 'C'))
                        'A', 'B', 'C')
          (ord-and-rule +symterm-htn+ 'P', '(('A', 'D')) 'A', 'D')
          (ord-and-rule +symterm-htn+ 'Q' nil 'B', 'C')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :intended '('M', 'N')
  :inputs '((('c') :each-action t)
            (('a', 'b') :each-action t)
            (('a', 'd') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'cp4
    (list (ord-and-rule +symterm-htn+ 'F', '(('C', 'B')) 'B', 'C')
          (unord-and-rule +symterm-htn+ 'G', 'C', 'D')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd'))
  :intended '('F', 'G')
  :inputs '((('c') :each-action t)
            (('c', 'b') :each-action t)
            (('c', 'd', 'b') :each-action t))
  :long-blurb "C0 without the single top-level goal.")

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'cp5
    (list FullAll('M', IndexedSeq('A', 'B', 'C'))
          (ord-and-rule +symterm-htn+ 'N', '(('A', 'B') ('B', 'D') ('B', 'E'))
                        'A', 'B', 'D', 'E')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd')
          Act('E', 'e'))
  :intended '('M', 'N')
  :inputs '((('a', 'b', 'c') :each-action t)
            (('a', 'b', 'd', 'e') :each-action t)
            (('a') :each-action t)))

  val a = Sample(
    HTNLib(
      Set(
      ),
      Seq(),
      Seq()
    ),
    "",
    Seq(
    )
  )

(make-planlib-spec 'cp6
    (list FullAll('M', IndexedSeq('A', 'B', 'C'))
          (ord-and-rule +symterm-htn+ 'N', '(('A', 'B')
                                             ('B', 'D') ('B', 'E')
                                             ('D', 'F') ('D', 'G')
                                             ('E', 'F') ('E', 'G'))
                        'A', 'B', 'D', 'E', 'F', 'G')
          Act('A', 'a')
          Act('B', 'b')
          Act('C', 'c')
          Act('D', 'd')
          Act('E', 'e')
          Act('F', 'f')
          Act('G', 'g'))
  :intended '('M', 'N')
  :inputs '((('a', 'b', 'c') :each-action t)
            (('a', 'b', 'd', 'e', 'f', 'g') :each-action t)
            (('a', 'b', 'e', 'd', 'f', 'g') :each-action t)
            (('a') :each-action t)))

   */


}

