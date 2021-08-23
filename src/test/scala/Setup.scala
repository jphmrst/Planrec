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
    * Was `'a0` in the old Lisp `yr/examples/specs.lisp` file.
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

  /*  Examples from Lisp code `yr/examples/specs.lisp`.

(make-planlib-spec 'a1
    (list (or-rule +symterm-htn+ '|M| '|X| '|Y|)
          (ord-and-rule +symterm-htn+ '|X| '((|A1| |B|)) '|A1| '|B|)
          (ord-and-rule +symterm-htn+ '|Y| '((|A2| |C|)) '|A2| '|C|)
          (make-term-rule +symterm-htn+ '|A1| '|a|)
          (make-term-rule +symterm-htn+ '|A2| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|))
  :short-blurb "reduce-reduce conflict"
  :inputs '(((|a| |b|) :each-action t)))

(make-planlib-spec 'a2
    (list (ord-and-rule +symterm-htn+ '|M| '((|N| |C|)) '|N| '|C|)
          (or-rule +symterm-htn+ '|N| '|A| '|X|)
          (ord-and-rule +symterm-htn+ '|X| '((|A| |B|)) '|A| '|B|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|))
  :short-blurb "shift-reduce conflict"
  :inputs '(((|a| |b| |c|) :each-action t)))

(make-planlib-spec 'a3
    (list (or-rule +symterm-htn+ '|S| '|L| '|M| '|N|)
          (full-ord-and-rule +symterm-htn+ '|L| '|A| '|B| '|D|)
          (full-ord-and-rule +symterm-htn+ '|M| '|A| '|C| '|D|)
          (full-ord-and-rule +symterm-htn+ '|N| '|B| '|C|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :inputs '(((|a| |b|) :each-action t)))

(make-planlib-spec 'a4
    (list (full-ord-and-rule +symterm-htn+ '|S| '|M| '|N|)
          (or-rule +symterm-htn+ '|M| '|A| '|C| '|D|)
          (or-rule +symterm-htn+ '|N| '|B| '|C|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :short-blurb "basic probability/picks debugging"
  :inputs '(((|a| |b|) :each-action t)
            ((|a| |b| |a| |b|) :each-action t)))

(make-planlib-spec 'a5
    (list (full-ord-and-rule +symterm-htn+ '|M| '|A| '|B| '|C| '|N|)
          (full-ord-and-rule +symterm-htn+ '|N| '|D| '|E| '|F|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|)
          (make-term-rule +symterm-htn+ '|E| '|e|)
          (make-term-rule +symterm-htn+ '|F| '|f|))
  :short-blurb "two-level fully-ordered and-rules for ELEXIR testing"
  :inputs '(((|a| |b| |c| |d| |e| |f|) :each-action t)))

(make-planlib-spec 'a6
    (list (full-ord-and-rule +symterm-htn+ '|L| '|M| '|N| '|P|)
          (full-ord-and-rule +symterm-htn+ '|M| '|A| '|B| '|C|)
          (full-ord-and-rule +symterm-htn+ '|N| '|D| '|E| '|F|)
          (full-ord-and-rule +symterm-htn+ '|P| '|G| '|H| '|I|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|)
          (make-term-rule +symterm-htn+ '|E| '|e|)
          (make-term-rule +symterm-htn+ '|F| '|f|)
          (make-term-rule +symterm-htn+ '|G| '|g|)
          (make-term-rule +symterm-htn+ '|H| '|h|)
          (make-term-rule +symterm-htn+ '|I| '|i|))
  :short-blurb "balanced two-level fully-ordered and-rules for ELEXIR testing"
  :inputs '(((|a| |b| |c| |d| |e| |f| |g| |h| |i|) :each-action t)))

(make-planlib-spec 'ap0
    (list (ord-and-rule +symterm-htn+ '|M| '((|L| |A|)) '|L| '|A|)
          (ord-and-rule +symterm-htn+ '|N| '((|B| |C|)) '|B| '|C|)
          (or-rule +symterm-htn+ '|L| '|B| '|D|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :intended '(|M| |N|)
  :short-blurb "could be hard to get the disjunction probabilities right"
  :inputs '(((|b| |a|) :each-action t)
            ((|b|))))

(make-planlib-spec 'ap1
    (list (full-ord-and-rule +symterm-htn+ '|L| '|A| '|M| '|C|)
          (or-rule +symterm-htn+ '|M| '|N| '|B|)
          (unord-and-rule +symterm-htn+ '|N|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|))
  :short-blurb "with an epsilon rule"
  :inputs '(((|a| |c|) :each-action t)
            ((|a| |b| |c|) :each-action t)))

(make-planlib-spec 'ap2
    (list (full-ord-and-rule +symterm-htn+ '|M| '|A| '|B|)
          (full-ord-and-rule +symterm-htn+ '|N| '|A| '|C|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|))
  :short-blurb "simple multi-goal"
  :intended '(|M| |N|)
  :inputs '(((|a| |c|) :each-action t)
            ((|a| |b|) :each-action t)
            ((|a|) :each-action t)))

(make-planlib-spec 'b0
    (list (ord-and-rule +symterm-htn+ '|S| () '|M| '|N|)
          (ord-and-rule +symterm-htn+ '|M| '((|A| |B|)) '|A| '|B|)
          (ord-and-rule +symterm-htn+ '|N| '((|C| |D|)) '|C| '|D|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :short-blurb "top-level interleaving"
  :long-blurb "This library shows a very simple, top-level case of allowing subgoal actions to interleave."
  :essay "State 0 corresponds to the two-insertion point item $S(M N)$; upon entering State 0 the machine should spawn concurrent stacks capped with States 1 and 2 generated from items $M (A)$ and $N (C)$ respectively. State 0 and its successors are controller states. State 1 heads a sequence of states corresponding to single-intersection point items recognizing first an $A$ and then a $B$, and similarly from State 2 for $N (C)$."
  :inputs '(((|a| |b| |c| |d|) :each-action t)
            ((|a| |c| |b| |d|) :each-action t)
            ((|a| |a| |a| |b|) :each-action t)))

(make-planlib-spec 'b1
    (list (ord-and-rule +symterm-htn+ '|U| () '|M| '|P|)
          (ord-and-rule +symterm-htn+ '|M| '((|A| |B|)) '|A| '|B|)
          (ord-and-rule +symterm-htn+ '|P| '((|A| |C|)) '|A| '|C|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|))
  :long-blurb "This library has interleaving subgoals which share their initial lower-level goal $A$."
  :inputs '(((|a| |a| |b| |c|) :each-action t)))

(make-planlib-spec 'b2
    (list (ord-and-rule +symterm-htn+ '|R| '((|D| |S|)) '|D| '|S|)
          (ord-and-rule +symterm-htn+ '|S| () '|M| '|N|)
          (ord-and-rule +symterm-htn+ '|M| '((|A| |B|)) '|A| '|B|)
          (ord-and-rule +symterm-htn+ '|N| '((|C| |D|)) '|C| '|D|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :long-blurb "Table construction for this library encounters a multiple-insertion point item in the closure of an item set."
  :inputs '(((|d| |a| |c| |b| |d|) :each-action t)))

(make-planlib-spec 'b3
    (list (ord-and-rule +symterm-htn+ '|H| '((|A| |C|)) '|A| '|B| '|C|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|))
  :long-blurb "In this library, completing one of two interleavable subgoals triggers a third."
  :inputs '(((|a| |b| |c|) :each-action t)
            ((|a| |a| |b| |c|) :each-action t)
            ((|b| |a| |c|) :each-action t)))

(make-planlib-spec 'b4
    (list (ord-and-rule +symterm-htn+ '|L| '((|A| |C|) (|A| |D|)
                                             (|B| |C|) (|B| |D|))
                        '|A| '|B| '|C| '|D|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :long-blurb "In this library, completing two of four interleavable subgoals triggers the other two."
  :inputs '(((|a| |b| |c| |d|) :each-action t)
            ((|a| |a| |b| |c| |d|) :each-action t)))

(make-planlib-spec 'b5
    (list (ord-and-rule +symterm-htn+ '|L| '((|A| |B|) (|A| |C|))
                        '|A| '|B| '|C|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|))
  :long-blurb "In this library, completing one subgoal enables two interleavable ones."
  :inputs '(((|a| |b| |c|) :each-action t)
            ((|a| |a| |b| |c|) :each-action t)))

(make-planlib-spec 'b6
    (list (ord-and-rule +symterm-htn+ '|S| () '|M| '|N|)
          (ord-and-rule +symterm-htn+ '|M| '((|A| |B|)) '|A| '|B|)
          (ord-and-rule +symterm-htn+ '|N| '((|A| |B|)) '|A| '|B|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|))
  :short-blurb "duplicated concurrent goals"
  :inputs '(((|a| |b|) :each-action t)))

(make-planlib-spec 'b7
    (list (unord-and-rule +symterm-htn+ '|M| '|N| '|D|)
          (or-rule +symterm-htn+ '|N| '|P| '|Q|)
          (full-ord-and-rule +symterm-htn+ '|P| '|A|)
          (full-ord-and-rule +symterm-htn+ '|Q| '|A| '|B| '|C|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :inputs '(((|a| |b| |c| |d|) :each-action t)
            ((|d| |a| |b| |c|) :each-action t)))

(make-planlib-spec 'b8
    (list (unord-and-rule +symterm-htn+ '|L| '|M| '|A|)
          (unord-and-rule +symterm-htn+ '|M| '|B| '|C|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|))
  :inputs '(((|a| |b| |c|) :each-action t)
            ((|c| |b| |a|) :each-action t)))

(make-planlib-spec 'b9
    (list (unord-and-rule +symterm-htn+ '|L| '|M| '|A|)
          (unord-and-rule +symterm-htn+ '|M| '|N| '|B|)
          (unord-and-rule +symterm-htn+ '|N| '|C| '|D|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :inputs '(((|a| |b| |c| |d|) :each-action t)
            ((|d| |c| |b| |a|) :each-action t)))

(make-planlib-spec 'b10
    (list (full-ord-and-rule +symterm-htn+ '|L| '|A| '|M|)
          (unord-and-rule +symterm-htn+ '|M| '|B| '|N|)
          (unord-and-rule +symterm-htn+ '|N|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|))
  :inputs '(((|a| |b|) :each-action t))
  :short-blurb "a concurrent epsilon rule")

(make-planlib-spec 'bp0
    (list (ord-and-rule +symterm-htn+ '|S| '((|A| |M|) (|B| |M|))
                        '|A| '|B| '|M|)
          (unord-and-rule +symterm-htn+ '|M| '|C| '|D|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :inputs '(((|a| |b| |c| |d|) :each-action t)))

(make-planlib-spec 'bp1
    (list (ord-and-rule +symterm-htn+ '|S| '((|A| |B|) (|B| |C|) (|B| |D|))
                        '|A| '|B| '|C| '|D|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :inputs '(((|a| |b| |c| |d|) :each-action t)))

(make-planlib-spec 'c0
    (list (or-rule +symterm-htn+ '|S| '|F| '|G|)
          (ord-and-rule +symterm-htn+ '|F| '((|C| |B|)) '|B| '|C|)
          (ord-and-rule +symterm-htn+ '|G| nil '|C| '|D|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :inputs '(((|c| |d| |b|) :each-action t)
            ((|c| |b|) :each-action t)
            ((|c| |d|) :each-action t))
  :short-blurb "simplification of C1")

(make-planlib-spec 'c1
    (list (or-rule +symterm-htn+ '|S| '|F| '|G|)
          (ord-and-rule +symterm-htn+ '|F| '((|A| |C|) (|C| |B|))
                        '|A| '|B| '|C|)
          (ord-and-rule +symterm-htn+ '|G| '((|A| |C|) (|A| |D|))
                        '|A| '|C| '|D|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :short-blurb "variation of B8."
  :inputs '(((|a| |d|) :each-action t)
            ((|a| |c| |b|) :each-action t)
            ((|a| |d| |c|) :each-action t)
            ((|a| |c| |d|) :each-action t)))

(make-planlib-spec 'c2
    (list (ord-and-rule +symterm-htn+ '|L| '((|D| |P|)) '|D| '|P|)
          (or-rule +symterm-htn+ '|P| '|Q| '|S|)
          (ord-and-rule +symterm-htn+ '|Q| '((|A| |D|)) '|A| '|D|)
          (ord-and-rule +symterm-htn+ '|S| () '|M| '|N|)
          (ord-and-rule +symterm-htn+ '|M| '((|A| |B|)) '|A| '|B|)
          (ord-and-rule +symterm-htn+ '|N| '((|C| |D|)) '|C| '|D|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :short-blurb "combining several features"
  :inputs '(((|d| |a| |d|) :each-action t)))

(make-planlib-spec 'c3
    (list (or-rule +symterm-htn+ '|S| '|F| '|G|)
          (ord-and-rule +symterm-htn+ '|F| '((|A| |B|) (|A| |C|))
                        '|A| '|B| '|C|)
          (ord-and-rule +symterm-htn+ '|G| '((|A| |C|) (|A| |D|))
                        '|A| '|C| '|D|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :long-blurb "Shift leads to a state that could have an empty base set. This case shows why the \\texttt{itemSet} and \texttt{baseSet} must be distinct.
"
  :inputs '(((|a| |c| |b|) :each-action t)))

(make-planlib-spec 'c4
    (list (or-rule +symterm-htn+ '|P| '|Q| '|S|)
          (ord-and-rule +symterm-htn+ '|Q| '((|A| |D|)) '|A| '|D|)
          (ord-and-rule +symterm-htn+ '|S| () '|M| '|N|)
          (ord-and-rule +symterm-htn+ '|M| '((|A| |B|)) '|A| '|B|)
          (ord-and-rule +symterm-htn+ '|N| '((|C| |D|)) '|C| '|D|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :long-blurb "In this library the top-level intention is a disjunction of a subgoal with interleaving, and one without."
  :inputs '(((|a| |d|) :each-action t)))

(make-planlib-spec 'c5
    (list (ord-and-rule +symterm-htn+ '|X| () '|D| '|Y|)
          (or-rule +symterm-htn+ '|Y| '|Z| '|K| '|F|)
          (ord-and-rule +symterm-htn+ '|Z| () '|J| '|C|)
          (ord-and-rule +symterm-htn+ '|K| '((|A| |B|)) '|A| '|B|)
          (ord-and-rule +symterm-htn+ '|F| () '|A| '|B| '|D|)
          (ord-and-rule +symterm-htn+ '|J| '((|B| |A|)) '|B| '|A|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :long-blurb "Here the disjunction of both interleaving and non-interleaving subgoals arises in the closure of an item set.  Closure leads to disjunction of heterogeneous content."
  :inputs '(((|d| |b| |a| |c|) :each-action t)
            ((|d| |c|) :each-action t)))

(make-planlib-spec 'c6
    (list (or-rule +symterm-htn+ '|S| '|L| '|M| '|N|)
          (ord-and-rule +symterm-htn+ '|L| '((|A| |B|)) '|A| '|B|)
          (ord-and-rule +symterm-htn+ '|M| '((|C| |D|)) '|C| '|D|)
          (ord-and-rule +symterm-htn+ '|N| '() '|F| '|G|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|)
          (make-term-rule +symterm-htn+ '|F| '|f|)
          (make-term-rule +symterm-htn+ '|G| '|g|))
  :inputs '(((|a| |c|) :each-action t)))

(make-planlib-spec 'c7
    (list (full-ord-and-rule +symterm-htn+ '|M| '|N| '|D|)
          (or-rule +symterm-htn+ '|N| '|P| '|Q|)
          (unord-and-rule +symterm-htn+ '|P| '|A|)
          (unord-and-rule +symterm-htn+ '|Q| '|A| '|B| '|C|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :inputs '(((|a| |b| |c| |d|) :each-action t)
            ((|a|) :each-action t)))

(make-planlib-spec 'c8
    (list (unord-and-rule +symterm-htn+ '|X| '|C| '|Y|)
          (or-rule +symterm-htn+ '|Y| '|Z| '|F|)
          (full-ord-and-rule +symterm-htn+ '|Z| '|B| '|A|)
          (unord-and-rule +symterm-htn+ '|F| '|A| '|B|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|))
  :long-blurb "Simplification of C5 to isolate a bug."
  :inputs '(((|b| |a|) :each-action t)))

(make-planlib-spec 'cp0
    (list (ord-and-rule +symterm-htn+ '|F| '((|A| |B|) (|A| |C|))
                        '|A| '|B| '|C|)
          (ord-and-rule +symterm-htn+ '|G| '((|A| |C|) (|A| |D|))
                        '|A| '|C| '|D|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :intended '(|F| |G|)
  :long-blurb "Variation of B8 with no $K$, and both $F$ and $G$ intended."
  :inputs '(((|a| |c| |b|) :each-action t)
            ((|a| |c|) :each-action t)
            ((|a|) :each-action t)))

(make-planlib-spec 'cp1
    (list (ord-and-rule +symterm-htn+ '|F| '() '|B| '|C|)
          (ord-and-rule +symterm-htn+ '|G| '() '|C| '|D|)
          ;; (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :intended '(|F| |G|)
  :long-blurb "Variation of B8 with the split at the very top level."
  :inputs '(((|c| |b|) :each-action t)
            ((|c|) :each-action t)))

(make-planlib-spec 'cp2
    (list (ord-and-rule +symterm-htn+ '|F| '() '|B| '|C|)
          (ord-and-rule +symterm-htn+ '|G| '() '|C| '|D|)
          (ord-and-rule +symterm-htn+ '|H| '((|A| |B|)) '|A| '|B|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :intended '(|F| |G| |H|)
  :long-blurb "Variation of B8 comparing split and alt."
  :inputs '(((|a| |b|) :each-action t)
            ((|c|) :each-action t)))

(make-planlib-spec 'cp3
    (list (or-rule +symterm-htn+ '|M| '|P| '|Q|)
          (ord-and-rule +symterm-htn+ '|N| '((|A| |C|) (|B| |C|))
                        '|A| '|B| '|C|)
          (ord-and-rule +symterm-htn+ '|P| '((|A| |D|)) '|A| '|D|)
          (ord-and-rule +symterm-htn+ '|Q| nil '|B| '|C|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :intended '(|M| |N|)
  :inputs '(((|c|) :each-action t)
            ((|a| |b|) :each-action t)
            ((|a| |d|) :each-action t)))

(make-planlib-spec 'cp4
    (list (ord-and-rule +symterm-htn+ '|F| '((|C| |B|)) '|B| '|C|)
          (unord-and-rule +symterm-htn+ '|G| '|C| '|D|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|))
  :intended '(|F| |G|)
  :inputs '(((|c|) :each-action t)
            ((|c| |b|) :each-action t)
            ((|c| |d| |b|) :each-action t))
  :long-blurb "C0 without the single top-level goal.")

(make-planlib-spec 'cp5
    (list (full-ord-and-rule +symterm-htn+ '|M| '|A| '|B| '|C|)
          (ord-and-rule +symterm-htn+ '|N| '((|A| |B|) (|B| |D|) (|B| |E|))
                        '|A| '|B| '|D| '|E|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|)
          (make-term-rule +symterm-htn+ '|E| '|e|))
  :intended '(|M| |N|)
  :inputs '(((|a| |b| |c|) :each-action t)
            ((|a| |b| |d| |e|) :each-action t)
            ((|a|) :each-action t)))

(make-planlib-spec 'cp6
    (list (full-ord-and-rule +symterm-htn+ '|M| '|A| '|B| '|C|)
          (ord-and-rule +symterm-htn+ '|N| '((|A| |B|)
                                             (|B| |D|) (|B| |E|)
                                             (|D| |F|) (|D| |G|)
                                             (|E| |F|) (|E| |G|))
                        '|A| '|B| '|D| '|E| '|F| '|G|)
          (make-term-rule +symterm-htn+ '|A| '|a|)
          (make-term-rule +symterm-htn+ '|B| '|b|)
          (make-term-rule +symterm-htn+ '|C| '|c|)
          (make-term-rule +symterm-htn+ '|D| '|d|)
          (make-term-rule +symterm-htn+ '|E| '|e|)
          (make-term-rule +symterm-htn+ '|F| '|f|)
          (make-term-rule +symterm-htn+ '|G| '|g|))
  :intended '(|M| |N|)
  :inputs '(((|a| |b| |c|) :each-action t)
            ((|a| |b| |d| |e| |f| |g|) :each-action t)
            ((|a| |b| |e| |d| |f| |g|) :each-action t)
            ((|a|) :each-action t)))

   */


}

