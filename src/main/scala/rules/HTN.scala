// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.rules
import org.maraist.latex.{LaTeXdoc,LaTeXRenderable,LaTeXRenderer}
import org.maraist.planrec.terms.Term.*
import org.maraist.util.Collections.normalizeSeq

/** Common trait of all rules. */
trait RuleForm[T, H, S](using TermImpl[T,H,S])
    extends LaTeXRenderable {
  def goal: T
  def unblockedSubgoals: Set[T]
  def apply(subst: S): RuleForm[T, H, S]
}

/** An "and-rule", indicating that a goal can be fulfilled by meeting
  * all of the given subgoals.
  */
case class All[T, H, S](
  val goal: T, val subgoals: IndexedSeq[T], val order: Array[(Int,Int)])
  (using termImpl: TermImpl[T, H, S])
  (using termRender: LaTeXRenderer[T])
    extends RuleForm[T, H, S] {
  // Check for well-formedness of order constraints when creating a rule
  for ((before, after) <- order) {
    if before == after
    then throw IllegalArgumentException
               ("One index may not be ordered against itself")
    else if before > after
    then throw IllegalArgumentException
               ("Index order must respect subgoal array order")
    else if after >= subgoals.length || before < 0
    then throw IllegalArgumentException
               ("Ordered indices must refer to subgoal index")
  }

  def apply(s: S): All[T, H, S] =
    All(goal.subst(s), subgoals.map(_.subst(s)), order)

  def unblockedSubgoals: Set[T] = unblockedSubgoalIndices.map(subgoals(_))

  def unblockedSubgoalIndices: Set[Int] = {
    val blocked = {
      val blockedBuilder = Set.newBuilder[Int]
      for ((_, after) <- order) { blockedBuilder += after }
      blockedBuilder.result()
    }

    val res = Set.newBuilder[Int]
    for (i <- 0 until subgoals.size; if !(blocked.contains(i))) {
      res += i
    }
    res.result()
  }

  def toLaTeX(doc: LaTeXdoc):
      Unit = {
    termRender.toLaTeX(doc, goal)
    var sep = " & ::= "
    for(subgoal <- subgoals) {
      doc ++= sep
      termRender.toLaTeX(doc, subgoal)
      sep = "\\,"
    }

    sep = " ($"
    var fin = ""
    for (pair <- order) {
      pair match {
        case (a,b) => {
          doc ++= s"$sep$a < $b"
        }
      }
      sep = ", "
      fin = "$)"
    }
    doc ++= fin
  }
}

/**
  * Shortcut for [[All]] which builds and uses the list of all pairs
  * for a total order among the subgoals.
  */
object FullAll {
  def apply[T, H, S](goal: T, subgoals: IndexedSeq[T])
    (using termImpl: TermImpl[T, H, S])
    (using termRender: LaTeXRenderer[T]):
      All[T, H, S] = {
    val pairs = Array.newBuilder[(Int,Int)]
    for(i <- 1 to subgoals.length - 1) {
      pairs += ((i - 1, i))
    }
    All(goal, subgoals, pairs.result())
  }
}

/**
  * Shortcut for [[All]] which where the ordering list is empty, and
  * so the subgoals are unordered.
  */
object UnordAll {
  def apply[T, H, S](goal: T, subgoals: IndexedSeq[T])
    (using termImpl: TermImpl[T, H, S])
    (using termRender: LaTeXRenderer[T]):
      All[T, H, S] = All(goal, subgoals, Array())
}

/** An "or-rule", indicating that a goal can be fulfilled by meeting
  * any one of the given subgoals.
  *
  * @tparam T Type of terms mentioned in this rule.
  * @param goal The goal term of this rule.
  * @param subgoals The subgoals required to satisfy the goal term.
  * @param rawProb The probabilities associated with the subterms.
  * This sequence must be of the same length as the sequence of
  * subgoals.
  */
case class One[T, H, S](val goal: T, val subgoals: Seq[T], subgoalProbs: Seq[Double])
  (using TermImpl[T, H, S])(using termRender: LaTeXRenderer[T])
    extends RuleForm[T, H, S] {

  if subgoals.length != subgoalProbs.length
  then throw new IllegalArgumentException(
    "Must have the same number of subgoals and subgoal probabilities")

  /**
    * Normalized probabilities of each subgoal.
    */
  val probs: Seq[Double] = normalizeSeq(subgoalProbs)

  def apply(s: S): One[T, H, S] =
    One(goal.subst(s), subgoals.map(_.subst(s)), subgoalProbs)

  def unblockedSubgoals: Set[T] = Set.from(subgoals)

  def toLaTeX(doc: LaTeXdoc):
      Unit = {
    termRender.toLaTeX(doc, goal)
    var sep = " & ::= "
    for(subgoal <- subgoals) {
      doc ++= sep
      termRender.toLaTeX(doc, subgoal)
      sep = " $|$ "
    }
  }
}

/** A "terminal rule," indicating that a goal corresponds to a simple
  * action.
  */
case class Act[T, H, S](val goal: T, val action: T)
  (using TermImpl[T, H, S])(using termRender: LaTeXRenderer[T])
    extends RuleForm[T, H, S] {
  def unblockedSubgoals: Set[T] = Set.empty

  def apply(s: S): Act[T, H, S] = Act(goal.subst(s), action.subst(s))

  def toLaTeX(doc: LaTeXdoc): Unit = {
    termRender.toLaTeX(doc, goal)
    doc ++= " & ::= "
    termRender.toLaTeX(doc, action)
  }
}

object HTN {
  export org.maraist.planrec.rules.All
  export org.maraist.planrec.rules.One
  export org.maraist.planrec.rules.Act
  type HTNrule[T,H,S] = All[T,H,S] | One[T,H,S] | Act[T,H,S]
}

// -----------------------------------------------------------------

import HTN.*
import org.maraist.planrec.PlanLibrary

class HTNLib[T, H, S]
  (val rules: Set[HTNrule[T, H, S]], val top: Seq[H], topProbs: Seq[Double])
  (using TermImpl[T, H, S])
  (using termRender: LaTeXRenderer[T])
    extends PlanLibrary[HTNrule, T, H, S]
    with LaTeXRenderable {

  if top.length != topProbs.length
  then throw new IllegalArgumentException(
    "Must have the same number of top-level goals and top-level goal "
      + "probabilities")

  def rules(h: H): Set[HTNrule[T, H, S]] =
    for(r <- rules; if r.goal.termHead == h) yield r

  /**
    * Normalized probabilities of each subgoal.
    */
  val probs: Seq[Double] = normalizeSeq(topProbs)

  def heads: Set[H] = {
    val builder = Set.newBuilder[H]
    for (rule <- rules) do rule match {
      case All(goal, subgoals, _) => {
        builder += goal.termHead
        for (subgoal <- subgoals) do builder += subgoal.termHead
      }
      case One(goal, subgoals, _) => {
        for (subgoal <- subgoals) do builder += subgoal.termHead
      }
      case Act(goal, action) => {
        builder += goal.termHead
        builder += action.termHead
      }
    }
    builder.result()
  }

  def toLaTeX(doc: LaTeXdoc): Unit = {
    doc ++= "\\begin{tabular}{r@{~}l}\n"
    for(rule <- rules) {
      rule.toLaTeX(doc)
      doc ++= "\\\\\n"
    }
    doc ++= "\\end{tabular}\n"
  }
}
