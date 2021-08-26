// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr.table
import scala.collection.mutable.Queue
import org.maraist.fa.NDFABuilder
import org.maraist.planrec.rules.{All,One,Act,TriggerHint,TriggerMatchIndex}
import org.maraist.planrec.rules.HTN.*
import org.maraist.planrec.terms.TermImpl
import TriggerHint.*
import org.maraist.planrec.terms.{>?<, >><<, >+<}
import scala.compiletime.ops.any

// NEXT TODO --- to build the table we use goal heads H, not goal
// terms T.  Need to replace T with H in all of the triggers and
// similar.

/** An item is the combination of a rule and a particular state of
  * progress through that rule.
  * @tparam T Type of terms for goals and actions in rules and items
  * @param impl Item implementation.
  */
sealed trait Item[T, H, S](using impl: TermImpl[T, H, S]) {

  /** HTN [[HTNrule rule]] associated with this item. */
  def rule: HTNrule[T, H, S]

  /** Returns `true` if this item is a final item for its rule. */
  def isFinal: Boolean

  /** The triggers corresponding to subgoals for this item. */
  def goalHints: Set[(T, TriggerHint)]

  /** The triggers corresponding to actions for this item. */
  def actionHints: Set[(T, TriggerHint)]

  /** All triggers for this item. */
  def triggers: Set[T]

  /** Result of advancing this item for the given trigger.
    *
    * Note that Skolemizing, if needed, should be performed *before*
    * calling this method.
    *
    * @param subst The substitution to be applied to the result.
    * @param trigger The action or subgoal used to advance this item.
    * @param hint Directive for resolving the trigger to a particular
    * subgoal of this item's rule.
    */
  def apply(trigger: T)(using hint: TriggerHint):
      Option[Item[T, H, S]]

  /** Hints which might apply to this trigger, if it is ambiguous within
    * the possible subgoals/actions.
    * @param trigger The action or subgoal under question.
    */
  def applications(trigger: T): Seq[TriggerHint]
}

/** Helper methods. */
object Item {
  import scala.util.control.NonLocalReturns.*

  def findMatchingSubgoal[T, H, S]
    (trigger: T, subgoals: IndexedSeq[T], hint: TriggerHint)
    (using impl: TermImpl[T, H, S]):
      Option[(S, Int)] = hint match {
      case TriggerMatchIndex(i) => (subgoals(i) >+< trigger).map((_,i))
      case _ => returning {
        for(i <- 0 until subgoals.length) {
          (subgoals(i) >+< trigger).map(
            (s: S) => throwReturn[Option[(S,Int)]](Some((s,i))))
        }
        None
      }
  }
}

/** Items associated with [[All]] rules.  Corresponds to the
  * [[Item.all]] instance of [[RuleItem]].
  *
  * @param rule Rule associated a particular item instance
  * @param ready The frontier of subgoals which are neither satisfied
  * nor blocked in this item.
  */
case class AllItem[T, H, S](
  val rule: All[T, H, S],
  val ready: Set[Int]
)(
  using TermImpl[T, H, S]
) extends Item[T, H, S] {

  /** In [[All]] rules' items, the item is in the final state when the
    * frontier set is empty. */
  def isFinal: Boolean = ready.isEmpty

  def goalHints: Set[(T, TriggerHint)] = {
    val res = Set.newBuilder[(T, TriggerHint)]
    for(i <- ready) res += ((rule.subgoals(i), TriggerMatchIndex(i)))
    res.result()
  }
  def actionHints: Set[(T, TriggerHint)] = Set.empty[(T, TriggerHint)]

  def apply(trigger: T)(using hint: TriggerHint):
      Option[AllItem[T, H, S]] =
    Item.findMatchingSubgoal(trigger, rule.subgoals, hint)
      .map(
        (s, i) => {
          val derivedRule: All[T, H, S] = rule.apply(s)
          val sb = Set.newBuilder[Int]
          for(j <- ready; if i != j) { sb += j }
          for((before, after) <- rule.order; if before == i) { sb += after }
          AllItem(derivedRule, sb.result())
        }
      )

  def applications(trigger: T): Seq[TriggerHint] = {
    val builder = Seq.newBuilder[TriggerHint]
    for i <- 0 until rule.subgoals.length if rule.subgoals(i) >?< trigger
      do builder += TriggerMatchIndex(i)
    builder.result()
  }

  def triggers: Set[T] = for((trigger, _) <- goalHints) yield trigger
}

/** Position of an item in a [[org.maraist.planrec.rules.One One]]
  * rule.
  */
case class OneItem[T, H, S](val rule: One[T, H, S], val isFinal: Boolean)
  (using TermImpl[T, H, S]) extends Item[T, H, S] {

  override def equals(a: Any): Boolean = a match {
    case that: OneItem[?,?,?] => rule == that.rule && isFinal == that.isFinal
    case _ => false
  }
  override def hashCode(): Int =
    if isFinal then 2 * rule.hashCode() else rule.hashCode()

  def goalHints: Set[(T, TriggerHint)] = ???

  def actionHints: Set[(T, TriggerHint)] = Set.empty[(T, TriggerHint)]

  def apply(trigger: T)(using hint: TriggerHint):
      Option[OneItem[T, H, S]] =
    Item.findMatchingSubgoal(trigger, rule.subgoals, hint)
      .map(???)

  def applications(trigger: T): Seq[TriggerHint] = ???

  def triggers: Set[T] = ??? // Set.from(goalTriggers(rule, pos))
}


/** Position of an item in a [[org.maraist.planrec.rules.Act Act]]
  * rule.
  */
case class ActItem[T, H, S](val rule: Act[T, H, S], val isFinal: Boolean)
  (using TermImpl[T, H, S]) extends Item[T, H, S] {

  override def equals(a: Any): Boolean = a match {
    case that: ActItem[?, ?, ?] => rule == that.rule && isFinal == that.isFinal
    case _ => false
  }
  override def hashCode(): Int =
    if isFinal then 2 * rule.hashCode() else rule.hashCode()

  def goalHints: Set[(T, TriggerHint)] = ???

  def actionHints: Set[(T, TriggerHint)] = Set.empty[(T, TriggerHint)]

  def apply(trigger: T)(using hint: TriggerHint):
      Option[ActItem[T, H, S]] =
    ???

  def applications(trigger: T): Seq[TriggerHint] = ???

  def triggers: Set[T] = ??? // Set.from(goalTriggers(rule, pos))
}


/** This trait links one type of [[HTNrule]] with the type of item
  * positions within that rule, and the implementation of item
  * operations for the rule/position.  It may be that not every rule
  * form is used with an algorithm based on items, so there is no
  * mention of items in the rules themselves.
  *
  * @tparam T The type of terms used to describe goals, subgoals and
  * actions in these rules.
  * @tparam RuleType Type constructor for the plan rules whose items
  * are implemented here.  The argument to this type constructor must
  * implement [[TermImpl]].
  * @tparam PosType Type constructor for the position markers used in
  * the items implemented here.  The argument to this type constructor
  * must implement [[TermImpl]].
  */
sealed trait RuleItem[
  T, H, S,
  RuleType[X, Y, Z] <: HTNrule[X, Y, Z],
  PosType[X, Y, Z] <: Item[X, Y, Z]
]
  (using TermImpl[T, H, S]) {

  /** Build the initial item for the particular rule type.
    */
  def initialItem(rule: RuleType[T, H, S]): PosType[T, H, S]
}

extension [T, H, S](rule: HTNrule[T, H, S])(using termImpl: TermImpl[T,H,S]) {
  def initialItem: Item[T, H, S] =
    rule match {
      case allRule: All[T, H, S] => {
        val getDelayed = Set.newBuilder[Int]
        for((_,after) <- allRule.order) {
          getDelayed += after
        }

        val delayed = getDelayed.result()
        val getReady = Set.newBuilder[Int]
        for (i <- 0 until allRule.subgoals.length) {
          if (!delayed.contains(i)) {
            getReady += i
          }
        }

        AllItem[T, H, S](allRule, getReady.result())
      }
      case oneRule: One[T, H, S] => OneItem[T, H, S](oneRule, false)
      case actRule: Act[T, H, S] => ActItem[T, H, S](actRule, false)
    }
}

object HTN {
  import org.maraist.planrec.terms.Term.*
  export org.maraist.planrec.rules.{All, One, Act}
  export org.maraist.planrec.rules.HTN.HTNrule
  export org.maraist.planrec.rules.HTNLib
  export org.maraist.planrec.yr.table.initialItem
  export org.maraist.planrec.yr.table.{AllItem, OneItem, ActItem}
}
