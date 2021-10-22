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
import org.maraist.planrec.rules.
  {All, One, Act, TriggerHint, TriggerMatchIndex, NoHint}
import org.maraist.planrec.rules.HTN.*
import org.maraist.planrec.terms.TermImpl
import org.maraist.planrec.terms.Term.termHead
import TriggerHint.*
import org.maraist.planrec.terms.{>?<, >><<, >+<}
import scala.compiletime.ops.any

/** An item is the combination of a rule and a particular state of
  * progress through that rule.
  * @tparam T Type of terms for goals and actions in rules and items
  * @param impl Item implementation.
  */
sealed trait Item[T, H, S](using impl: TermImpl[T, H, S]) {

  /** HTN [[org.maraist.planrec.rules.HTN.HTNrule rule]] associated with this item. */
  def rule: HTNrule[T, H, S]

  /** Returns `true` if this item is a final item for its rule. */
  def isFinal: Boolean

  /** The triggers corresponding to actions for this item. */
  def actionHints: Set[(H, TriggerHint)]

  /** All triggers for this item. */
  def triggers: Set[H] = for((trigger, _) <- actionHints) yield trigger

  /** Result of advancing this item for the given trigger.
    *
    * Note that Skolemizing, if needed, should be performed *before*
g    * calling this method.
    *
    * @param subst The substitution to be applied to the result.
    * @param trigger The action or subgoal used to advance this item.
    * @param hint Directive for resolving the trigger to a particular
    * subgoal of this item's rule.
    */
  def apply(trigger: H)(using hint: TriggerHint):
      Option[Item[T, H, S]]

  /** Hints which might apply to this trigger, if it is ambiguous within
    * the possible subgoals/actions.
    * @param trigger The action or subgoal under question.
    */
  def applications(trigger: H): Seq[TriggerHint]
}

/** Helper methods. */
object Item {
  import scala.util.control.NonLocalReturns.*

  def findMatchingSubgoal[T, H, S]
    (trigger: H, subgoals: IndexedSeq[T], hint: TriggerHint)
    (using impl: TermImpl[T, H, S]):
      Option[Int] = hint match {
    case TriggerMatchIndex(i) => subgoals(i).termHead.equals(trigger) match {
      case true => Some(i)
      case false => None
    }
    case _ => returning {
      for(i <- 0 until subgoals.length) {
        if (subgoals(i).termHead.equals(trigger)) {
          throwReturn[Option[Int]](Some(i))
        }
      }
      None
    }
  }
}

/** Items associated with [[org.maraist.planrec.rules.All]] rules.
  * Corresponds to the [[Item.all]] instance of [[RuleItem]].
  *
  * @param rule Rule associated a particular item instance
  * @param ready The frontier of subgoals which are neither satisfied
  * nor blocked in this item.
  */
case class AllItem[T, H, S](val rule: All[T, H, S], val ready: Set[Int])
  (using TermImpl[T, H, S])
    extends Item[T, H, S] {

  /** In [[org.maraist.planrec.rules.All]] rules' items, the item is in
    * the final state when the frontier set is empty. */
  def isFinal: Boolean = ready.isEmpty

  def actionHints: Set[(H, TriggerHint)] = {
    val res = Set.newBuilder[(H, TriggerHint)]
    for(i <- ready) res += ((rule.subgoals(i).termHead, TriggerMatchIndex(i)))
    res.result()
  }

  def apply(trigger: H)(using hint: TriggerHint):
      Option[AllItem[T, H, S]] =
    Item.findMatchingSubgoal(trigger, rule.subgoals, hint)
      .flatMap(applyIdx(_))

  def applyIdx(i: Int): Option[AllItem[T, H, S]] =
    if (ready.contains(i))
      then {
        val sb = Set.newBuilder[Int]
        for(j <- ready; if i != j) { sb += j }
        for((before, after) <- rule.order; if before == i) { sb += after }
        Some(AllItem(rule, sb.result()))
      }
      else None

  def applications(trigger: H): Seq[TriggerHint] = {
    val builder = Seq.newBuilder[TriggerHint]
    for i <- 0 until rule.subgoals.length
        if rule.subgoals(i).termHead.equals(trigger)
      do builder += TriggerMatchIndex(i)
    builder.result()
  }
}

/** Position of an item in a [[org.maraist.planrec.rules.One One]]
  * rule.
  */
case class OneItem[T, H, S](val rule: One[T, H, S], val isFinal: Boolean)
  (using impl: TermImpl[T, H, S]) extends Item[T, H, S] {

  override def equals(a: Any): Boolean = a match {
    case that: OneItem[?,?,?] => rule == that.rule && isFinal == that.isFinal
    case _ => false
  }

  override def hashCode(): Int =
    if isFinal then 2 * rule.hashCode() else rule.hashCode()

  def actionHints: Set[(H, TriggerHint)] = {
    val res = Set.newBuilder[(H, TriggerHint)]
    for(i <- 0 until rule.subgoals.size)
      res += ((rule.subgoals(i).termHead, TriggerMatchIndex(i)))
    res.result()
  }

  def apply(trigger: H)(using hint: TriggerHint): Option[OneItem[T, H, S]] =
    if isFinal then None
    else
      Item.findMatchingSubgoal(trigger, rule.subgoals, hint)
        .map((x) => OneItem(rule, true))

  def applications(trigger: H): Seq[TriggerHint] =
    for(
      i <- 0 until rule.subgoals.size;
      if rule.subgoals(i).termHead.equals(trigger)
    ) yield TriggerMatchIndex(i)
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

  def actionHints: Set[(H, TriggerHint)] =
    if isFinal then Set.empty[(H, TriggerHint)]
    else Set[(H, TriggerHint)]((rule.action.termHead, NoHint()))

  def apply(trigger: H)(using hint: TriggerHint):
      Option[ActItem[T, H, S]] =
    if !isFinal && rule.action.termHead.equals(trigger) then
      Some(ActItem(rule,true))
    else None

  def applications(trigger: H): Seq[TriggerHint] =
    if isFinal || !rule.action.termHead.equals(trigger) then Seq()
    else Seq(NoHint())
}


/** This trait links one type of
  * [[org.maraist.planrec.rules.HTN.HTNrule]] with the type of item
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
