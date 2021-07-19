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
import org.maraist.planrec.terms.{>?<, >><<}
import scala.compiletime.ops.any

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
sealed trait RuleItem[T, H, RuleType[X] <: HTNrule[X, H], PosType[X] <: Item[X, H]]
  (using TermImpl[T,H,?]) {

  /** Build the initial item for the particular rule type.
    */
  def initialItem(rule: RuleType[T]): PosType[T]

  /** Add the basic NFA elements for this rule to a handle-finding NFA,
    * and to the queue of items to process.  By default, this method:
    *
    *  1. Adds the rule's station to the NFA.
    *
    *  1. Adds the rule's initial item, and any other associated
    *     nodes, to the NFA.
    *
    *  1. Adds the rule's initial item to the queue.
    *
    * Most implementations of [[RuleItem]] should override this
    * method, but still call this superclass method at some point.
    */
  def queueInitialRuleForms[I >: PosType[T]](
    rule: RuleType[T],
    nfa: NDFABuilder[Item.Node[T,H],?,?,?,?],
    queue: Queue[(Option[(I, T)], I)]
  ): Unit =
    queue.enqueue((None, initialItem(rule)))
}

/** An item is the combination of a rule and a particular state of
  * progress through that rule.
  * @tparam T Type of terms for goals and actions in rules and items
  * @param impl Item implementation.
  */
sealed trait Item[T,H](using impl: TermImpl[T,H,?]) {

  /** HTN [[HTNrule rule]] associated with this item. */
  def rule: HTNrule[T, H]

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
    * @param trigger The action or subgoal used to advance this item.
    * @param hint Directive for resolving the trigger to a particular
    * subgoal of this item's rule.
    */
  def apply(trigger: T)(using hint: TriggerHint): Option[Item[T, H]]

  /** Hints which might apply to this trigger, if it is ambiguous within
    * the possible subgoals/actions.
    * @param trigger The action or subgoal under question.
    */
  def applications(trigger: T): Seq[TriggerHint]

  /** Add the necessary components to an [[NDFABuilder]] for the `next`
    * item (which may already have been added to the NFA), and for the
    * transition from the `prev` item to the `next` item.  Additional
    * items pairs may be pushed to the `queue`, but no pairs should be
    * read from it.
    *
    * @param prev Item from which the `next` item was derived, if any.
    * @param next The item being added to the NFA.
    * @param queue Queue of items to subsequently be added to the NFA.
    */
  def encodeItemNode(
    nfa: NDFABuilder[Item.Node[T,H],?,?,?,?],
    queue: Queue[(Option[(Item[T, H], T)], Item[T, H])]):
      Unit

  /** Add the necessary components to an [[NDFABuilder]] for the `next`
    * item (which may already have been added to the NFA), and for the
    * transition from the `prev` item to the `next` item.  Additional
    * items pairs may be pushed to the `queue`, but no pairs should be
    * read from it.
    *
    * @param prev Item from which the `next` item was derived, if any.
    * @param next The item being added to the NFA.
    * @param queue Queue of items to subsequently be added to the NFA.
    */
  def encodeItemTransition(
    prev: Option[(Item[T, H], T)],
    nfa: NDFABuilder[Item.Node[T,H],?,?,?,?],
    queue: Queue[(Option[(Item[T, H], T)], Item[T, H])]):
      Unit

}

/** Items associated with [[All]] rules.  Corresponds to the
  * [[Item.all]] instance of [[RuleItem]].
  *
  * @param rule Rule associated a particular item instance
  * @param ready The frontier of subgoals which are neither satisfied
  * nor blocked in this item.
  */
case class AllItem[T, H](
  val rule: All[T, H],
  val ready: Set[Int]
)(
  using TermImpl[T, H,?]
) extends Item[T, H] {

  /** In [[All]] rules' items, the item is in the final state when the
    * frontier set is empty. */
  def isFinal: Boolean = ready.isEmpty

  def goalHints: Set[(T, TriggerHint)] = {
    val res = Set.newBuilder[(T, TriggerHint)]
    for(i <- ready) res += ((rule.subgoals(i), TriggerMatchIndex(i)))
    res.result()
  }
  def actionHints: Set[(T, TriggerHint)] = Set.empty[(T, TriggerHint)]

  def apply(trigger: T)(using hint: TriggerHint): Option[AllItem[T, H]] = {
    import scala.util.control.NonLocalReturns.*
    val idx: Option[Int] = hint match {
      case TriggerMatchIndex(i) => Some(i)
      case _ => returning {
        for(i <- 0 until rule.subgoals.length) {
          if (rule.subgoals(i) >?< trigger) {
            throwReturn[Option[Int]](Some(i))
          }
        }
        None
      }
    }

    idx.flatMap(rule.subgoals(_) >><< trigger)
      .map((term) => {
        val i = idx.get
        val sb = Set.newBuilder[Int]
        for(j <- ready; if i != j) { sb += j }
        for((before, after) <- rule.order; if before == i) { sb += after }
        AllItem(rule, sb.result())
      })
  }

  def applications(trigger: T): Seq[TriggerHint] = {
    val builder = Seq.newBuilder[TriggerHint]
    for i <- 0 until rule.subgoals.length if rule.subgoals(i) >?< trigger
      do builder += TriggerMatchIndex(i)
    builder.result()
  }

  def triggers: Set[T] = for((trigger, _) <- goalHints) yield trigger

  def encodeItemNode(
    nfa: NDFABuilder[Item.Node[T,H],?,?,?,?],
    queue: Queue[(Option[(Item[T, H], T)], Item[T, H])]):
      Unit = {

    // Add the item as a node if it is not already in the NDA.
    if !(nfa.isState(this)) then nfa.addState(this)
  }

  def encodeItemTransition(
    prev: Option[(Item[T, H], T)],
    nfa: NDFABuilder[Item.Node[T,H],?,?,?,?],
    queue: Queue[(Option[(Item[T, H], T)], Item[T, H])]):
      Unit = {

    val prior = prev match {
      case None => ()
      case Some(item, trigger) => ()
    }
    // If there is only one ready element, then we can just add a
    // simple item.  ***BUT*** we need a trigger for the transition
    // itself.

    // Otherwise if there was only one ready element in the `prev`, or
    // if there is no `prev`, then we need to add an indirection node
    // since we now must synchronize two possibly-interleaved
    // subgoals.

    // Moreover if we have more than one element of the ready set, and
    // it is also not a subset of the predecessor, then we need to
    // annotate with concurrent stack top spawning.

    ???
  }
}

/** Position of an item in a [[org.maraist.planrec.rules.One One]]
  * rule.
  */
case class OneItem[T, H](val rule: One[T, H], val isFinal: Boolean)
  (using TermImpl[T,H,?]) extends Item[T, H] {

  override def equals(a: Any): Boolean = a match {
    case that: OneItem[?,?] => rule == that.rule && isFinal == that.isFinal
    case _ => false
  }
  override def hashCode(): Int =
    if isFinal then 2 * rule.hashCode() else rule.hashCode()

  def goalHints: Set[(T, TriggerHint)] = ???

  def actionHints: Set[(T, TriggerHint)] = Set.empty[(T, TriggerHint)]

  def apply(trigger: T)(using hint: TriggerHint): Option[OneItem[T, H]] =
    ???

  def applications(trigger: T): Seq[TriggerHint] = ???

  def triggers: Set[T] = ??? // Set.from(goalTriggers(rule, pos))

  def encodeItemNode(
    nfa: NDFABuilder[Item.Node[T,H],?,?,?,?],
    queue: Queue[(Option[(Item[T, H], T)], Item[T, H])]):
      Unit = ???

  def encodeItemTransition(
    prev: Option[(Item[T, H], T)],
    nfa: NDFABuilder[Item.Node[T,H],?,?,?,?],
    queue: Queue[(Option[(Item[T, H], T)], Item[T, H])]):
      Unit = ???
}


/** Position of an item in a [[org.maraist.planrec.rules.Act Act]]
  * rule.
  */
case class ActItem[T, H](val rule: Act[T, H], val isFinal: Boolean)
  (using TermImpl[T,H,?]) extends Item[T, H] {

  override def equals(a: Any): Boolean = a match {
    case that: ActItem[?, ?] => rule == that.rule && isFinal == that.isFinal
    case _ => false
  }
  override def hashCode(): Int =
    if isFinal then 2 * rule.hashCode() else rule.hashCode()

  def goalHints: Set[(T, TriggerHint)] = ???

  def actionHints: Set[(T, TriggerHint)] = Set.empty[(T, TriggerHint)]

  def apply(trigger: T)(using hint: TriggerHint): Option[ActItem[T, H]] =
    ???

  def applications(trigger: T): Seq[TriggerHint] = ???

  def triggers: Set[T] = ??? // Set.from(goalTriggers(rule, pos))

  def encodeItemNode(
    nfa: NDFABuilder[Item.Node[T,H],?,?,?,?],
    queue: Queue[(Option[(Item[T, H], T)], Item[T, H])]):
      Unit = ???

  def encodeItemTransition(
    prev: Option[(Item[T, H], T)],
    nfa: NDFABuilder[Item.Node[T,H],?,?,?,?],
    queue: Queue[(Option[(Item[T, H], T)], Item[T, H])]):
      Unit = ???
}


/** This object contains the `given` implementations of [[RuleItem]]
  * for various rules.
  */
object Item {
  type Node[T,H] = Item[T, H] | H | Ind[Item[T, H], H]

  given all[T,H](using TermImpl[T,H,?]):
      RuleItem[T, H, [X] =>> All[X, H], [X] =>> AllItem[X, H]] with {

    def initialItem(rule: All[T, H]): AllItem[T, H] = {
      val getDelayed = Set.newBuilder[Int]
      for((_,after) <- rule.order) {
        getDelayed += after
      }

      val delayed = getDelayed.result()
      val getReady = Set.newBuilder[Int]
      for (i <- 0 until rule.subgoals.length) {
        if (!delayed.contains(i)) {
          getReady += i
        }
      }

      AllItem[T, H](rule, getReady.result())
    }

    override def queueInitialRuleForms[I >: AllItem[T, H]](
      rule: All[T, H],
      nfa: NDFABuilder[Node[T,H],?,?,?,?],
      queue: Queue[(Option[(I, T)], I)]):
        Unit = {
      ???
      super.queueInitialRuleForms[I](rule, nfa, queue)
    }
  }

  given one[T,H](using TermImpl[T,H,?]):
      RuleItem[T, H, [X] =>> One[X, H], [X] =>> OneItem[X, H]] with {
    def initialItem(rule: One[T, H]): OneItem[T, H] = OneItem[T, H](rule, false)

    override def queueInitialRuleForms[I >: OneItem[T, H]](
      rule: One[T, H],
      nfa: NDFABuilder[Node[T,H],?,?,?,?],
      queue: Queue[(Option[(I, T)], I)]):
        Unit = {
      ???
      super.queueInitialRuleForms[I](rule, nfa, queue)
    }
  }

  given act[T,H](using TermImpl[T,H,?]):
      RuleItem[T, H, [X] =>> Act[X, H], [X] =>> ActItem[X, H]] with {
    def initialItem(rule: Act[T, H]): ActItem[T, H] = ActItem[T, H](rule, false)

    override def queueInitialRuleForms[I >: ActItem[T, H]](
      rule: Act[T, H],
      nfa: NDFABuilder[Node[T,H],?,?,?,?],
      queue: Queue[(Option[(I, T)], I)]):
        Unit = {
      ???
      super.queueInitialRuleForms[I](rule, nfa, queue)
    }
  }
}

import org.maraist.planrec.yr.table.Item.*

extension [T,H](rule: HTNrule[T, H])(using termImpl: TermImpl[T,H,?]) {
  def initialItem: Item[T, H] =
    rule match {
      case allRule: All[T, H] => Item.all.initialItem(allRule)
      case oneRule: One[T, H] => Item.one.initialItem(oneRule)
      case actRule: Act[T, H] => Item.act.initialItem(actRule)
    }

  def queueInitialRuleForms(
    queue: Queue[(Option[(Item[T, H], T)], Item[T, H])],
    nfa: NDFABuilder[Node[T,H],?,?,?,?]):
      Unit =
    rule match {
      case allRule: All[T, H] =>
        Item.all.queueInitialRuleForms(allRule, nfa, queue)
      case oneRule: One[T, H] =>
        Item.one.queueInitialRuleForms(oneRule, nfa, queue)
      case actRule: Act[T, H] =>
        Item.act.queueInitialRuleForms(actRule, nfa, queue)
    }
}

object HTN {
  import org.maraist.planrec.terms.Term.*
  export org.maraist.planrec.rules.{All, One, Act}
  export org.maraist.planrec.rules.HTN.HTNrule
  export org.maraist.planrec.yr.table.initialItem
  export org.maraist.planrec.yr.table.{AllItem, OneItem, ActItem}
  export org.maraist.planrec.yr.table.Item.{all, one, act}
}
