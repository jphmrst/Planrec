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

type NFAbuild[T, H, S] = NDFABuilder[Item.Node[T, H, S], ?, ?, ?, ?]

extension [T, H, S](rule: HTNrule[T, H, S])(using termImpl: TermImpl[T,H,S]) {
  def queueInitialRuleForms(
    queue: Queue[(Option[(Item[T, H, S], T)], Item[T, H, S])],
    nfa: NDFABuilder[Node[T, H, S],?,?,?,?]):
      Unit =
    rule match {
      case allRule: All[T, H, S] =>
        Item.all.queueInitialRuleForms(allRule, nfa, queue)
      case oneRule: One[T, H, S] =>
        Item.one.queueInitialRuleForms(oneRule, nfa, queue)
      case actRule: Act[T, H, S] =>
        Item.act.queueInitialRuleForms(actRule, nfa, queue)
    }
}
