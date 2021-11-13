// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr
import scala.collection.mutable.Queue
import org.maraist.graphviz.Graphable
import org.maraist.fa.{
  EdgeAnnotatedNFA, EdgeAnnotatedNFABuilder, EdgeAnnotatedDFA}
import org.maraist.fa.util
import org.maraist.planrec.rules.{All,One,Act,TriggerHint,TriggerMatchIndex}
import org.maraist.planrec.rules.HTN.*
import org.maraist.planrec.terms.TermImpl
import org.maraist.planrec.terms.Term.termHead
import TriggerHint.*
import org.maraist.planrec.terms.{>?<, >><<}
import scala.compiletime.ops.any

case class Ind[T, H, S](val rule: HTNrule[T, H, S])

// case class NfaAnnotation[T, H, S](indirects: List[H])

// case class DfaAnnotation[T, H, S](indirects: List[(H, Int)])

case class Sparking[T, H, S](indirects: List[H], item: AllItem[T, H, S])

type HState[T, H, S] = Sparking[T, H, S] | Item[T, H, S] | H

type ItemsQueue[T, H, S] =
  Queue[(
    HState[T, H, S], Set[Int], Option[Int],
    Sparking[T, H, S] | AllItem[T, H, S])]
