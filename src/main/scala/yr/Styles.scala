// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr
import org.maraist.graphviz.Graphable
import org.maraist.fa
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle
import org.maraist.fa.traits.EdgeAnnotatedDFA
import org.maraist.planrec.rules.{All,One,Act,TriggerHint,TriggerMatchIndex}

// class StyleNFA[T, H, S](id: String = "")
//
// extends EdgeAnnotatedAutomatonStyle[HState[T, H, S], H, NfaAnnotation[T, H, S]](
//   id = id,
//
//   // TODO remove --- for debugging
//   keepDOT = true,
//
//   finalNodeShape = (s: HState[T, H, S], _: Graphable[HState[T, H, S], H, ?])
//     => "box3d",
//
//   nodeShape = (s: HState[T, H, S], _: Graphable[HState[T, H, S], H, ?])
//     => s match {
//       case _: Item[_, _, _] => "rectangle"
//       case _: Ind[_, _, _]  => "rectangle"
//       case _ => "circle"
//     },
//
//   annotationLabel = (
//     a: NfaAnnotation[T, H, S], t: H, s0: HState[T, H, S], s1: HState[T, H, S]
//   ) => nfaAnnotationToLaTeX(a),
//
//   eAnnotationLabel = (
//     a: NfaAnnotation[T, H, S],
//     s0: HState[T, H, S],
//     s1: HState[T, H, S]) => nfaAnnotationToLaTeX(a),
//
//   nodeLabel = (
//     n: HState[T, H, S], _: Graphable[HState[T, H, S], H, ?]) => nodeDOT(n)
// )
//
// given yrNfaGraphStyle[T, H, S]: StyleNFA[T, H, S] =
//   new StyleNFA[T, H, S](id = "yrNfaGraphStyle")
//
// private[yr] def nfaAnnotationToLaTeX[T, H, S](ann: NfaAnnotation[T, H, S]):
//     String = ann match {
//   case NfaAnnotation(indirs) => {
//     val sb = new StringBuilder
//     sb ++= " {"
//     var sep = ""
//     for (ind <- indirs) do {
//       sb ++= sep
//       sb ++= ind.toString()
//       sep = ", "
//     }
//     sb ++= "}"
//     sb.toString()
//   }
// }

// =================================================================

// class StyleDFA[T, H, S](id: String = "")
//
// extends EdgeAnnotatedAutomatonStyle[
//   Set[HState[T, H, S]], H, Set[DfaAnnotation[T, H, S]]
// ](
//
//   id = id,
//
//   // TODO remove --- for debugging
//   keepDOT = true,
//
//   finalNodeShape = (
//     s: Set[HState[T, H, S]],
//     _: Graphable[Set[HState[T, H, S]], H, ?]
//   ) => "box3d",
//
//   nodeShape = (s: Set[HState[T, H, S]], _: Graphable[Set[HState[T, H, S]], H, ?])
//     => "rectangle",
//
//   annotationLabel = (
//     anns: Set[DfaAnnotation[T, H, S]],
//     _: H, _: Set[HState[T, H, S]], _: Set[HState[T, H, S]]
//   ) => "{" + anns.map(dfaAnnotationToLaTeX).mkString(", ") + "}",
//
//   eAnnotationLabel = (
//     anns: Set[DfaAnnotation[T, H, S]],
//     _: Set[HState[T, H, S]],
//     _: Set[HState[T, H, S]]
//   ) => "{" + anns.map(dfaAnnotationToLaTeX).mkString(", ") + "}",
//
//   initialAnnotationLabel = (
//     anns: Set[DfaAnnotation[T, H, S]],
//     _: Set[HState[T, H, S]]
//   ) => "{" + anns.map(dfaAnnotationToLaTeX).mkString(", ") + "}",
//
//   nodeLabel = (
//     ns: Set[HState[T, H, S]], _: Graphable[Set[HState[T, H, S]], H, ?]
//   ) => ns.map(nodeDOT).mkString("<br/>")
// )
//
// given yrDfaGraphStyle[T, H, S]: StyleDFA[T, H, S] =
//   new StyleDFA[T, H, S](id = "yrDfaGraphStyle")
//
// private[yr] def dfaAnnotationToLaTeX[T, H, S](ann: DfaAnnotation[T, H, S]):
//     String = ann match {
//   case DfaAnnotation(indirs) => {
//     val sb = new StringBuilder
//     sb ++= " {"
//     var sep = ""
//     for ((h, ind) <- indirs) do {
//       sb ++= s"$sep<sup>${ind.toString()}</sup>$h"
//       sep = ", "
//     }
//     sb ++= "}"
//     sb.toString()
//   }
// }

// =================================================================

def nodeDOT[T, H, S](node: HState[T, H, S]): String = node match {
  case AllItem(All(goal, subgoals, _), ready, _) => {
    val sb = new StringBuilder
    sb ++= goal.toString()
    sb ++= " &rarr;"
    for (i <- 0 until subgoals.size) do {
      sb ++= " "
      if (ready.contains(i)) { sb ++= "&#x2022;" }
      sb ++= subgoals(i).toString()
    }
    if (ready.isEmpty) { sb ++= "&#x2022;" }
    sb.toString()
  }
  case OneItem(One(goal, subgoals, _), isFinal) => {
    val sb = new StringBuilder
    sb ++= goal.toString()
    sb ++= " &rarr; "
    if (!isFinal) { sb ++= "&#x2022;" }
    sb ++= "("
    var sep = ""
    for (subgoal <- subgoals) do {
      sb ++= sep
      sb ++= subgoal.toString()
      sep = " | "
    }
    sb ++= ")"
    if (isFinal) { sb ++= "&#x2022;" }
    sb.toString()
  }
  case ActItem(Act(goal, action), isFinal) => {
    val sb = new StringBuilder
    sb ++= goal.toString()
    sb ++= " &rarr; "
    if (!isFinal) { sb ++= "&#x2022;" }
    sb ++= action.toString()
    if (isFinal) { sb ++= "&#x2022;" }
    sb.toString()
  }
  case Ind(_) => "NN"
  // Last case is for the station, not under a constructor.
  case _ => node.toString
}
