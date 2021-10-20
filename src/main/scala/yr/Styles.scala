// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr.table
import org.maraist.graphviz.Graphable
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle
import org.maraist.planrec.rules.{All,One,Act,TriggerHint,TriggerMatchIndex}

class StyleNFA[T, H, S](
  id: String = "",
  nodeShape: (Node[T, H, S], Graphable[Node[T, H, S], H, ?]) => String =
    (s: Node[T, H, S], _: Graphable[Node[T, H, S], H, ?]) => "circle",
  nodeLabel: (Node[T, H, S], Graphable[Node[T, H, S], H, ?]) => String =
    (s: Node[T, H, S], _: Graphable[Node[T, H, S], H, ?]) => s.toString(),
  finalNodeShape: (Node[T, H, S], Graphable[Node[T, H, S], H, ?]) => String =
    (s: Node[T, H, S], _: Graphable[Node[T, H, S], H, ?]) => "doublecircle",
  edgeLabel: (H, Node[T, H, S], Node[T, H, S], Graphable[Node[T, H, S], H, ?]) => String =
    (t: H, _: Node[T, H, S], _: Node[T, H, S], _: Graphable[Node[T, H, S], H, ?]) => t.toString()
)
    extends EdgeAnnotatedAutomatonStyle[
  Node[T, H, S],
  H,
  NfaAnnotation[T, H, S]
](id = id, nodeShape = nodeShape, nodeLabel = nodeLabel,
  finalNodeShape = finalNodeShape)

class StyleDFA[T, H, S](
  id: String = "",
  nodeShape: (Set[Node[T, H, S]], Graphable[Set[Node[T, H, S]], H, ?]) => String =
    (s: Set[Node[T, H, S]], _: Graphable[Set[Node[T, H, S]], H, ?]) => "circle",
  nodeLabel: (Set[Node[T, H, S]], Graphable[Set[Node[T, H, S]], H, ?]) => String =
    (s: Set[Node[T, H, S]], _: Graphable[Set[Node[T, H, S]], H, ?]) => s.toString(),
  finalNodeShape: (Set[Node[T, H, S]], Graphable[Set[Node[T, H, S]], H, ?]) => String =
    (s: Set[Node[T, H, S]], _: Graphable[Set[Node[T, H, S]], H, ?]) => "doublecircle",
  edgeLabel: (H, Set[Node[T, H, S]], Set[Node[T, H, S]], Graphable[Set[Node[T, H, S]], H, ?]) => String =
    (t: H, _: Set[Node[T, H, S]], _: Set[Node[T, H, S]], _: Graphable[Set[Node[T, H, S]], H, ?]) => t.toString()
) extends EdgeAnnotatedAutomatonStyle[
  Set[Node[T, H, S]],
  H,
  Set[NfaAnnotation[T, H, S]]
](id = id, nodeShape = nodeShape, nodeLabel = nodeLabel,
  finalNodeShape = finalNodeShape)

def nodeDOT[T, H, S](node: Node[T, H, S]): String = node match {
  case AllItem(All(goal, subgoals, _), ready) => {
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

given yrNfaGraphStyle[T, H, S]: StyleNFA[T, H, S] =
  new StyleNFA[T, H, S](
    id = "yrNfaGraphStyle",

    finalNodeShape = (s: Node[T, H, S], _: Graphable[Node[T, H, S], H, ?])
      => "box3d",

    nodeShape = (s: Node[T, H, S], _: Graphable[Node[T, H, S], H, ?])
      => s match {
        case _: Item[_, _, _] => "rectangle"
        case _: Ind[_, _, _]  => "rectangle"
        case _ => "circle"
      },

    edgeLabel = (
      t: H, s0: Node[T, H, S], s1: Node[T, H, S],
      graph: Graphable[Node[T, H, S], H, ?]
    ) => t.toString + (graph match {
      case nfa: NondetHandleFinder[T, H, S] => {
        nfa.annotation(s0, t, s1) match {
          case Some(NfaAnnotation(indirs)) => {
            val sb = new StringBuilder
            sb ++= " {"
            var sep = ""
            for (ind <- indirs) do {
              sb ++= sep
              sb ++= ind.toString()
              sep = ", "
            }
            sb ++= "}"
            sb.toString()
          }
          case None => ""
        }
      }
    }),

    nodeLabel = (node: Node[T, H, S], _: Graphable[Node[T, H, S], H, ?])
      => nodeDOT(node)
  )

given yrDfaGraphStyleTest[T, H, S]: StyleDFA[T, H, S] =
  new StyleDFA[T, H, S](
    id = "yrDfaGraphStyleTest",

    edgeLabel = (
      t: H, s0: Set[Node[T, H, S]], s1: Set[Node[T, H, S]],
      graph: Graphable[Set[Node[T, H, S]], H, ?]
    ) => "EE",

    nodeLabel = (_: Set[Node[T, H, S]], _: Graphable[Set[Node[T, H, S]], H, ?])
      => "NN"

  )

given yrDfaGraphStyle[T, H, S]: StyleDFA[T, H, S] =
  new StyleDFA[T, H, S](
    id = "yrDfaGraphStyle",

    finalNodeShape = (
      s: Set[Node[T, H, S]],
      _: Graphable[Set[Node[T, H, S]], H, ?]
    ) => "box3d",

    nodeShape = (s: Set[Node[T, H, S]], _: Graphable[Set[Node[T, H, S]], H, ?])
      => "rectangle",

    edgeLabel = (
      t: H, s0: Set[Node[T, H, S]], s1: Set[Node[T, H, S]],
      graph: Graphable[Set[Node[T, H, S]], H, ?]
    ) => t.toString + (graph match {
      case dfa: HandleFinder[T, H, S] => {
        dfa.annotation(s0, t) match {
          case Some(annSet) => {
            val sb = new StringBuilder
            var sep2 = ""
            for (ann <- annSet) do ann match {
              case NfaAnnotation(indirs) => {
                sb ++= sep2
                sb ++= " {"
                var sep = ""
                for (ind <- indirs) do {
                  sb ++= sep
                  sb ++= ind.toString()
                  sep = ", "
                }
                sb ++= "}"
                sep2 = "; "
              }
            }
            sb.toString()
          }
          case None => ""
        }
      }
    }),

    nodeLabel = (
      nodeSet: Set[Node[T, H, S]],
      graph: Graphable[Set[Node[T, H, S]], H, ?]
    ) => {
      val sb = new StringBuilder
      var sep = ""
      for (node <- nodeSet) do {
        sb ++= sep
        sb ++= nodeDOT(node)
        sep = "<br/>"
      }
      sb.toString()
    }
  )
