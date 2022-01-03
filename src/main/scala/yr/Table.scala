// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr
import scala.collection.mutable.{ListBuffer, Queue}
import org.maraist.latex.{LaTeXdoc, LaTeXRenderable}
import org.maraist.planrec.rules.HTNLib
import org.maraist.planrec.rules.HTN.HTNrule
import org.maraist.planrec.terms.Term.*
import org.maraist.planrec.rules.HTN.*

class Table[T, H, S](
  initialIndex: Int,
  val termHeads: Vector[H],
  val states: Vector[Set[HState[T, H, S]]],
  private val headColMap: Map[H, Int],
  private val stateRowMap: Map[Set[HState[T, H, S]], Int],
  private val shifts: Array[Array[Option[Int]]],
  private val reduces: Array[List[(Int, HTNrule[T, H, S])]],
  private val sparks: Array[List[List[Int]]]
) /* (using TermImpl[T, H, S]) */
    extends LaTeXRenderable {

  opaque type Row = Int
  opaque type Col = Int

  val headCol: Map[H, Col] = headColMap

  val stateRow: Map[Set[HState[T, H, S]], Row] = stateRowMap

  inline def rowShift(state: Row, termHead: Col): Option[Row] =
    shifts(state)(termHead)

  inline def rowReduces(state: Row): List[(Row, HTNrule[T, H, S])] =
    reduces(state)

  inline def rowSparks(state: Row): List[List[Row]] = sparks(state)

  val initial: Row = initialIndex

  def toLaTeX(doc: LaTeXdoc): Unit = {
    doc ++= "\\begin{center}\n"

    val lastCol = 4 + termHeads.length
    val cline = s"\\cline{2-$lastCol}"
    doc ++= s"\\tabletail{$cline\\multicolumn{$lastCol}{r}{\\emph{continues}}\\\\}\n"
    doc ++= s"\\tablelasttail{$cline}\n"
    doc ++= s"\\tablehead{"
    doc ++= "\\multicolumn{4}{c}{} "
    for (i <- 0 until termHeads.length) do doc ++= s" & \\multicolumn{1}{c}{$i}"
    doc ++= "\\\\\n"
    doc ++= "\\multicolumn{1}{c}{} & \\multicolumn{2}{c}{Reduce} & \\multicolumn{1}{c}{Spark}"
    for (i <- 0 until termHeads.length) do doc ++= s" & \\multicolumn{1}{c}{${termHeads(i)}}"
    doc ++= s"\\\\ $cline\n"
    doc ++= "}\n" // end of \tablehead
    doc ++= "\\begin{supertabular}{r|r@{$\\:$}l|c|"
    for (i <- 0 until termHeads.length) do {
      doc ++= "|c"
    }
    doc ++= "|}\n"
    for (i <- 0 until states.length) do {
      doc ++= s"$i & "

      val rs: List[(Int, HTNrule[T, H, S])] = reduces(i)
      rs.length match {
        case 0 => doc ++= "\\multicolumn{2}{c|}{---}"
        case 1 => rs(0) match {
          case (row, rule) => {
            doc ++= s"{\\small [$row]} "
            rule.toLaTeX(doc)
          }
        }
        case _ => {
          doc ++= "\\begin{tabular}[c]{@{}c@{}}"
          for ((rule, len) <- rs) {
            doc ++= s"$rule [$len]\\\\"
          }
          doc ++= "\\end{tabular}"
        }
      }

      doc ++= " & "
      val ss: List[List[Int]] = sparks(i)
      ss.length match {
        case 0 => doc ++= "---"
        case 1 => doc ++= ss(0).map(_.toString).mkString(", ")
        case _ => {
          doc ++= "\\begin{tabular}[c]{@{}c@{}}"
          for (s <- ss) {
            doc ++= s.map(_.toString).mkString(", ")
            doc ++= "\\\\"
          }
          doc ++= "\\end{tabular}"
        }
      }

      for (j <- 0 until termHeads.length) do {
        doc ++= " & "
        shifts(i)(j).map((r) => doc ++= r.toString)
      }
      doc ++= s" \\\\ $cline\n"
    }
    doc ++= "\\end{supertabular}\n"
    doc ++= "\\end{center}\n"
  }
}

object Table {
  def apply[T, H, S](library: HTNLib[T, H, S])(using TermImpl[T, H, S]):
      Table[T, H, S] = {

    val dfa = HandleDFA(library)
    val dfaLabelMax = dfa.labels.length
    val labels: IndexedSeq[H] = {
      val allLabels = IndexedSeq.newBuilder[H]
      allLabels ++= dfa.labels
      for (topNonterm <- library.top)
        do if !dfa.labels.contains(topNonterm) then allLabels += topNonterm
      allLabels.result
    }
    val states: IndexedSeq[Set[HState[T, H, S]]] = dfa.states

    val stations: Set[H] = {
      val builder = Set.newBuilder[H]
      for (state <- states; elem <- state) do elem match {
        case Sparking(_, _) => { }
        case AllItem(_, _, _) => { }
        case OneItem(_, _) => { }
        case ActItem(_, _) => { }
        case h: H => builder += h
      }
      builder.result
    }
    // println(states)
    // println(stations)

    val stationHome: Map[H, Int] = {
      val builder = Map.newBuilder[H, Int]

      for (station <- stations) {
        var home = -1
        var bestSize = Int.MaxValue

        for (i <- 0 until states.size) do {
          val state = states(i)
          if state.contains(station) && state.size < bestSize then {
            home = i
            bestSize = state.size
          }
        }

        if (home > -1) then builder += ((station, home))
      }

      builder.result
    }
    // println(stationHome)

    val headCol: Map[H, Int] = {
      val builder = Map.newBuilder[H, Int]
      for (i <- 0 until labels.length)
        do builder += ((labels(i), i))
      builder.result
    }

    val stateRow: Map[Set[HState[T, H, S]], Int] = {
      val builder = Map.newBuilder[Set[HState[T, H, S]], Int]
      for (i <- 0 until states.length)
        do builder += ((states(i), i))
      builder.result
    }

    val shifts = Array.tabulate(states.length, labels.length)((s, l) =>
      if l < dfaLabelMax then dfa.transitionIndex(s, l) else None)

    val reduces = Array.tabulate(states.length)((s) => {
      val buf = new RowPairListBuffer[T, H, S](headCol)
      // println(headCol)
      for(elem <- dfa.state(s)) do elem match {
        case i@AllItem(_, _, _): AllItem[T, H, S] => buf.consider(i)
        case i@OneItem(_, _): OneItem[T, H, S] => buf.consider(i)
        case i@ActItem(_, _): ActItem[T, H, S] => buf.consider(i)
        // If it's sparking subtasks, better not be a final item.
        case Sparking(_, i): Sparking[T, H, S] => { }
        case _: H => { }
      }

      buf.result
    })

    val sparks = Array.tabulate(states.length)((s) => {
      val buf = Set.newBuilder[List[Int]]

      for(elem <- dfa.state(s)) do elem match {
        case Sparking(indirs, _): Sparking[T, H, S] =>
          buf += indirs.map(stationHome)
        case _ => { }
      }

      buf.result.toList
    })
    // sparks.map(println)

    new Table(
      dfa.initialStateIndex, labels.toVector, states.toVector,
      headCol, stateRow, shifts, reduces, sparks)
  }
}

class RowPairListBuffer[T, H, S](val headCol: Map[H, Int])
  (using TermImpl[T, H, S])
    extends ListBuffer[(Int, HTNrule[T, H, S])] {
  def consider(i: Item[T, H, S]): Unit = {
    if i.isFinal then +=((headCol(i.rule.goal.termHead), i.rule))
  }
}

