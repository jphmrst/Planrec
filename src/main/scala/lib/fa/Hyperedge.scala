// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa
import org.maraist.graphviz.NodeLabeling
import org.maraist.graphviz.TransitionLabeling

trait Hyperedge[S] {

  def eHyperedgeTargets(s:S): Set[Set[S]]

  /** Intended to be called from an override of
   *  {@link org.maraist.fa.NDFA#internalsToDOT} or
   *  {@link org.maraist.fa.DFA#internalsToDOT}
   *
   * FIXME This method may be redundant and removable --- see commented-out
   * call in HyperedgeDFA.internalsToDOT; are there other calls to this
   * method?
   */
  protected def eHyperedgesToDOT(nodeLabeling:NodeLabeling[S],
                                 stateList:IndexedSeq[S],
                                 sb:StringBuilder):Unit = {
    var edge:Int = 0
    val stateCount = stateList.length

    // Add the epsilon hyperedges
    for(fromStateI <- 0 until stateCount) {
      val fromState = stateList(fromStateI)

      for (targetSet <- eHyperedgeTargets(fromState)) {
        val nodeName:String = "EHE" + edge
        sb ++= "\t"
        sb ++= nodeName
        sb ++= " [shape=point, margin=0, label=\"\", color=\"gray\" ];\n"
        sb ++= "\tV"
        sb ++= Integer.toString(fromStateI)
        sb ++= " -> "
        sb ++= nodeName
        sb ++= " [ label=\"\", color=\"gray\", arrowhead=\"none\" ];\n"

        for (target <- targetSet) {
          val targetI = stateList.indexOf(target)
          sb ++= "\t"
          sb ++= nodeName
          sb ++= " -> V"
          sb ++= Integer.toString(targetI)
          sb ++= " [ label=\"\", color=\"gray\" ];\n"
        }

        edge += 1
      }
    }

    sb.toString()
  }
}
