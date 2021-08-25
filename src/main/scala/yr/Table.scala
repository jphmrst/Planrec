// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr.table
import scala.collection.mutable.Queue
import org.maraist.planrec.rules.HTNLib
import org.maraist.planrec.rules.HTN.HTNrule
import org.maraist.planrec.terms.Term.*
import org.maraist.planrec.rules.HTN.*
import org.maraist.fa.hyperedges.{HyperedgeNDFA,HyperedgeDFA}
import org.maraist.fa.hyperedges.impl.HashHyperedgeNDFABuilder

class Table[T, H, S](val library: HTNLib[T, H, S])(using TermImpl[T, H, S]) {

  // Commenting out while debugging Rule extension methods

  val dfa = {
    val nfa = HandleFinder.libToNFA(library)
    nfa.toDFA
  }
}



