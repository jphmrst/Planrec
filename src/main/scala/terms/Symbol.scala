// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.terms

object Symbol {
  given SymbolAsTerm: TermImpl[Symbol, Symbol, Unit] with
    override def unifiable(t1: Symbol, t2: Symbol): Boolean = t1 == t2
    def unifyTerms(t1: Symbol, t2: Symbol): Option[Symbol] =
      if unifiable(t1, t2) then Some(t1) else None
    def head(s: Symbol): Symbol = s
    def getUnifier(t1: Symbol, t2: Symbol): Option[Unit] =
      if t1 == t2 then Some(()) else None
    def substitute(t: Symbol, s: Unit): Symbol = t
}
