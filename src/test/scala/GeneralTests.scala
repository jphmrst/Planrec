// Copyright (C) 2021 John Maraist
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.generaltest
import scala.language.adhocExtensions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.maraist.planrec.terms.Term.RenderCharAsTerm

class GeneralTests extends AnyFlatSpec with Matchers {

  import org.maraist.planrec.terms.Term.*
  import org.maraist.planrec.terms.Term.{StringAsTerm,CharAsTerm}

  "FullAll" `should` "translate to All with sequence of order pairs" in {
    import org.maraist.planrec.rules.{All,FullAll}
    import org.maraist.planrec.terms.Char.RenderCharAsTerm
    val full = FullAll('L', IndexedSeq('A', 'B', 'D'))
    full.order.size `should` be (2)
    full.order(0) `should` be ((0,1))
    full.order(1) `should` be ((1,2))
  }

  "String" `should` "have term extension methods" in {
    ("x" >?< "x") `should` be (true)
    ("x" >?< "y") `should` be (false)
  }
}
