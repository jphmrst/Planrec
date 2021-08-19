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

class GeneralTests extends AnyFlatSpec with Matchers {

  import org.maraist.planrec.terms.Term.*
  import org.maraist.planrec.terms.Term.StringAsTerm

  "String" `should` "have term extension methods" in {
    ("x" >?< "x") `should` be (true)
    ("x" >?< "y") `should` be (false)
  }
}