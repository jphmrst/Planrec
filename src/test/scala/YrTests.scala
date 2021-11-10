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

class YrTermsRules extends AnyFlatSpec with Matchers {

  import org.maraist.planrec.terms.Term.*
  import org.maraist.planrec.terms.Term.StringAsTerm

  import org.maraist.planrec.yr.Item
  import org.maraist.planrec.yr.HTN.*
  {
    import org.maraist.planrec.generaltest.RuleItemSamples1.*

    "HTN All rules (ordered)" `should` "have rule extension methods" in {
      (mm.initialItem) `should` be (mInitial)
    }

    "HTN All rules (unordered)" `should` "have rule extension methods" in {
      (rr.initialItem) `should` be (rInitial)
    }

    "HTN All rule R" `should` "allow traversal through its lattice" in {
      val rrInit = rr.initialItem
      (rrInit) `should` be (rInitial)
      (rrInit.triggers) `should` be (Set("N", "P"))

      val rrAfterN = rrInit("N")
      val rrAfterNP = rrAfterN.get("P")

      val rrAfterP = rrInit("P")
      val rrAfterPN = rrAfterP.get("N")

      (rrAfterPN.get) `should` be (rComplete)
      (rrAfterNP.get) `should` be (rComplete)
    }

    "HTN All rule T" `should` "allow traversal through its lattice" in {
      val ttInit = tt.initialItem
      (ttInit) `should` be (tInitial)
      (ttInit.triggers) `should` be (Set("A", "B"))

      val ttAfterA = ttInit("A")
      (ttAfterA.get) `should` be (tAfterA)
    }

    "HTN One rule N" `should` "allow traversal through its lattice" in {
      val nnInit = nn.initialItem
      (nnInit) `should` be (nInitial)
      (nnInit.isFinal) `should` be (false)
      (nnInit.triggers) `should` be (Set("A", "B"))

      val nnAfterA = nnInit("A").get
      (nnAfterA) `should` be (nComplete)
      (nnAfterA.isFinal) `should` be (true)

      val nnAfterB = nnInit("B").get
      (nnAfterB) `should` be (nComplete)
      (nnAfterB.isFinal) `should` be (true)
    }

    "HTN All rule S" `should` "allow traversal through its lattice" in {
      val ssInit = ss.initialItem
      (ssInit) `should` be (sInitial)
      (ssInit.isFinal) `should` be (false)
      (ssInit.triggers) `should` be (Set("N", "P"))

      val ssAfterN = ssInit("N")
      (ssAfterN.get) `should` be (sAfterN)
      (ssAfterN.get.isFinal) `should` be (false)
      (ssAfterN.get.triggers) `should` be (Set("P"))

      val ssAfterNPopt = ssAfterN.get("P")
      val ssAfterNP = ssAfterNPopt.get
      (ssAfterNP) `should` be (sAfterNP)
      (ssAfterNP.isFinal) `should` be (false)
      (ssAfterNP.triggers) `should` be (Set("Q"))

      val ssAfterNPS = ssAfterNP("Q")
      (ssAfterNPS.get) `should` be (sComplete)
      (ssAfterNPS.get.isFinal) `should` be (true)
      (ssAfterNPS.get.triggers) `should` be (Set())

      val ssAfterP = ssInit("P")
      (ssAfterP.get) `should` be (sAfterP)
      (ssAfterP.get.isFinal) `should` be (false)
      (ssAfterP.get.triggers) `should` be (Set("N", "Q"))

      val ssAfterPN = ssAfterP.get("N")
      (ssAfterPN.get) `should` be (sAfterNP)
      (ssAfterPN.get.isFinal) `should` be (false)
      (ssAfterPN.get.triggers) `should` be (Set("Q"))

      val ssAfterPNS = ssAfterPN.get("Q")
      (ssAfterPNS.get) `should` be (sComplete)
      (ssAfterPNS.get.isFinal) `should` be (true)
      (ssAfterPNS.get.triggers) `should` be (Set())

      val ssAfterPS = ssAfterP.get("Q")
      (ssAfterPS.get) `should` be (sAfterPS)
      (ssAfterPS.get.isFinal) `should` be (false)
      (ssAfterPS.get.triggers) `should` be (Set("N"))

      val ssAfterPSN = ssAfterPS.get("N")
      (ssAfterPSN.get) `should` be (sComplete)
      (ssAfterPSN.get.isFinal) `should` be (true)
      (ssAfterPSN.get.triggers) `should` be (Set())
    }


    "HTN One rules" `should` "have rule extension methods" in {
      (nn.initialItem) `should` be (nInitial)
    }

    "HTN Act rules" `should` "have rule extension methods" in {
      val aaInit = aa.initialItem
      (aaInit) `should` be (aBefore)
    }
  }
}