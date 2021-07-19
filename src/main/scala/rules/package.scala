// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec

/** Structure of plan library rules and items on those rules, and
  * implementations of particular rules.
  *
  * To add a new rule form:
  *
  *  1. Add a new class extending [[org.maraist.planrec.rules.RuleForm
  *     RuleForm]] to implement the rule itself.
  *
  *  2. Add a new class extending
  *     [[org.maraist.planrec.rules.RulePosition RulePosition]] to
  *     describe an item position within the new rule.
  *
  *  3. In a `given` declaration, add an
  *     [[org.maraist.planrec.rules.InitialItem InitialItem]] instance
  *     to calculate the initial item of a specific rule.
  *
  *  4. TODO --- how to derive items
  */
package object rules
