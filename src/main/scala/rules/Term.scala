// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.rules
import scala.annotation.targetName

trait TermImplementation[T]

object TermImplementation {
  given allTermImplementation[T]: TermImplementation[T] =
    new TermImplementation[T]() { }
}

transparent trait Term[TermType] {
  @targetName("unifiesWith")
  def >?< (t2: TermType): Boolean
  def >><< (t2: TermType): Option[TermType]
}
