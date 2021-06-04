// Copyright (C) 2018 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.util

/** Trait which guarantees a unique hash code (across all classes
 *  implementing this trait) for instances. */
trait UniqueHashCode {
  private val uniqHash:Int = UniqueHashCode.next
  /** The hash code is a unique assigned serial number. */
  override final def hashCode():Int = uniqHash
  /** The agreement of equals and hashCode means that all objects are
   * unequal to this one. */
  override final def equals(o:Any):Boolean = false
}
/** This object generates unique hash code numbers for classes
 *  implementing the
 *  {@link org.maraist.util.UniqueHashCode UniqueHashCode} trait.
 */
private[util] object UniqueHashCode {
  private var nextHashCode:Int = 1

  def next:Int = {
    val result:Int = nextHashCode
    if (nextHashCode == Int.MaxValue) {
      nextHashCode = 1
    } else {
      nextHashCode += 1
    }
    result
  }
}
