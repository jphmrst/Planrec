// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr
import scala.collection.mutable.WeakHashMap
import scala.collection.mutable.{HashSet, HashMap}

type Label[T, H, S] = Set[HState[T, H, S]] | Ind[T, H, S]

class Lower[T, H, S](
  val content: Label[T, H, S],
  val parent: HashSet[Lower[T, H, S]]
) {

  def push(item: Label[T, H, S], cache: Cache[T, H, S]): Lower[T, H, S]  =
    cache.map.get(item) match {
      case Some(result) => {
        result.parent += this
        result
      }
      case None => {
        val result = new Lower(item, HashSet[Lower[T, H, S]](this))
        result
      }
    }
}

class Cache[T, H, S] {
  val map = new HashMap[Label[T, H, S], Lower[T, H, S]]
}

class LowerGraph[T, H, S](
  val tops: WeakHashMap[Lower[T, H, S], Unit] =
    new WeakHashMap[Lower[T, H, S], Unit]()
)
