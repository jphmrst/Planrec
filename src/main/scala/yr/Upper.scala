// Copyright (C) 2021, 2022 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr
import scala.collection.mutable.HashMap

/**
  * Result of traversing the upper graph for recognizing one
  * observation.
  */
type UpperTraversalResult[T, H, S, C, R] =
  (Option[Upper[T, H, S, C, R]], Option[H])

/**
  * Cache of upper graph nodes across recognition of one observation.
  *
  */
type UpperCache[T, H, S, C, R] = HashMap[Upper[T, H, S, C, R], Upper[T, H, S, C, R]]

/**
  * Combined cache upper and lower graph nodes across recognition of
  * one observation.
  *
  */
class Cache[T, H, S, C, R](
  val upper: UpperCache[T, H, S, C, R] = new UpperCache[T, H, S, C, R]
) {
  val lower = new LowerCache[T, H, S, C, R]
}

/**
  * Common superclass of upper graph nodes.
  */
sealed trait Upper[T, H, S, C, R] {
  def recognize(term: T, cache: Cache[T, H, S, C, R]):
      UpperTraversalResult[T, H, S, C, R]
}

/**
  * Upper graph node corresponding to the disjunction of several
  * possible parses.
  *
  * @param subs Subgraphs each representing a possible parse.
  */
class UpperOne[T, H, S, C, R](
  subs: List[UpperAll[T, H, S, C, R] | UpperLower[T, H, S, C, R]]
) extends Upper[T, H, S, C, R] {
  override def recognize(term: T, cache: Cache[T, H, S, C, R]):
      UpperTraversalResult[T, H, S, C, R] = ???
}

/**
  * Upper graph node corresponding to the synchronization point of
  * overlapping subparses.
  *
  * @param pars Upper graphs nodes corresponding to the partial parse
  * of each overlapping subgoal.
  * @param base Lower graph node of the overall parse state.
  */
class UpperAll[T, H, S, C, R](
  pars: List[Upper[T, H, S, C, R]],
  base: Lower[T, H, S, C, R]

) extends Upper[T, H, S, C, R] {

  override def recognize(term: T, cache: Cache[T, H, S, C, R]):
      UpperTraversalResult[T, H, S, C, R] = ???
}

/**
  * Upper graph node corresponding to one possible parse.
  *
  * @param stack Lower graph node corresponding at the top of the
  * parse graph.
  */
class UpperLower[T, H, S, C, R](stack: Lower[T, H, S, C, R])
extends Upper[T, H, S, C, R] {
  override def recognize(term: T, cache: Cache[T, H, S, C, R]):
      UpperTraversalResult[T, H, S, C, R] = ???
}
