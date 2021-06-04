// Copyright (C) 2017, 2018 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.util

object Collections {

  def crossLists[X](iterables:List[Iterable[X]]): List[List[X]] = {
    iterables match {
      case List() => List()
      case (is:Iterable[X])::Nil => {
        val res = List.newBuilder[List[X]]
        for(i <- is) { res += List(i) }
        res.result()
      }
      case (is:Iterable[X])::(iss:List[Iterable[X]]) => {
        val res = List.newBuilder[List[X]]
        val crossNext = crossLists(iss)
        for(i <- is) {
          for (ii <- crossNext) {
            res += (i :: ii)
          }
        }
        res.result()
      }
    }
  }

  def hoistSome[A](l:List[Option[A]]):Option[List[A]] = l match {
    case (Some(x)::xs) => hoistSome(xs) match {
      case Some(ys) => Some(x::ys)
      case None => None
    }
    case _ => Some(Nil)
  }

  def hoistIterable[A](l:List[Iterable[A]]):Iterable[List[A]] = l match {
    case Nil => new Iterable[List[A]] {
      override def iterator:Iterator[List[A]] = Iterator[List[A]](Nil)
    }
    case (x::xs) => new Iterable[List[A]] {
      override def iterator:Iterator[List[A]] = {
        val hoistedXs = hoistIterable(xs)
        new Iterator[List[A]] {
          val headIterator:Iterator[A] = x.iterator
          var currentHead:A = headIterator.next()
          var tailIterator:Iterator[List[A]] = hoistedXs.iterator

          override def hasNext:Boolean =
            (tailIterator.hasNext
             || (headIterator.hasNext && !hoistedXs.isEmpty))
          override def next():List[A] = {
            if (!tailIterator.hasNext) {
              currentHead = headIterator.next()
              tailIterator = hoistedXs.iterator
            }
            val result = currentHead :: tailIterator.next()
            result
          }
        }
      }
    }
  }

  def concatIterators[A](iters:Iterator[Iterator[A]]):Iterator[A] = {
    val basis = for(iter <- iters if iter.hasNext) yield iter
    if (basis.hasNext) {
      new Iterator[A] {
        var current = basis.next()
        override def hasNext:Boolean = current.hasNext || basis.hasNext
        override def next():A = {
          if (!current.hasNext) current = basis.next()
          current.next()
        }
      }
    } else
      Iterator[A]()
  }
}
