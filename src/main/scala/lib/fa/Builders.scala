// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa
import scala.collection.mutable.{Builder, HashMap, HashSet}

object Builders {
  case class AddState[S,T](state: S)
  case class RemoveState[S,T](state: S)
  case class RemoveFinalState[S,T](state: S)
  case class RemoveTransition[S,T](state1: S, trans: T, state2: S)
  type AnyBuilders[S,T] =
    AddState[S,T] | RemoveState[S,T] | RemoveFinalState[S,T] | RemoveTransition[S,T]

  case class AddFinalState[S,T](state: S)
  case class AddTransition[S,T](state1: S, trans: T, state2: S)
  type NonProbBuilders[S,T] = AddFinalState[S,T] | AddTransition[S,T]

  case class SetInitialState[S](state: S)
  type SingleInitialStateBuilders[S] = SetInitialState[S]

  case class AddInitialState[S](state: S)
  case class RemoveInitialState[S](state: S)
  type MultipleInitialStateBuilders[S] = AddInitialState[S] | RemoveInitialState[S]

  case class AddETransition[S,T](state1: S, state2: S)
  case class RemoveETransition[S,T](state1: S, state2: S)
  type NDFABuilders[S,T] = AddETransition[S,T] | RemoveETransition[S,T]

  case class AddProbFinalState[S,T](state: S, prob: Double)
  case class AddProbTransition[S,T](state1: S, trans: T, state2: S, prob: Double)
  case class AddProbETransition[S,T](state1: S, state2: S, prob: Double)
  case class RemoveProbETransition[S,T](state1: S, state2: S, prob: Double)
  type ProbBuilders[S,T] = AddProbFinalState[S,T] | AddProbTransition[S,T] | AddProbETransition[S,T] | RemoveProbETransition[S,T]

  case class AddEHyperedge[S](s: S, ss: Set[S])
  type EHyperedgeBuilders[S] = AddEHyperedge[S]

  type DFAelements[S, T] =
    SingleInitialStateBuilders[S] | NonProbBuilders[S,T] | AnyBuilders[S,T]
  type NDFAelements[S, T] =
    MultipleInitialStateBuilders[S] | NDFABuilders[S,T] | NonProbBuilders[S,T]
     | AnyBuilders[S,T]
  type PFAelements[S, T] =
    SingleInitialStateBuilders[S] | ProbBuilders[S,T] | AnyBuilders[S,T]
  type HyperedgeDFAelements[S, T]  = DFAelements[S, T]  | EHyperedgeBuilders[S]
  type HyperedgeNDFAelements[S, T] = NDFAelements[S, T] | EHyperedgeBuilders[S]

  trait HasBuilder[Setter[_], Mapper[_,_], Elements[_,_], Res[_,_]] {
    def build[S,T](): Builder[Elements[S,T], Res[S,T]]
  }
  trait HasBuilderWithInit[Setter[_], Mapper[_,_], Elements[_,_], Res[_,_]] {
    def build[S,T](init: S): Builder[Elements[S,T], Res[S,T]]
  }

  given HasBuilderWithInit[HashSet, HashMap, DFAelements, DFA] with {
    override def build[S,T](init: S): Builder[DFAelements[S, T], DFA[S, T]] =
        new HashDFABuilder[S, T](init)
  }

  given HasBuilder[
    HashSet, HashMap, NDFAelements, [X,Y] =>> NDFA[X, Y, IndexedDFA[Set[X], Y]]
  ] with {
    override def build[S,T]():
      Builder[NDFAelements[S, T], NDFA[S, T, IndexedDFA[Set[S], T]]] =
        new HashNDFABuilder[S, T]
  }

  given HasBuilder[HashSet, HashMap, PFAelements, PFA] with {
    override def build[S,T](): Builder[PFAelements[S, T], PFA[S, T]] =
        new HashPFABuilder[S, T]
  }

  given HasBuilderWithInit[
    HashSet, HashMap, HyperedgeDFAelements, HyperedgeDFA
  ] with {
    override def build[S,T](init: S): Builder[HyperedgeDFAelements[S, T],
                                              HyperedgeDFA[S, T]] =
      new HashHyperedgeDFABuilder[S, T](init)
  }

  given HasBuilder[
    HashSet, HashMap, HyperedgeNDFAelements,
    [X,Y] =>> HyperedgeNDFA[X, Y, IndexedHyperedgeDFA[Set[X], Y]]
  ] with {
    override def build[S,T]():
        Builder[HyperedgeNDFAelements[S, T],
                HyperedgeNDFA[S, T, IndexedHyperedgeDFA[Set[S], T]]] =
        new HashHyperedgeNDFABuilder[S, T]
  }
}
