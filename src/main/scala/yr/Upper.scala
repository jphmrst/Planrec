// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr

trait Upper[T, H, S]

class UpperAny[T, H, S](subs: List[UpperOne[T, H, S] | UpperLower[T, H, S]])
    extends Upper[T, H, S]

class UpperOne[T, H, S](pars: List[Upper[T, H, S]], base: Lower[T, H, S])
    extends Upper[T, H, S]

class UpperLower[T, H, S](stack: Lower[T, H, S]) extends Upper[T, H, S]
