/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.model

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.util.{Random,Sorting}
import scala.util.Random
import scala.math
import scala.util.Sorting
import cc.factorie.la._
import cc.factorie.util.Substitutions
import java.io._
import cc.factorie.variable.{TensorVar, Var, DiscreteVar}
import cc.factorie.model

abstract class Template4[N1<:Var,N2<:Var,N3<:Var,N4<:Var](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Family4[N1,N2,N3,N4] with Template {
  val neighborClass1 = nm1.erasure
  val neighborClass2 = nm2.erasure
  val neighborClass3 = nm3.erasure
  val neighborClass4 = nm4.erasure
  def neighborClasses: Seq[Class[_]] = Seq(neighborClass1, neighborClass2, neighborClass3, neighborClass4)

  final override def addFactors(v:Var, result:scala.collection.mutable.Set[model.Factor]): Unit = {
    if (neighborClass1.isAssignableFrom(v.getClass)) result ++= unroll1(v.asInstanceOf[N1])
    if (neighborClass2.isAssignableFrom(v.getClass)) result ++= unroll2(v.asInstanceOf[N2])
    if (neighborClass3.isAssignableFrom(v.getClass)) result ++= unroll3(v.asInstanceOf[N3])
    if (neighborClass4.isAssignableFrom(v.getClass)) result ++= unroll4(v.asInstanceOf[N4])
    unroll(v) match { case fs:IterableSingleFactor[Factor] => result += fs.factor; case Nil => {}; case fs => result ++= fs }
  }
  //* Override this method if you want to re-capture old unrollCascade functionality. */ 
  def unroll(v:Var): Iterable[Factor] = Nil
  def unroll1(v:N1): Iterable[FactorType]
  def unroll2(v:N2): Iterable[FactorType]
  def unroll3(v:N3): Iterable[FactorType]
  def unroll4(v:N4): Iterable[FactorType]
  def limitDiscreteValuesAsIn(vars:Iterable[DiscreteVar]): Unit = throw new Error("Not yet implemented.") 
}

abstract class TupleTemplate4[N1<:Var:Manifest,N2<:Var:Manifest,N3<:Var:Manifest,N4<:Var:Manifest] extends Template4[N1,N2,N3,N4] with TupleFamily4[N1,N2,N3,N4]
abstract class TupleTemplateWithStatistics4[N1<:Var:Manifest,N2<:Var:Manifest,N3<:Var:Manifest,N4<:Var:Manifest] extends Template4[N1,N2,N3,N4] with TupleFamilyWithStatistics4[N1,N2,N3,N4]
abstract class TensorTemplate4[N1<:Var:Manifest,N2<:Var:Manifest,N3<:Var:Manifest,N4<:Var:Manifest] extends Template4[N1,N2,N3,N4] with TensorFamily4[N1,N2,N3,N4]
abstract class TensorTemplateWithStatistics4[N1<:TensorVar:Manifest,N2<:TensorVar:Manifest,N3<:TensorVar:Manifest,N4<:TensorVar:Manifest] extends Template4[N1,N2,N3,N4] with TensorFamilyWithStatistics4[N1,N2,N3,N4]
abstract class DotTemplate4[N1<:Var:Manifest,N2<:Var:Manifest,N3<:Var:Manifest,N4<:Var:Manifest] extends Template4[N1,N2,N3,N4] with DotFamily4[N1,N2,N3,N4]
abstract class DotTemplateWithStatistics4[N1<:TensorVar:Manifest,N2<:TensorVar:Manifest,N3<:TensorVar:Manifest,N4<:TensorVar:Manifest] extends Template4[N1,N2,N3,N4] with DotFamilyWithStatistics4[N1,N2,N3,N4]
