/*
 * Copyright 2020 Azavea
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package geotrellis.server.ogc.utils

import com.azavea.maml.ast.Expression

import cats.syntax.option._
import scalaxb.DataRecord

import scala.xml.Elem

trait Implicits {
  implicit class DataRecordMethods[A](dataRecord: DataRecord[A]) {
    def toXML: Elem = ScalaxbUtils.toXML(dataRecord)
  }

  implicit class ExpressionMethods(expr: Expression) {
    def bindExtendedParameters(fun: Expression => Expression): Expression =
      bindExtendedParameters(fun.some)

    def bindExtendedParameters(fun: Option[Expression => Expression]): Expression =
      fun.fold(expr)(ExpressionUtils.bindExpression(expr, _))
  }
}

object Implicits extends Implicits
