/*
 * Copyright 2017 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.phase.jvm

/**
  * A common super-type for JVM types.
  *
  * A JVM type is either one of the primitive types or a reference type.
  */
sealed trait JvmType {
  /**
    * Returns the type descriptor of `this` Java name.
    */
  def toDescriptor: String = this match {
    case JvmType.PrimBool => "Z"
    case _ => ??? // TODO
  }
}

object JvmType {

  /**
    * Alias for the `java.lang.Object` type.
    */
  val Obj: JvmType.Reference = Reference(JvmName(List("java", "lang"), "Object"))

  /**
    * Represents the primitive boolean type.
    */
  case object PrimBool extends JvmType

  /**
    * Represents the primitive character type.
    */
  case object PrimChar extends JvmType

  /**
    * Represents the primitive byte type.
    */
  case object PrimByte extends JvmType

  /**
    * Represents the primitive short type.
    */
  case object PrimShort extends JvmType

  /**
    * Represents the primitive int type.
    */
  case object PrimInt extends JvmType

  /**
    * Represents the primitive long type.
    */
  case object PrimLong extends JvmType

  /**
    * Represents the primitive float type.
    */
  case object PrimFloat extends JvmType

  /**
    * Represents the primitive double type.
    */
  case object PrimDouble extends JvmType

  /**
    * Represents a reference type of the given `name`.
    */
  case class Reference(name: JvmName) extends JvmType

}
