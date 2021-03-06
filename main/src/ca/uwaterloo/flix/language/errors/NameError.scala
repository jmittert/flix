/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{Source, SourceLocation}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for naming errors.
  */
sealed trait NameError extends CompilationError {
  val kind = "Name Error"
}

object NameError {

  /**
    * An error raised to indicate that the given definition `name` is defined multiple times.
    *
    * @param name the name.
    * @param loc1 the location of the first definition.
    * @param loc2 the location of the second definition.
    */
  case class DuplicateDefinition(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    val source: Source = loc1.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Duplicate definition of '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first definition was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second definition was here.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Remove or rename one of the definitions." << NewLine
    }
  }

  /**
    * An error raised to indicate that an index is defined multiple times for the same relation/lattice.
    *
    * @param name the name.
    * @param loc1 the location of the first definition.
    * @param loc2 the location of the second definition.
    */
  case class DuplicateIndex(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    val source: Source = loc1.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Duplicate index declaration for table '" << Red(name) << "'." << NewLine
      vt << Code(loc1, "the first declaration was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second declaration was here.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Remove one of the two index declarations." << NewLine
    }
  }

  /**
    * An error raised to indicate that an ambiguous constructor was not found.
    *
    * @param className the class name.
    * @param arity     the expected arity.
    * @param loc       the location of the class name.
    */
  case class AmbiguousNativeConstructor(className: String, arity: Int, loc: SourceLocation) extends NameError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Ambiguous constructor for class '" << Cyan(className) << "' with expected arity " << arity << "." << NewLine
      vt << NewLine
      vt << Code(loc, "ambiguous constructor.") << NewLine
    }
  }

  /**
    * An error raised to indicate that an ambiguous method name was not found.
    *
    * @param className the class name.
    * @param fieldName the method name.
    * @param arity     the expected arity.
    * @param loc       the location of the class name.
    */
  case class AmbiguousNativeMethod(className: String, fieldName: String, arity: Int, loc: SourceLocation) extends NameError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Ambiguous method '" << Red(fieldName) << "' in class '" << Cyan(className) << "' with expected arity " << arity << "." << NewLine
      vt << NewLine
      vt << Code(loc, "ambiguous method.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the class name was not found.
    *
    * @param name the class name.
    * @param loc  the location of the class name.
    */
  case class UndefinedNativeClass(name: String, loc: SourceLocation) extends NameError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined class '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "undefined class.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the constructor was not found.
    *
    * @param className the class name.
    * @param arity     the expected arity.
    * @param loc       the location of the method name.
    */
  case class UndefinedNativeConstructor(className: String, arity: Int, loc: SourceLocation) extends NameError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined constructor for class '" << Cyan(className) << "' with expected arity " << arity << "." << NewLine
      vt << NewLine
      vt << Code(loc, "undefined constructor.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the field name was not found.
    *
    * @param className the class name.
    * @param fieldName the field name.
    * @param loc       the location of the field name.
    */
  case class UndefinedNativeField(className: String, fieldName: String, loc: SourceLocation) extends NameError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined field '" << Red(fieldName) << "' in class '" << Cyan(className) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "undefined field.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the method name was not found.
    *
    * @param className the class name.
    * @param fieldName the method name.
    * @param arity     the expected arity.
    * @param loc       the location of the method name.
    */
  case class UndefinedNativeMethod(className: String, fieldName: String, arity: Int, loc: SourceLocation) extends NameError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined method '" << Red(fieldName) << "' in class '" << Cyan(className) << "' with expected arity " << arity << "." << NewLine
      vt << NewLine
      vt << Code(loc, "undefined method.") << NewLine
    }
  }

}
