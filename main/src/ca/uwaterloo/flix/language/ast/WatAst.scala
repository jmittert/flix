/*
 * Copyright 2017 Jason Mittertreiner
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


package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast
import ca.uwaterloo.flix.language.ast.WatAst.Binop._
import ca.uwaterloo.flix.language.ast.WatAst.Expr._
import ca.uwaterloo.flix.language.ast.WatAst.Func.Local.Loc
import ca.uwaterloo.flix.language.ast.WatAst.Func._
import ca.uwaterloo.flix.language.ast.WatAst.FuncSig.FSig
import ca.uwaterloo.flix.language.ast.WatAst.Module.Mod1
import ca.uwaterloo.flix.language.ast.WatAst.Operation._
import ca.uwaterloo.flix.language.ast.WatAst.Param._
import ca.uwaterloo.flix.language.ast.WatAst.Relop._
import ca.uwaterloo.flix.language.ast.WatAst.Result.Res
import ca.uwaterloo.flix.language.ast.WatAst._
import ca.uwaterloo.flix.language.ast.WatAst.Sign._
import ca.uwaterloo.flix.language.ast.WatAst.Unop._
import ca.uwaterloo.flix.language.ast.WatAst.Value._
import ca.uwaterloo.flix.language.ast.WatAst.WType._

sealed trait WatAst {
  override def toString: String = this match {
    case Root(_, module) => module.toString
    case a: Module => a.toString
    case a: Unop => a.toString
    case a: WatAst.Binop => a.toString
    case a: Relop => a.toString
    case a: Sign => a.toString
    case Offset(amt) => "offset=" + amt
    case Align(amt) => "align=(" + amt + ")"
    case a: FuncSig => a.toString
    case a: Param => a.toString
    case a: Func => a.toString
    case a: Local => a.toString
    case a: Value => a.toString
    case a: WType => a.toString
    case a: Instr => a.toString
  }
}

/**
  * An AST for Web Assembly based off of the grammar here:
  * https://github.com/WebAssembly/spec/blob/master/interpreter/README.md#s-expression-syntax
  */
object WatAst {

  case class Root(time: ast.Time, module: WatAst.Module) extends WatAst

  sealed trait Module extends WatAst {
    override def toString: String = this match {
      case Mod1(name, funcs) => "(module " + funcs.mkString("\n") +  "\n(export \"main\" (func $main))\n)\n"
    }
  }

  object Module {

    case class Mod1(name: Option[String], funcs: List[Func]) extends Module

  }

  sealed trait Unop extends WatAst {
    override def toString: String = this match {
      case Shrs => "shr_s"
      case Shru => "shr_u"
      case Shls => "shl_s"
      case Shlu => "shl_u"
      case Eqz => "eqz"
    }
  }
  object Unop {

    // See https://github.com/sunfishcode/wasm-reference-manual/blob/master/WebAssembly.md#integer-shift-left
    case object Shls extends Unop
    case object Shlu extends Unop
    case object Shrs extends Unop
    case object Shru extends Unop
    case object Eqz extends Unop
  }

  object Binop {

    case object Add extends Binop

    case object Sub extends Binop

    case object Mul extends Binop

    case object Div_u extends Binop

    case object Div_s extends Binop

  }

  sealed trait Binop extends WatAst {
    override def toString: String = this match {
      case Add => "add"
      case Sub => "sub"
      case Mul => "mul"
      case Div_u => "div_u"
      case Div_s => "div_s"
    }
  }

  object Relop {

    case object Eq extends Relop

    case object Ne extends Relop

    case object Lt extends Relop

  }

  sealed trait Relop extends WatAst {
    override def toString: String = this match {
      case Eq => "eq"
      case Ne => "ne"
      case Lt => "lt"
    }
  }

  sealed trait Sign extends WatAst {
    override def toString: String = this match {
      case Signed => "s"
      case Unsigned => "u"
    }
  }

  object Sign {
    case object Signed extends Sign
    case object Unsigned extends Sign
  }

  case class Offset(amt: Int) extends WatAst
  case class Align(amt: Int) extends WatAst


  sealed trait FuncSig extends WatAst {
    override def toString: String = this match {
      case FSig(name, params, result) => (name match {
        case Some(x) => s"(type $x ) "
        case None => ""
      }) + params.mkString(" ") + result
    }
  }

  object FuncSig {
    case class FSig(tpe: Option[WType], params: List[Param], result: Result) extends FuncSig
  }

  sealed trait Param extends WatAst {
    override def toString: String = this match {
      case ParamT(name, tpe) => s"(param $$$name $tpe)"
      case ParamN(tpes) => s"(param ${tpes.mkString(" ")})"
    }
  }

  object Param {
    case class ParamT(name: String, tpe: WType) extends Param
    case class ParamN(tpes: List[WType]) extends Param
  }

  sealed trait Result {
    override def toString: String = this match {
      case Res(tpe) => s"(result $tpe)"
    }
  }

  object Result {
    case class Res(tpe: WType) extends Result
  }

  sealed trait Func extends WatAst {
    override def toString: String = this match {
      case LocalFunc(name, sig, locals, instrs) => "(func " + showOptName(name) + sig + locals.mkString(" ") + "\n" + instrs.mkString("\n") + ")"

    }
  }


  object Func {

    case class LocalFunc(name: Option[String], sig: FuncSig, locals: List[Local], instrs: List[Instr]) extends Func


    sealed trait Local extends WatAst {
      override def toString: String = this match {
        case Loc(name, tpe) => "(local " + showOptName(name) + tpe.toString + ")"
      }
    }

    object Local {

      case class Loc(name: Option[String], tpe: WType) extends Local

    }

  }

  sealed trait Value extends WatAst {
    override def toString: String = this match {
      case Float32(lit) => lit.toString
      case Float64(lit) => lit.toString
      case Int32(lit) => lit.toString
      case Int64(lit) => lit.toString
    }
  }

  object Value {

    case class Float32(lit: scala.Float) extends Value

    case class Float64(lit: scala.Double) extends Value

    case class Int32(lit: scala.Int) extends Value

    case class Int64(lit: scala.Long) extends Value

  }

  sealed trait WType extends WatAst {
    override def toString: String = this match {
      case I32 => "i32"
      case I64 => "i64"
      case F32 => "f32"
      case F64 => "f64"
    }
  }

  object WType {

    case object I32 extends WType

    case object I64 extends WType

    case object F32 extends WType

    case object F64 extends WType

  }

  sealed trait Instr extends WatAst

  sealed trait Expr extends Instr {
    override def toString: String = this match {
      case Op(o, exprs) => exprs.mkString("\n") + "\n" + o.toString
      case Exprs(exprs) => exprs.mkString("\n")
    }
  }

  object Expr {

    case class Op(o: Operation, exprs: List[WatAst]) extends Expr
    case class Exprs(lst: List[WatAst]) extends Expr

  }

  sealed trait Operation extends Instr {
    override def toString: String = this match {
      case Nop => "nop"
      case Bop(tpe, o) => tpe.toString + "." + o.toString
      case Const(tpe, v) => tpe.toString + ".const " + v.toString
      case SetLocal(i) => "set_local " + i
      case GetLocal(i) => "get_local " + i
    }
  }

  object Operation {

    case object Nop extends Operation

    case class Bop(tpe: WType, o: Binop) extends Operation

    case class Const(tpe: WType, v: Value) extends Operation

    case class SetLocal(imm: Int) extends Operation

    case class GetLocal(imm: Int) extends Operation

  }

  def showOpt[A](opt: Option[A]): String = opt match {
    case Some(x) => x.toString + " "
    case None => ""
  }

  def showOptName[A](opt: Option[A]): String = opt match {
    case Some(x) => "$" + x.toString + " "
    case None => ""
  }
}
