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
import ca.uwaterloo.flix.language.ast.WatAst.Binop.{Ne, _}
import ca.uwaterloo.flix.language.ast.WatAst.Expr._
import ca.uwaterloo.flix.language.ast.WatAst.Func.Local.Loc
import ca.uwaterloo.flix.language.ast.WatAst.Func._
import ca.uwaterloo.flix.language.ast.WatAst.FuncSig.FSig
import ca.uwaterloo.flix.language.ast.WatAst.Instr._
import ca.uwaterloo.flix.language.ast.WatAst.Module.Mod1
import ca.uwaterloo.flix.language.ast.WatAst.Operation._
import ca.uwaterloo.flix.language.ast.WatAst.Param._
import ca.uwaterloo.flix.language.ast.WatAst.Result.Res
import ca.uwaterloo.flix.language.ast.WatAst._
import ca.uwaterloo.flix.language.ast.WatAst.Sign._
import ca.uwaterloo.flix.language.ast.WatAst.Unop.{Eqz, _}
import ca.uwaterloo.flix.language.ast.WatAst.Value._
import ca.uwaterloo.flix.language.ast.WatAst.ValType._

sealed trait WatAst {
  override def toString: String = this match {
    case Root(_, module) => module.toString
    case a: Module => a.toString
    case a: Unop => a.toString
    case a: WatAst.Binop => a.toString
    case a: Sign => a.toString
    case Offset(amt) => "offset=" + amt
    case Align(amt) => "align=(" + amt + ")"
    case a: FuncSig => a.toString
    case a: Param => a.toString
    case a: Func => a.toString
    case a: Local => a.toString
    case a: Value => a.toString
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
      case Mod1(name, funcs) => f"(module ${showOpt(name)}" + funcs.mkString("\n") + "\n(export \"main\" (func $main))\n)\n"
    }
  }

  object Module {

    case class Mod1(name: Option[Id], funcs: List[Func]) extends Module

  }

  type Typeidx = Int
  type Funcidx = Int
  type Tableidx = Int
  type Memidx = Int
  type Globalidx = Int
  type Localidx = Int
  type Labelidx = Int

  case class Id(s: String) {
    override def toString: String = "$" + s
  }


  sealed trait Unop extends WatAst {
    override def toString: String = this match {
      case Eqz => "eqz"
    }
  }

  object Unop {

    // See https://github.com/sunfishcode/wasm-reference-manual/blob/master/WebAssembly.md#integer-shift-left

    case object Eqz extends Unop

  }

  object Binop {
    case object Shl_s extends Binop

    case object Shl_u extends Binop

    case object Shr_s extends Binop

    case object Shr_u extends Binop

    case object Add extends Binop

    case object Sub extends Binop

    case object Mul extends Binop

    case object Div_u extends Binop

    case object Div_s extends Binop

    case object Rem_u extends Binop

    case object Rem_s extends Binop

    case object Eq extends Binop
    case object Ne extends Binop
    case object Gt_s extends Binop
    case object Gt_u extends Binop
    case object Lt_s extends Binop
    case object Lt_u extends Binop
    case object Le_s extends Binop
    case object Le_u extends Binop
    case object Ge_s extends Binop
    case object Ge_u extends Binop
    case object And extends Binop
    case object Or extends Binop
    case object Xor extends Binop

  }

  sealed trait Binop extends WatAst {
    override def toString: String = this match {
      case Add => "add"
      case Sub => "sub"
      case Mul => "mul"
      case Div_u => "div_u"
      case Div_s => "div_s"
      case Rem_u => "rem_u"
      case Rem_s => "rem_s"
      case Eq => "eq"
      case Ne => "ne"
      case Lt_s => "lt_s"
      case Lt_u => "lt_u"
      case Gt_s => "gt_s"
      case Gt_u => "gt_u"
      case Le_s => "le_s"
      case Le_u => "le_u"
      case Ge_s => "ge_s"
      case Ge_u => "ge_u"
      case And  => "and"
      case Or => "or"
      case Xor => "xor"
      case Shr_s => "shr_s"
      case Shr_u => "shr_u"
      case Shl_s => "shl_s"
      case Shl_u => "shl_u"
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

    case class FSig(tpe: Option[ValType], params: List[Param], result: Result) extends FuncSig

  }

  sealed trait Param extends WatAst {
    override def toString: String = this match {
      case ParamT(name, tpe) => s"(param $$$name $tpe)"
      case ParamN(tpes) => s"(param ${tpes.mkString(" ")})"
    }
  }

  object Param {

    case class ParamT(name: String, tpe: ValType) extends Param

    case class ParamN(tpes: List[ValType]) extends Param

  }

  sealed trait Result {
    override def toString: String = this match {
      case Res(tpe) => s"(result $tpe)"
    }
  }

  object Result {

    case class Res(tpe: ValType) extends Result

  }

  sealed trait Func extends WatAst {
    override def toString: String = this match {
      case LocalFunc(name, sig, locals, instrs) => "(func " + showOpt(name) + sig + locals.mkString(" ") + "\n" + instrs.mkString("\n") + ")"

    }
  }


  object Func {

    case class LocalFunc(name: Option[Id], sig: FuncSig, locals: List[Local], instrs: List[Instr]) extends Func


    sealed trait Local extends WatAst {
      override def toString: String = this match {
        case Loc(name, tpe) => "(local " + showOpt(name) + tpe.toString + ")"
      }
    }

    object Local {

      case class Loc(name: Option[String], tpe: ValType) extends Local

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

  sealed trait ValType {
    override def toString: String = this match {
      case I32 => "i32"
      case I64 => "i64"
      case F32 => "f32"
      case F64 => "f64"
    }
  }

  object ValType {

    case object I32 extends ValType

    case object I64 extends ValType

    case object F32 extends ValType

    case object F64 extends ValType

  }

  case class ResultType(tpe: List[ValType]) {
    override def toString: String = f"(result ${tpe.head.toString})"
  }

  sealed trait Instr extends WatAst {
    override def toString: String = "(" + (this match {
      case Nop => "nop"
      case Unreachable => "unreachable"
      case Block(tpe, lab, instrs) => f"block ${showOpt(lab)}$tpe ${instrs.mkString("\n")}"
      case Loop(tpe, lab, instrs) => f"loop ${showOpt(lab)}$tpe (${instrs.mkString("\n")}) end"
      case IfElse(tpe, lab, test, tBranch, fBranch) => f"if $tpe ${test.mkString("\n")} (then ${tBranch.mkString("\n")}) (else ${fBranch.mkString("\n")})"
      case Br(idx) => f"br $idx"
      case Br_if(idx) => f"br_if $idx"
      case Br_table(idx) => f"br_table $idx"
      case Return => "return"
      case Call(idx) => f"call $idx"
      case Call_indirect(idx) => f"call_indirect $idx"
      case a => a.toString
    }) + ")"
  }


  object Instr {

    case object Nop extends Instr

    case object Unreachable extends Instr

    case class Block(tpe: ResultType, label: Option[Id], instrs: List[Instr]) extends Instr

    case class Loop(tpe: ResultType, label: Option[Id], instrs: List[Instr]) extends Instr

    case class IfElse(tpe: ResultType, label: Option[Id], test: List[Instr], tBranch: List[Instr], fBranch: List[Instr]) extends Instr

    case class Br(idx: Labelidx) extends Instr

    case class Br_if(idx: Labelidx) extends Instr

    case class Br_table(idx: Labelidx) extends Instr

    case object Return extends Instr

    case class Call(idx: Id) extends Instr

    case class Call_indirect(idx: Typeidx) extends Instr

  }

  sealed trait Expr extends Instr {
    override def toString: String = this match {
      case Op(o, exprs) => exprs.mkString("\n") + "\n" + o.toString
    }
  }

  object Expr {

    case class Op(o: Operation, exprs: List[WatAst]) extends Expr

  }

  sealed trait Operation extends Instr {
    override def toString: String = "(" + (this match {
      case Bop(tpe, o) => tpe.toString + "." + o.toString
      case Const(tpe, v) => tpe.toString + ".const " + v.toString
      case SetLocal(i) => "set_local " + i
      case GetLocal(i) => "get_local " + i
    }) + ")"
  }

  object Operation {


    case class Bop(tpe: ValType, o: Binop) extends Operation

    case class Const(tpe: ValType, v: Value) extends Operation

    case class SetLocal(imm: Int) extends Operation

    case class GetLocal(imm: Int) extends Operation

  }

  def showOpt[A](opt: Option[A]): String = opt match {
    case Some(x) => x.toString + " "
    case None => ""
  }
}
