/*
 * Copyright 2015-2016 Ming-Ho Yee
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.BinaryOperator._
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression._
import ca.uwaterloo.flix.language.ast.WatAst.Binop._
import ca.uwaterloo.flix.language.ast.WatAst.Expr._
import ca.uwaterloo.flix.language.ast.WatAst.Func.Local.Loc
import ca.uwaterloo.flix.language.ast.WatAst._
import ca.uwaterloo.flix.language.ast.WatAst.FuncSig.FSig
import ca.uwaterloo.flix.language.ast.WatAst.Instr._
import ca.uwaterloo.flix.language.ast.WatAst.Operation._
import ca.uwaterloo.flix.language.ast.WatAst.Param.ParamT
import ca.uwaterloo.flix.language.ast.WatAst.Result.Res
import ca.uwaterloo.flix.language.ast.WatAst.Value.{Float32, Float64, Int32, Int64}
import ca.uwaterloo.flix.language.ast.WatAst.ValType.{F32, F64, I32, I64}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object WatGen extends Phase[ExecutableAst.Root, WatAst.Root] {

  def run(root: ExecutableAst.Root)(implicit flix: Flix): Validation[WatAst.Root, CompilationError] = {
    // Put GenSym into the implicit scope.

    val time = root.time
    WatAst.Root(time, WatAst.Module.Mod1(Some(Id("main")), defsToWat(root.defs))).toSuccess
  }

  def defsToWat(defs: Map[Symbol.DefnSym, ExecutableAst.Def]): List[WatAst.Func] = defs.toList.map(toWat)


  def toWat(sast: (Symbol.DefnSym, ExecutableAst.Def)): WatAst.Func = {
    val defn = sast._2
    val wasmParams = defn.formals.toList.map(f => ParamT(f.sym.text, I32))
    WatAst.Func.LocalFunc(
      Some(Id(sast._1.name)),
      FSig(None, wasmParams, Res(I32)),
      List.fill(countLocals(defn.exp))(Loc(None, I32)),
      List(Expression.toWat(sast._2.exp)))
  }

  object Expression {
    def toWat(sast: ExecutableAst.Expression): Instr = sast match {
      case ExecutableAst.Expression.Unit => Const(I32, Int32(0))
      case ExecutableAst.Expression.True => Const(I32, Int32(1))
      case ExecutableAst.Expression.False => Const(I32, Int32(0))
      case ExecutableAst.Expression.Char(lit) => Const(I32, Int32(lit))
      case ExecutableAst.Expression.Float32(lit) => Const(F32, Float32(lit))
      case ExecutableAst.Expression.Float64(lit) => Const(F64, Float64(lit))
      case ExecutableAst.Expression.Int8(lit) => Const(I32, Int32(lit))
      case ExecutableAst.Expression.Int16(lit) => Const(I32, Int32(lit))
      case ExecutableAst.Expression.Int32(lit) => Const(I32, Int32(lit))
      case ExecutableAst.Expression.Int64(lit) => Const(I64, Int64(lit))
      case ExecutableAst.Expression.BigInt(lit) => ???
      case ExecutableAst.Expression.Str(lit) => Const(I32, Int32(0))
      case ExecutableAst.Expression.Var(sym, tpe, loc) => GetLocal(sym.getStackOffset)
      case ExecutableAst.Expression.Closure(exp, freeVars, fnType, tpe, loc) => Const(I32, Int32(0))
      case ExecutableAst.Expression.ApplyClo(exp, args, tpe, loc) => Const(I32, Int32(0))
      case ExecutableAst.Expression.ApplyDef(name, args, tpe, loc) => Block(ResultType(List(I32)), None,
        args.map(toWat) ::: List(Call(Id(name.name))))
      case ExecutableAst.Expression.ApplyCloTail(exp, args, tpe, loc) => ???
      case ExecutableAst.Expression.ApplyDefTail(name, args, tpe, loc) => ???
      case ExecutableAst.Expression.ApplySelfTail(name, formals, actuals, tpe, loc) => ???
      case ExecutableAst.Expression.ApplyHook(hook, args, tpe, loc) => Const(I32, Int32(0))
      case ExecutableAst.Expression.Unary(sop, op, exp, tpe, loc) => ???
      case ExecutableAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) => op match {
        case o: ArithmeticOperator => o match {
          case Plus => Op(Bop(I32, Add), List(toWat(exp1), toWat(exp2)))
          case Minus => Op(Bop(I32, Sub), List(toWat(exp1), toWat(exp2)))
          case Times => Op(Bop(I32, Mul), List(toWat(exp1), toWat(exp2)))
          case Divide => Op(Bop(I32, Div_s), List(toWat(exp1), toWat(exp2)))
          case Modulo => Op(Bop(I32, Rem_s), List(toWat(exp1), toWat(exp2)))
          case Exponentiate => ???
        }
        case o: ComparisonOperator => o match {
          case _: EqualityOperator => Op(Bop(I32, Eq), List(toWat(exp1), toWat(exp2)))
          case Less => Op(Bop(I32, Lt_s), List(toWat(exp1), toWat(exp2)))
          case LessEqual => Op(Bop(I32, Le_s), List(toWat(exp1), toWat(exp2)))
          case Greater => Op(Bop(I32, Gt_s), List(toWat(exp1), toWat(exp2)))
          case GreaterEqual => Op(Bop(I32, Ge_s), List(toWat(exp1), toWat(exp2)))
        }
        case o: LogicalOperator => o match {
          case LogicalAnd => Op(Bop(I32, And), List(toWat(exp1), toWat(exp2)))
          case LogicalOr => Op(Bop(I32, Or), List(toWat(exp1), toWat(exp2)))
        }
        case o: BitwiseOperator => o match {
          case BitwiseAnd => Op(Bop(I32, And), List(toWat(exp1), toWat(exp2)))
          case BitwiseOr => Op(Bop(I32, Or), List(toWat(exp1), toWat(exp2)))
          case BitwiseXor => Op(Bop(I32, Xor), List(toWat(exp1), toWat(exp2)))
          case BitwiseLeftShift => Op(Bop(I32, Shl_u), List(toWat(exp1), toWat(exp2)))
          case BitwiseRightShift => Op(Bop(I32, Shr_u), List(toWat(exp1), toWat(exp2)))
        }
      }
      case ExecutableAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        IfElse(ResultType(List(I32)), None, List(toWat(exp1)), List(toWat(exp2)), List(toWat(exp3)))


      case ExecutableAst.Expression.Branch(exp, branches, tpe, loc) => ???
      case ExecutableAst.Expression.JumpTo(sym, tpe, loc) => ???
      case ExecutableAst.Expression.Let(sym, exp1, exp2, tpe, loc) => Block(ResultType(List(I32)), None, List(
        toWat(exp1),
        SetLocal(sym.getStackOffset),
        toWat(exp2)
      ))
      case ExecutableAst.Expression.LetRec(sym, exp1, exp2, tpe, loc) => ???
      case ExecutableAst.Expression.Is(sym, tag, exp, loc) => Unreachable
      case ExecutableAst.Expression.Tag(enum, tag, exp, tpe, loc) => ???
      case ExecutableAst.Expression.Untag(sym, tag, exp, tpe, loc) => Unreachable
      case ExecutableAst.Expression.Index(base, offset, tpe, loc) => Const(I32, Int32(0))
      case ExecutableAst.Expression.Tuple(elms, tpe, loc) => ???
      case ExecutableAst.Expression.Ref(exp, tpe, loc) => ???
      case ExecutableAst.Expression.Deref(exp, tpe, loc) => ???
      case ExecutableAst.Expression.Assign(exp1, exp2, tpe, loc) => ???
      case ExecutableAst.Expression.Existential(fparam, exp, loc) => ???
      case ExecutableAst.Expression.Universal(fparam, exp, loc) => ???
      case ExecutableAst.Expression.NativeConstructor(constructor, args, tpe, loc) => Unreachable
      case ExecutableAst.Expression.NativeField(field, tpe, loc) => Unreachable
      case ExecutableAst.Expression.NativeMethod(method, args, tpe, loc) => Unreachable
      case ExecutableAst.Expression.UserError(tpe, loc) => Unreachable
      case ExecutableAst.Expression.HoleError(sym, tpe, loc) => Unreachable
      case ExecutableAst.Expression.MatchError(tpe, loc) => Unreachable
      case ExecutableAst.Expression.SwitchError(tpe, loc) => Unreachable
    }
  }

  def countLocals(exp: ExecutableAst.Expression): Int = exp.semifold(
    {
      case Let(sym, exp1, exp2, tpe, loc) => 1
      case LetRec(sym, exp1, exp2, tpe, loc) => 1
      case _ => 0
    }, (x: Int, y: Int) => x + y)

}

