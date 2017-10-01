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
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast.WatAst.Binop.{Add, Div_s, Mul, Sub}
import ca.uwaterloo.flix.language.ast.WatAst.Expr.{Exprs, Op}
import ca.uwaterloo.flix.language.ast.WatAst.Func.Local.Loc
import ca.uwaterloo.flix.language.ast.WatAst._
import ca.uwaterloo.flix.language.ast.WatAst.Func.{Local, LocalFunc}
import ca.uwaterloo.flix.language.ast.WatAst.FuncSig.FSig
import ca.uwaterloo.flix.language.ast.WatAst.Operation._
import ca.uwaterloo.flix.language.ast.WatAst.Param.ParamT
import ca.uwaterloo.flix.language.ast.WatAst.Result.Res
import ca.uwaterloo.flix.language.ast.WatAst.Value.{Float32, Float64, Int32, Int64}
import ca.uwaterloo.flix.language.ast.WatAst.WType.{F32, F64, I32, I64}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object WatGen extends Phase[SimplifiedAst.Root, WatAst.Root] {

  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[WatAst.Root, CompilationError] = {
    // Put GenSym into the implicit scope.

    val time = root.time
    WatAst.Root(time, WatAst.Module.Mod1(Some("main"), defsToWat(root.defs))).toSuccess
  }

  def defsToWat(defs: Map[Symbol.DefnSym, SimplifiedAst.Def]): List[WatAst.Func] = defs.toList.map(toWat)


  def toWat(sast: (Symbol.DefnSym, SimplifiedAst.Def)): WatAst.Func = {
    val defn = sast._2
    val wasmParams = defn.fparams.map(f => ParamT(f.sym.text, I32))
    WatAst.Func.LocalFunc(
      Some(sast._1.name),
      FSig(None, wasmParams, Res(I32)),
      List.fill(countLocals(defn.exp))(Loc(None, I32)),
      List(Expression.toWat(sast._2.exp)))
  }

  object Expression {
    def toWat(sast: SimplifiedAst.Expression): Instr = sast match {
      case SimplifiedAst.Expression.Unit => Const(I32, Int32(0))
      case SimplifiedAst.Expression.True => Const(I32, Int32(1))
      case SimplifiedAst.Expression.False => Const(I32, Int32(0))
      case SimplifiedAst.Expression.Char(lit) => Const(I32, Int32(lit))
      case SimplifiedAst.Expression.Float32(lit) => Const(F32, Float32(lit))
      case SimplifiedAst.Expression.Float64(lit) => Const(F64, Float64(lit))
      case SimplifiedAst.Expression.Int8(lit) => Const(I32, Int32(lit))
      case SimplifiedAst.Expression.Int16(lit) => Const(I32, Int32(lit))
      case SimplifiedAst.Expression.Int32(lit) => Const(I32, Int32(lit))
      case SimplifiedAst.Expression.Int64(lit) => Const(I64, Int64(lit))
      case SimplifiedAst.Expression.BigInt(lit) => ???
      case SimplifiedAst.Expression.Str(lit) => Const(I32, Int32(0))
      case SimplifiedAst.Expression.Var(sym, tpe, loc) => GetLocal(sym.getStackOffset)
      case SimplifiedAst.Expression.Def(name, tpe, loc) => ???
      case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
        throw InternalCompilerException("Lambdas should have been converted to closures and lifted.")
      case SimplifiedAst.Expression.Hook(hook, tpe, loc) =>
        throw InternalCompilerException("Hooks should have been inlined into ApplyHooks or wrapped inside lambdas.")
      case SimplifiedAst.Expression.LambdaClosure(lambda, freeVars, tpe, loc) =>
        throw InternalCompilerException("MkClosure should have been replaced by MkClosureRef after lambda lifting.")
      case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
        throw InternalCompilerException("Apply should have been replaced by ClosureConv.") // TODO: Doc
      case SimplifiedAst.Expression.Closure(exp, freeVars, tpe, loc) => Const(I32, Int32(0))
      case SimplifiedAst.Expression.ApplyClo(exp, args, tpe, loc) => Const(I32, Int32(0))
      case SimplifiedAst.Expression.ApplyDef(name, args, tpe, loc) => Const(I32, Int32(0))
      case SimplifiedAst.Expression.ApplyCloTail(exp, args, tpe, loc) => ???
      case SimplifiedAst.Expression.ApplyDefTail(name, args, tpe, loc) => ???
      case SimplifiedAst.Expression.ApplySelfTail(name, formals, actuals, tpe, loc) => ???
      case SimplifiedAst.Expression.ApplyHook(hook, args, tpe, loc) => Const(I32, Int32(0))
      case SimplifiedAst.Expression.Unary(sop, op, exp, tpe, loc) => ???
      case SimplifiedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) => op match {
        case o: ArithmeticOperator => o match {
          case Plus => Op( Bop(I32, Add), List(toWat(exp1), toWat(exp2)))
          case Minus => Op( Bop(I32, Sub), List(toWat(exp1), toWat(exp2)))
          case Times => Op( Bop(I32, Mul), List(toWat(exp1), toWat(exp2)))
          case Divide => Op( Bop(I32, Div_s), List(toWat(exp1), toWat(exp2)))
          case Modulo => ???
          case Exponentiate => ???
        }
        case _: ComparisonOperator => ???
        case _: LogicalOperator => ???
        case _: BitwiseOperator => ???
      }
      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => Const(I32, Int32(0))

      case SimplifiedAst.Expression.Branch(exp, branches, tpe, loc) => ???
      case SimplifiedAst.Expression.JumpTo(sym, tpe, loc) => ???
      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) => Exprs(List(
        toWat(exp1),
        SetLocal(sym.getStackOffset),
        toWat(exp2)
      ))
      case SimplifiedAst.Expression.LetRec(sym, exp1, exp2, tpe, loc) => ???
      case SimplifiedAst.Expression.Is(sym, tag, exp, loc) => ???
      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) => ???
      case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe, loc) => ???
      case SimplifiedAst.Expression.Index(base, offset, tpe, loc) => Const(I32, Int32(0))
      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) => ???
      case SimplifiedAst.Expression.Ref(exp, tpe, loc) => ???
      case SimplifiedAst.Expression.Deref(exp, tpe, loc) => ???
      case SimplifiedAst.Expression.Assign(exp1, exp2, tpe, loc) => ???
      case SimplifiedAst.Expression.Existential(fparam, exp, loc) => ???
      case SimplifiedAst.Expression.Universal(fparam, exp, loc) => ???
      case SimplifiedAst.Expression.NativeConstructor(constructor, args, tpe, loc) => ???
      case SimplifiedAst.Expression.NativeField(field, tpe, loc) => ???
      case SimplifiedAst.Expression.NativeMethod(method, args, tpe, loc) => Const(I32, Int32(0))
      case SimplifiedAst.Expression.UserError(tpe, loc) => ???
      case SimplifiedAst.Expression.HoleError(sym, tpe, eff, loc) => ???
      case SimplifiedAst.Expression.MatchError(tpe, loc) => Op(
        Bop(I32, Div_s),
        List(Const(I32, Int32(0)), Const(I32, Int32(0))))
      case SimplifiedAst.Expression.SwitchError(tpe, loc) => ???
    }
  }

  def countLocals(exp: SimplifiedAst.Expression): Int = exp.semifold(
    {
      case Let(sym, exp1, exp2, tpe, loc) => 1
      case LetRec(sym, exp1, exp2, tpe, loc) => 1
      case _ => 0
    }, (x: Int, y: Int) => x + y)

}

