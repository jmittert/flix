/*
 * Copyright 2015-2016 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.NamedAst.Program
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.errors.TypeError.UnresolvedDefinition
import ca.uwaterloo.flix.util.Validation.{ToSuccess, ToFailure}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.language.phase.Unification._

import scala.collection.mutable

object Typer2 {

  /**
    * Type checks the given program.
    */
  def typer(program: NamedAst.Program)(implicit genSym: GenSym): Validation[TypedAst.Root, TypeError] = {
    val s = System.nanoTime()

    /*
     * Definitions.
     */
    val constants = program.definitions.foldLeft(Map.empty[Symbol.Resolved, TypedAst.Definition.Constant]) {
      case (macc, (ns, defns)) => macc ++ defns.foldLeft(Map.empty[Symbol.Resolved, TypedAst.Definition.Constant]) {
        case (macc2, (name, defn0)) =>
          Declarations.infer(defn0, ns, program) match {
            case Success(defn, _) => macc2 + (toResolvedTemporaryHelperMethod(ns, name) -> defn)
            case Failure(e) => return e.toFailure[TypedAst.Root, TypeError]
          }
      }
    }

    /*
     * Lattices.
     */
    val lattices = program.lattices.foldLeft(Map.empty[Type, TypedAst.Definition.BoundedLattice]) {
      case (macc, (tpe, decl)) =>
        val NamedAst.Declaration.BoundedLattice(tpe, e1, e2, e3, e4, e5, ns, loc) = decl

        val Success(resolvedType, subst) = for (
          declaredType <- Types.resolve(tpe, ns, program);
          botType <- Expressions.infer(e1, ns, program);
          topType <- Expressions.infer(e2, ns, program);
          leqType <- Expressions.infer(e3, ns, program);
          lubType <- Expressions.infer(e4, ns, program);
          glbType <- Expressions.infer(e5, ns, program);
          _______ <- unifyM(botType, declaredType);
          _______ <- unifyM(topType, declaredType)
        // TODO Add constraints for leq, lub, glb, etc.
        )
          yield declaredType

        val bot = reassemble(e1, ns, program, subst)
        val top = reassemble(e2, ns, program, subst)
        val leq = reassemble(e3, ns, program, subst)
        val lub = reassemble(e4, ns, program, subst)
        val glb = reassemble(e5, ns, program, subst)

        val lattice = TypedAst.Definition.BoundedLattice(resolvedType, bot, top, leq, lub, glb, loc)
        macc + (resolvedType -> lattice)
    }

    /*
     * Tables.
     */
    val tables = inferTables(program) match {
      case Success(m, _) => m
      case Failure(e) => return e.toFailure
    }

    /*
     * Indexes.
     */
    val indexes = program.indexes.foldLeft(Map.empty[Symbol.TableSym, TypedAst.Definition.Index]) {
      case (macc, (sym, NamedAst.Declaration.Index(ident, idxs, loc))) => macc + (sym -> TypedAst.Definition.Index(sym, idxs, loc))
    }

    /*
     * Facts.
     */
    val facts = program.facts.flatMap {
      case (ns, fs) => fs map {
        case NamedAst.Declaration.Fact(head, loc) =>
          val h = Predicates.infer(head, ns, program)
          TypedAst.Constraint.Fact(h)
      }

    }

    /*
     * Rule.
     */
    val rules = program.rules.flatMap {
      case (ns, rs) => rs map {
        case NamedAst.Declaration.Rule(head, body, loc) =>
          val h = Predicates.infer(head, ns, program)
          TypedAst.Constraint.Rule(h, ???)
      }
    }

    val e = System.nanoTime()
    val time = program.time.copy(typer = e - s)

    TypedAst.Root(constants, lattices, tables, indexes, facts.toList, rules.toList, program.hooks, Nil, time).toSuccess
  }


  // TODO: Document and move somewhere.
  def inferTables(program: Program): InferMonad[Map[Symbol.TableSym, TypedAst.Table]] = {
    // resolve types for attributes in relations and lattices.
    val tables = program.tables.toList.flatMap {
      case (ns, decls) => decls.map {
        // relation, infer types for the attributes.
        case (_, NamedAst.Table.Relation(sym, attr, loc)) =>
          sequenceM(attr.map(a => infer(a, ns, program))) map {
            case as => sym -> (TypedAst.Table.Relation(sym, as, loc): TypedAst.Table)
          }
        // lattice, infer types for the keys and value.
        case (_, NamedAst.Table.Lattice(sym, keys, value, loc)) =>
          sequenceM(keys.map(a => infer(a, ns, program))) flatMap {
            case ks => infer(value, ns, program) map {
              case v => sym -> (TypedAst.Table.Lattice(sym, ks, v, loc): TypedAst.Table)
            }
          }
      }
    }

    sequenceM(tables).map(_.toMap)
  }


  def toResolvedTemporaryHelperMethod(ns: Name.NName, name: String): Symbol.Resolved =
    if (ns.isRoot) Symbol.Resolved.mk(name) else Symbol.Resolved.mk(ns.parts ::: name :: Nil)


  /**
    * Translates the given named attribute into a typed attributes.
    *
    * Substitutes the declared type for a resolved type.
    */
  def infer(attr: NamedAst.Attribute, ns: Name.NName, program: Program): InferMonad[TypedAst.Attribute] = attr match {
    case NamedAst.Attribute(ident, tpe, loc) => Types.resolve(tpe, ns, program) map {
      case rtpe => TypedAst.Attribute(ident, rtpe)
    }
  }

  object Declarations {

    /**
      * Infers the type of the given definition `defn0` in the given namespace `ns0`.
      */
    def infer(defn0: NamedAst.Declaration.Definition, ns0: Name.NName, program: NamedAst.Program)(implicit genSym: GenSym): InferMonad[TypedAst.Definition.Constant] = {

      val result = for (
        ____________ <- getSubstFromFormalParams(defn0.params, ns0, program);
        declaredType <- Types.resolve(defn0.tpe.asInstanceOf[NamedAst.Type.Lambda].retType, ns0, program);
        inferredType <- Expressions.infer(defn0.exp, ns0, program);
        unifiedType <- unifyM(declaredType, inferredType)
      ) yield unifiedType

      // TODO: See if this can be rewritten nicer
      result match {
        case Success(resultType, subst) =>
          val exp = reassemble(defn0.exp, ns0, program, subst)

          // Translate the named formals into typed formals.
          val formals = defn0.params.map {
            case NamedAst.FormalParam(sym, tpe, loc) => Types.resolve(tpe, ns0, program).map {
              case t => TypedAst.FormalArg(sym.toIdent, subst(t))
            }
          }

          sequenceM(formals) map {
            case fs =>
              val lambdaType = subst(Type.Lambda(fs.map(_.tpe), resultType))
              TypedAst.Definition.Constant(defn0.ann, defn0.sym.toResolvedTemporaryHelperMethod, fs, exp, lambdaType, defn0.loc)
          }

        case Failure(e) => Failure(e)
      }
    }

  }

  object Expressions {

    /**
      * Infers the type of the given expression `exp0` in the namespace `ns0` and `program`.
      */
    def infer(exp0: NamedAst.Expression, ns0: Name.NName, program: NamedAst.Program)(implicit genSym: GenSym): InferMonad[Type] = {

      /**
        * Infers the type of the given expression `exp0` inside the inference monad.
        */
      def visitExp(e0: NamedAst.Expression): InferMonad[Type] = e0 match {

        /*
         * Wildcard expression.
         */
        case NamedAst.Expression.Wild(tpe, loc) => liftM(tpe)

        /*
         * Variable expression.
         */
        case NamedAst.Expression.Var(sym, loc) => liftM(sym.tvar)

        /*
         * Reference expression.
         */
        case NamedAst.Expression.Ref(ref, tvar, loc) =>
          lookupRefType(ref, ns0, program) flatMap {
            case declaredType => unifyM(tvar, declaredType)
          }

        /*
         * Literal expression.
         */
        case NamedAst.Expression.Unit(loc) => liftM(Type.Unit)
        case NamedAst.Expression.True(loc) => liftM(Type.Bool)
        case NamedAst.Expression.False(loc) => liftM(Type.Bool)
        case NamedAst.Expression.Char(lit, loc) => liftM(Type.Char)
        case NamedAst.Expression.Float32(lit, loc) => liftM(Type.Float32)
        case NamedAst.Expression.Float64(lit, loc) => liftM(Type.Float64)
        case NamedAst.Expression.Int8(lit, loc) => liftM(Type.Int8)
        case NamedAst.Expression.Int16(lit, loc) => liftM(Type.Int16)
        case NamedAst.Expression.Int32(lit, loc) => liftM(Type.Int32)
        case NamedAst.Expression.Int64(lit, loc) => liftM(Type.Int64)
        case NamedAst.Expression.BigInt(lit, loc) => liftM(Type.BigInt)
        case NamedAst.Expression.Str(lit, loc) => liftM(Type.Str)

        /*
         * Lambda expression.
         */
        case NamedAst.Expression.Lambda(args, body, tpe, loc) => ??? // TODO

        /*
         * Apply expression.
         */
        case NamedAst.Expression.Apply(lambda, actuals, tvar, loc) =>
          for (
            lambdaType <- visitExp(lambda);
            actualTypes <- visitExps2(actuals);
            arrowType <- unifyM(lambdaType, Type.Lambda(actualTypes, tvar))
          ) yield tvar

        /*
         * Unary expression.
         */
        case NamedAst.Expression.Unary(op, exp1, tvar, loc) => op match {
          case UnaryOperator.LogicalNot =>
            for (
              tpe1 <- visitExp(exp1);
              res <- unifyM(tvar, tpe1, Type.Bool)
            ) yield res

          case UnaryOperator.Plus =>
            for (
              tpe1 <- visitExp(exp1)
            ) yield tpe1

          case UnaryOperator.Minus =>
            for (
              tpe1 <- visitExp(exp1)
            ) yield tpe1

          case UnaryOperator.BitwiseNegate =>
            for (
              tpe1 <- visitExp(exp1)
            ) yield tpe1
        }

        /*
         * Binary expression.
         */
        case NamedAst.Expression.Binary(op, exp1, exp2, tvar, loc) => op match {
          case BinaryOperator.Plus =>
            for (
              tpe1 <- visitExp(exp1);
              tpe2 <- visitExp(exp2);
              ____ <- unifyM(tvar, tpe1, tpe2)
            ) yield Type.Int32 // TODO

          case BinaryOperator.Minus =>
            for (
              tpe1 <- visitExp(exp1);
              tpe2 <- visitExp(exp2);
              ____ <- unifyM(tvar, tpe1, tpe2)
            ) yield Type.Int32 // TODO

          case BinaryOperator.Times =>
            for (
              tpe1 <- visitExp(exp1);
              tpe2 <- visitExp(exp2);
              ____ <- unifyM(tvar, tpe1, tpe2)
            ) yield Type.Int32 // TODO

          case BinaryOperator.Divide =>
            for (
              tpe1 <- visitExp(exp1);
              tpe2 <- visitExp(exp2);
              ____ <- unifyM(tvar, tpe1, tpe2)
            ) yield Type.Int32 // TODO

          case BinaryOperator.Modulo =>
            for (
              tpe1 <- visitExp(exp1);
              tpe2 <- visitExp(exp2);
              ____ <- unifyM(tvar, tpe1, tpe2)
            ) yield Type.Int32 // TODO

          case BinaryOperator.Exponentiate =>
            for (
              tpe1 <- visitExp(exp1);
              tpe2 <- visitExp(exp2);
              ____ <- unifyM(tvar, tpe1, tpe2)
            ) yield Type.Int32 // TODO

          case BinaryOperator.Equal | BinaryOperator.NotEqual =>
            for (
              tpe1 <- visitExp(exp1);
              tpe2 <- visitExp(exp2);
              ____ <- unifyM(tpe1, tpe2);
              resultType <- unifyM(tvar, Type.Bool)
            ) yield resultType

          case BinaryOperator.Less | BinaryOperator.LessEqual | BinaryOperator.Greater | BinaryOperator.GreaterEqual =>
            for (
              tpe1 <- visitExp(exp1);
              tpe2 <- visitExp(exp2);
              ____ <- unifyM(tpe1, tpe2);
              resultType <- unifyM(tvar, Type.Bool)
            ) yield resultType

          case BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr | BinaryOperator.Implication | BinaryOperator.Biconditional =>
            for (
              tpe1 <- visitExp(exp1);
              tpe2 <- visitExp(exp2);
              resultType <- unifyM(tvar, tpe1, tpe2, Type.Bool)
            ) yield resultType

          case BinaryOperator.BitwiseAnd | BinaryOperator.BitwiseOr | BinaryOperator.BitwiseXor | BinaryOperator.BitwiseLeftShift | BinaryOperator.BitwiseRightShift =>
            for (
              tpe1 <- visitExp(exp1);
              tpe2 <- visitExp(exp2);
              ____ <- unifyM(tvar, tpe1)
            ) yield Type.Int32

        }

        /*
         * Let expression.
         */
        case NamedAst.Expression.Let(sym, exp1, exp2, tvar, loc) =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            resultVar <- unifyM(tvar, tpe2)
          ) yield resultVar

        /*
         * If-then-else expression.
         */
        case NamedAst.Expression.IfThenElse(exp1, exp2, exp3, tvar, loc) =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            tpe3 <- visitExp(exp3);
            ____ <- unifyM(Type.Bool, tpe1);
            rtpe <- unifyM(tvar, tpe2, tpe3)
          ) yield rtpe

        /*
         * Match expression.
         */
        case NamedAst.Expression.Match(exp1, rules, tvar, loc) =>
          val patterns = rules.map(_._1)
          val bodies = rules.map(_._2)

          for (
            matchType <- visitExp(exp1);
            patternTypes <- visitPats2(patterns, ns0);
            patternType <- unifyM(patternTypes);
            ___________ <- unifyM(matchType, patternType);
            resultType <- visitExps(bodies, tvar)
          ) yield resultType

        /*
           * Switch expression.
           */
        case NamedAst.Expression.Switch(rules, tvar, loc) =>
          val condExps = rules.map(_._1)
          val bodyExps = rules.map(_._2)
          for (
            condType <- visitExps(condExps, Type.Bool);
            bodyType <- visitExps(bodyExps, Type.freshTypeVar());
            _ <- unifyM(condType, Type.Bool);
            resultType <- unifyM(tvar, bodyType)
          ) yield resultType

        /*
         * Tag expression.
         */
        case NamedAst.Expression.Tag(enum, tag, exp, tvar, loc) =>
          for (
            enumType <- lookupTagType(enum, tag, ns0, program);
            __________ <- visitExp(exp); // TODO: need to check that the nested type is compatible with one of the tag types.
            resultType <- unifyM(tvar, enumType)
          ) yield resultType

        /*
         * Tuple expression.
         */
        case NamedAst.Expression.Tuple(elms, tvar, loc) =>
          for (
            elementTypes <- visitExps2(elms);
            resultType <- unifyM(tvar, Type.mkFTuple(elementTypes))
          ) yield resultType

        /*
         * None expression.
         */
        case NamedAst.Expression.FNone(tvar, loc) =>
          liftM(Type.mkFOpt(tvar))

        /*
         * Some expression.
         */
        case NamedAst.Expression.FSome(exp, tvar, loc) =>
          for (
            innerType <- visitExp(exp);
            resultType <- unifyM(tvar, Type.mkFOpt(innerType))
          ) yield resultType

        /*
         * Nil expression.
         */
        case NamedAst.Expression.FNil(tvar, loc) =>
          liftM(Type.mkFList(tvar))

        /*
         * List expression.
         */
        case NamedAst.Expression.FList(head, tail, tvar, loc) =>
          for (
            headType <- visitExp(head);
            tailType <- visitExp(tail);
            resultType <- unifyM(tvar, Type.mkFList(headType), tailType)
          ) yield resultType

        /*
         * Vector expression.
         */
        case NamedAst.Expression.FVec(elms, tvar, loc) =>
          for (
            elementType <- visitExps(elms, Type.freshTypeVar());
            resultType <- unifyM(Type.mkFVec(elementType), tvar)
          ) yield resultType

        /*
         * Set expression.
         */
        case NamedAst.Expression.FSet(elms, tvar, loc) =>
          for (
            elementType <- visitExps(elms, Type.freshTypeVar());
            resultType <- unifyM(tvar, Type.mkFSet(elementType))
          ) yield resultType

        /*
         * Map expression.
         */
        case NamedAst.Expression.FMap(elms, tvar, loc) =>
          val keys = elms.map(_._1)
          val vals = elms.map(_._2)
          for (
            keyType <- visitExps(keys, Type.freshTypeVar());
            valType <- visitExps(vals, Type.freshTypeVar());
            resultType <- unifyM(tvar, Type.mkFMap(keyType, valType))
          ) yield resultType

        /*
         * GetIndex expression.
         */
        case NamedAst.Expression.GetIndex(exp1, exp2, tvar, loc) =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            ____ <- unifyM(tpe1, Type.mkFVec(tvar));
            ____ <- unifyM(tpe2, Type.Int32)
          ) yield tvar

        /*
         * PutIndex expression.
         */
        case NamedAst.Expression.PutIndex(exp1, exp2, exp3, tvar, loc) =>
          val elementType = Type.freshTypeVar()
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            tpe3 <- visitExp(exp3);
            ____ <- unifyM(tpe2, Type.Int32);
            ____ <- unifyM(tpe3, elementType);
            resultType <- unifyM(tvar, tpe1, Type.mkFVec(elementType))
          ) yield resultType

        /*
         * Existential expression.
         */
        case NamedAst.Expression.Existential(params, exp, loc) =>
          // TODO: Check formal parameters.
          for (
            tpe <- visitExp(exp);
            resultType <- unifyM(tpe, Type.Bool)
          ) yield resultType

        /*
         * Universal expression.
         */
        case NamedAst.Expression.Universal(params, exp, loc) =>
          val subst0 = params.foldLeft(Substitution.empty) {
            // TODO: Need to setup connection between sym and the Exp.Var's tvar.
            case (subst, NamedAst.FormalParam(sym, tpe, loc)) => ???
          }

          for (
            ___ <- liftM(Type.Bool, subst0);
            tpe <- visitExp(exp);
            ___ <- unifyM(Type.Bool, tpe)
          ) yield Type.Bool

        /*
         * Ascribe expression.
         */
        case NamedAst.Expression.Ascribe(exp, expectedType, loc) =>
          for (
            actualType <- visitExp(exp);
            resolvedType <- Types.resolve(expectedType, ns0, program);
            resultType <- unifyM(actualType, resolvedType)
          )
            yield resultType

        /*
         * User Error expression.
         */
        case NamedAst.Expression.UserError(tvar, loc) => liftM(tvar)

      }

      // TODO: Doc and names.
      def visitExps(es: List[NamedAst.Expression], tpe: Type): InferMonad[Type] = es match {
        case Nil => liftM(tpe)
        case x :: xs =>
          for (
            tpe1 <- visitExp(x);
            tpe2 <- visitExps(xs, tpe);
            resultType <- unifyM(tpe1, tpe2)
          ) yield resultType
      }

      // TODO: Doc and names.
      def visitExps2(es: List[NamedAst.Expression]): InferMonad[List[Type]] = es match {
        case Nil => liftM(Nil)
        case x :: xs =>
          for (
            tpe <- visitExp(x);
            tpes <- visitExps2(xs)
          ) yield tpe :: tpes
      }

      /**
        * Infers the type of the given pattern `pat0`.
        */
      def visitPat(pat0: NamedAst.Pattern, ns0: Name.NName): InferMonad[Type] = pat0 match {
        case NamedAst.Pattern.Wild(tvar, loc) => liftM(tvar)
        case NamedAst.Pattern.Var(sym, tvar, loc) => unifyM(sym.tvar, tvar)
        case NamedAst.Pattern.Unit(loc) => liftM(Type.Unit)
        case NamedAst.Pattern.True(loc) => liftM(Type.Bool)
        case NamedAst.Pattern.False(loc) => liftM(Type.Bool)
        case NamedAst.Pattern.Char(c, loc) => liftM(Type.Char)
        case NamedAst.Pattern.Float32(i, loc) => liftM(Type.Float32)
        case NamedAst.Pattern.Float64(i, loc) => liftM(Type.Float64)
        case NamedAst.Pattern.Int8(i, loc) => liftM(Type.Int8)
        case NamedAst.Pattern.Int16(i, loc) => liftM(Type.Int16)
        case NamedAst.Pattern.Int32(i, loc) => liftM(Type.Int32)
        case NamedAst.Pattern.Int64(i, loc) => liftM(Type.Int64)
        case NamedAst.Pattern.BigInt(i, loc) => liftM(Type.BigInt)
        case NamedAst.Pattern.Str(s, loc) => liftM(Type.Str)
        case NamedAst.Pattern.Tag(enum, tag, pat, tvar, loc) =>
          for (
            enumType <- lookupTagType(enum, tag, ns0, program);
            __________ <- visitPat(pat, ns0); // TODO: need to check that the nested type is compatible with one of the tag types.
            resultType <- unifyM(tvar, enumType)
          ) yield resultType

        case NamedAst.Pattern.Tuple(elms, tvar, loc) =>
          for (
            elementTypes <- visitPats2(elms, ns0);
            resultType <- unifyM(tvar, Type.mkFTuple(elementTypes))
          ) yield resultType

        case NamedAst.Pattern.FNone(tvar, loc) => ???
        case NamedAst.Pattern.FSome(pat, tvar, loc) => ???
        case NamedAst.Pattern.FNil(tvar, loc) => ???
        case NamedAst.Pattern.FList(hd, tl, tvar, loc) => ???
        case NamedAst.Pattern.FVec(elms, rest, tvar, loc) => ???
        case NamedAst.Pattern.FSet(elms, rest, tvar, loc) => ???
        case NamedAst.Pattern.FMap(elms, rest, tvar, loc) => ???
      }

      // TODO: Doc and names.
      def visitPats2(es: List[NamedAst.Pattern], ns: Name.NName): InferMonad[List[Type]] = es match {
        case Nil => liftM(Nil)
        case x :: xs =>
          for (
            tpe <- visitPat(x, ns);
            tpes <- visitPats2(xs, ns)
          ) yield tpe :: tpes
      }


      // TODO: Need to create initial type environment from defn

      visitExp(exp0)

    }
  }

  object Predicates {

    /**
      * Infers the type of the given head predicate.
      */
    def infer(head: NamedAst.Predicate.Head, ns: Name.NName, program: Program): TypedAst.Predicate.Head = head match {
      case NamedAst.Predicate.Head.True(loc) => TypedAst.Predicate.Head.True(loc)
      case NamedAst.Predicate.Head.False(loc) => TypedAst.Predicate.Head.False(loc)
      case NamedAst.Predicate.Head.Table(qname, terms, loc) =>
        val table = lookupTable(qname, ns, program)
        val ts = terms map Terms.toHeadTerm
        TypedAst.Predicate.Head.Table(table.sym, ts, loc)
    }

  }

  object Terms {

    // TODO: Compability
    def toHeadTerm(e: NamedAst.Expression): TypedAst.Term.Head = ???

  }

  object Types {

    /**
      * TODO: DOC
      *
      */
    def resolve(tpe0: NamedAst.Type, ns0: Name.NName, program: Program): InferMonad[Type] = tpe0 match {
      case NamedAst.Type.Unit(loc) => liftM(Type.Unit)
      case NamedAst.Type.Ref(name, loc) if name.isUnqualified => name.ident.name match {
        case "Unit" => liftM(Type.Unit)
        case "Bool" => liftM(Type.Bool)
        case "Char" => liftM(Type.Char)
        case "Float" => liftM(Type.Float64)
        case "Float32" => liftM(Type.Float32)
        case "Float64" => liftM(Type.Float64)
        case "Int" => liftM(Type.Int32)
        case "Int8" => liftM(Type.Int8)
        case "Int16" => liftM(Type.Int16)
        case "Int32" => liftM(Type.Int32)
        case "Int64" => liftM(Type.Int64)
        case "BigInt" => liftM(Type.BigInt)
        case "Str" => liftM(Type.Str)
        case typeName =>
          // Lookup the enums in the current namespace.
          // If the namespace doesn't even exist, just use an empty map.
          val decls = program.enums.getOrElse(ns0, Map.empty)
          decls.get(typeName) match {
            case None => failM(TypeError.UnresolvedType(name, ns0, loc))
            case Some(enum) => resolve(enum.tpe, ns0, program)
          }
      }
      case NamedAst.Type.Enum(name, cases) =>
        val asList = cases.toList
        val tags = asList.map(_._1)
        val tpes = asList.map(_._2)
        sequenceM(tpes.map(tpe => resolve(tpe, ns0, program))) map {
          case rtpes => Type.Enum(name.toResolvedTemporaryHelperMethod, (tags zip rtpes).toMap)
        }
      case NamedAst.Type.Tuple(elms, loc) =>
        sequenceM(elms.map(tpe => resolve(tpe, ns0, program))) map {
          case resolvedType => Type.Tuple(resolvedType)
        }
      case NamedAst.Type.Lambda(tparams, retType, loc) =>
        sequenceM(tparams.map(tpe => resolve(tpe, ns0, program))) flatMap {
          case ts => resolve(retType, ns0, program) map {
            case r => Type.Lambda(ts, r)
          }
        }
      case NamedAst.Type.Parametric(base, tparams, loc) => ??? // TODO
    }

  }

  /**
    * Applies the given substitution `subst0` to the given expression `exp0` in the given namespace `ns0`.
    */
  def reassemble(exp0: NamedAst.Expression, ns0: Name.NName, program: Program, subst0: Substitution): TypedAst.Expression = {
    /**
      * Applies the given substitution `subst0` to the given expression `exp0`.
      */
    def visitExp(exp0: NamedAst.Expression, subst0: Substitution): TypedAst.Expression = exp0 match {
      /*
       * Wildcard expression.
       */
      case NamedAst.Expression.Wild(tvar, loc) => throw InternalCompilerException("Not yet supported")

      /*
       * Variable expression.
       */
      case NamedAst.Expression.Var(sym, loc) => TypedAst.Expression.Var(sym.toIdent, subst0(sym.tvar), loc)

      /*
       * Reference expression.
       */
      case NamedAst.Expression.Ref(ref, tvar, loc) =>
        val namespace = ns0.idents.map(_.name) ::: ref.namespace.idents.map(_.name) ::: ref.ident.name :: Nil
        TypedAst.Expression.Ref(Symbol.Resolved.mk(namespace), subst0(tvar), loc)

      /*
       * Literal expression.
       */
      case NamedAst.Expression.Unit(loc) => TypedAst.Expression.Unit(loc)
      case NamedAst.Expression.True(loc) => TypedAst.Expression.True(loc)
      case NamedAst.Expression.False(loc) => TypedAst.Expression.False(loc)
      case NamedAst.Expression.Char(lit, loc) => TypedAst.Expression.Char(lit, loc)
      case NamedAst.Expression.Float32(lit, loc) => TypedAst.Expression.Float32(lit, loc)
      case NamedAst.Expression.Float64(lit, loc) => TypedAst.Expression.Float64(lit, loc)
      case NamedAst.Expression.Int8(lit, loc) => TypedAst.Expression.Int8(lit, loc)
      case NamedAst.Expression.Int16(lit, loc) => TypedAst.Expression.Int16(lit, loc)
      case NamedAst.Expression.Int32(lit, loc) => TypedAst.Expression.Int32(lit, loc)
      case NamedAst.Expression.Int64(lit, loc) => TypedAst.Expression.Int64(lit, loc)
      case NamedAst.Expression.BigInt(lit, loc) => TypedAst.Expression.BigInt(lit, loc)
      case NamedAst.Expression.Str(lit, loc) => TypedAst.Expression.Str(lit, loc)

      /*
       * Apply expression.
       */
      case NamedAst.Expression.Apply(lambda, actuals, tvar, loc) =>
        val l = visitExp(lambda, subst0)
        val as = actuals.map(e => visitExp(e, subst0))
        TypedAst.Expression.Apply(l, as, subst0(tvar), loc)

      /*
       * Lambda expression.
       */
      case NamedAst.Expression.Lambda(params, exp, tvar, loc) => ??? // TODO

      /*
       * Unary expression.
       */
      case NamedAst.Expression.Unary(op, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Unary(op, e, subst0(tvar), loc)

      /*
       * Binary expression.
       */
      case NamedAst.Expression.Binary(op, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.Binary(op, e1, e2, subst0(tvar), loc)

      /*
       * If-then-else expression.
       */
      case NamedAst.Expression.IfThenElse(exp1, exp2, exp3, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        TypedAst.Expression.IfThenElse(e1, e2, e3, subst0(tvar), loc)

      /*
       * Let expression.
       */
      case NamedAst.Expression.Let(sym, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.Let(sym.toIdent, e1, e2, subst0(tvar), loc)

      /*
       * Match expression.
       */
      case NamedAst.Expression.Match(exp1, rules, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val rs = rules map {
          case (pat, exp) => visitPat(pat, subst0) -> visitExp(exp, subst0)
        }
        TypedAst.Expression.Match(e1, rs, subst0(tvar), loc)

      /*
       * Switch expression.
       */
      case NamedAst.Expression.Switch(rules, tvar, loc) =>
        val rs = rules.map {
          case (cond, body) => (visitExp(cond, subst0), visitExp(body, subst0))
        }
        TypedAst.Expression.Switch(rs, subst0(tvar), loc)

      /*
       * Tag expression.
       */
      case NamedAst.Expression.Tag(enum, tag, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Tag(enum.toResolved, tag, e, subst0(tvar), loc)

      /*
       * Tuple expression.
       */
      case NamedAst.Expression.Tuple(elms, tvar, loc) =>
        val es = elms.map(e => visitExp(e, subst0))
        TypedAst.Expression.Tuple(es, subst0(tvar), loc)

      /*
       * None expression.
       */
      case NamedAst.Expression.FNone(tvar, loc) =>
        TypedAst.Expression.FNone(subst0(tvar), loc)

      /*
       * Some expression.
       */
      case NamedAst.Expression.FSome(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.FSome(e, subst0(tvar), loc)

      /*
       * Nil expression.
       */
      case NamedAst.Expression.FNil(tvar, loc) =>
        TypedAst.Expression.FNil(subst0(tvar), loc)

      /*
       * List expression.
       */
      case NamedAst.Expression.FList(hd, tl, tvar, loc) =>
        val e1 = visitExp(hd, subst0)
        val e2 = visitExp(tl, subst0)
        TypedAst.Expression.FList(e1, e2, subst0(tvar), loc)

      /*
       * Vec expression.
       */
      case NamedAst.Expression.FVec(elms, tvar, loc) =>
        val es = elms.map(e => visitExp(e, subst0))
        TypedAst.Expression.FVec(es, subst0(tvar), loc)

      /*
       * Set expression.
       */
      case NamedAst.Expression.FSet(elms, tvar, loc) =>
        val es = elms.map(e => visitExp(e, subst0))
        TypedAst.Expression.FSet(es, subst0(tvar), loc)

      /*
       * Map expression.
       */
      case NamedAst.Expression.FMap(elms, tvar, loc) =>
        val es = elms map {
          case (key, value) => (visitExp(key, subst0), visitExp(value, subst0))
        }
        TypedAst.Expression.FMap(es, subst0(tvar), loc)

      /*
       * GetIndex expression.
       */
      case NamedAst.Expression.GetIndex(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.GetIndex(e1, e2, subst0(tvar), loc)

      /*
       * PutIndex expression.
       */
      case NamedAst.Expression.PutIndex(exp1, exp2, exp3, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        TypedAst.Expression.PutIndex(e1, e2, e3, subst0(tvar), loc)

      /*
       * Existential expression.
       */
      case NamedAst.Expression.Existential(params, exp, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Existential(compat(params, subst0), e, loc)

      /*
       * Universal expression.
       */
      case NamedAst.Expression.Universal(params, exp, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Universal(compat(params, subst0), e, loc)

      /*
       * Ascribe expression.
       */
      case NamedAst.Expression.Ascribe(exp, tpe, loc) =>
        // simply reassemble the nested expression.
        visitExp(exp, subst0)

      /*
       * User Error expression.
       */
      case NamedAst.Expression.UserError(tvar, loc) =>
        TypedAst.Expression.Error(subst0(tvar), loc)
    }

    /**
      * Applies the given substitution `subst0` to the given pattern `pat0`.
      */
    def visitPat(pat0: NamedAst.Pattern, subst0: Substitution): TypedAst.Pattern = pat0 match {
      case NamedAst.Pattern.Wild(tvar, loc) => TypedAst.Pattern.Wildcard(subst0(tvar), loc)
      case NamedAst.Pattern.Var(sym, tvar, loc) => TypedAst.Pattern.Var(sym.toIdent, subst0(tvar), loc)
      case NamedAst.Pattern.Unit(loc) => TypedAst.Pattern.Lit(TypedAst.Literal.Unit(loc), Type.Unit, loc)
      case NamedAst.Pattern.True(loc) => TypedAst.Pattern.Lit(TypedAst.Literal.Bool(lit = true, loc), Type.Bool, loc)
      case NamedAst.Pattern.False(loc) => TypedAst.Pattern.Lit(TypedAst.Literal.Bool(lit = false, loc), Type.Bool, loc)
      case NamedAst.Pattern.Char(lit, loc) => TypedAst.Pattern.Lit(TypedAst.Literal.Char(lit, loc), Type.Char, loc)
      case NamedAst.Pattern.Float32(lit, loc) => TypedAst.Pattern.Lit(TypedAst.Literal.Float32(lit, loc), Type.Float32, loc)
      case NamedAst.Pattern.Float64(lit, loc) => TypedAst.Pattern.Lit(TypedAst.Literal.Float64(lit, loc), Type.Float64, loc)
      case NamedAst.Pattern.Int8(lit, loc) => TypedAst.Pattern.Lit(TypedAst.Literal.Int8(lit, loc), Type.Int8, loc)
      case NamedAst.Pattern.Int16(lit, loc) => TypedAst.Pattern.Lit(TypedAst.Literal.Int16(lit, loc), Type.Int16, loc)
      case NamedAst.Pattern.Int32(lit, loc) => TypedAst.Pattern.Lit(TypedAst.Literal.Int32(lit, loc), Type.Int32, loc)
      case NamedAst.Pattern.Int64(lit, loc) => TypedAst.Pattern.Lit(TypedAst.Literal.Int64(lit, loc), Type.Int64, loc)
      case NamedAst.Pattern.BigInt(lit, loc) => TypedAst.Pattern.Lit(TypedAst.Literal.BigInt(lit, loc), Type.BigInt, loc)
      case NamedAst.Pattern.Str(lit, loc) => TypedAst.Pattern.Lit(TypedAst.Literal.Str(lit, loc), Type.Str, loc)
      case NamedAst.Pattern.Tag(enum, tag, pat, tvar, loc) =>
        val p = visitPat(pat, subst0)
        TypedAst.Pattern.Tag(enum.toResolved, tag, p, subst0(tvar), loc)
      case NamedAst.Pattern.Tuple(elms, tvar, loc) =>
        val es = elms.map(e => visitPat(e, subst0))
        TypedAst.Pattern.Tuple(es, subst0(tvar), loc)
      case NamedAst.Pattern.FNone(tvar, loc) => ???
      case NamedAst.Pattern.FSome(pat, tvar, loc) => ???
      case NamedAst.Pattern.FNil(tvar, loc) => ???
      case NamedAst.Pattern.FList(hd, tl, tvar, loc) => ???
      case NamedAst.Pattern.FVec(elms, rest, tvar, loc) => ???
      case NamedAst.Pattern.FSet(elms, rest, tvar, loc) => ???
      case NamedAst.Pattern.FMap(elms, rest, tvar, loc) => ???
    }

    visitExp(exp0, subst0)
  }

  /**
    * Returns the declared type of the given reference `ref` in the current namespace `ns` in the given `program`.
    */
  // TODO: Better to lookup the defn, and then get its type?
  private def lookupRefType(ref: Name.QName, ns: Name.NName, program: Program): InferMonad[Type] = {
    // check whether the reference is fully-qualified.
    if (ref.isUnqualified) {
      // Case 1: Unqualified reference. Try the local namespace.
      program.definitions.get(ns) match {
        case None => ???
        case Some(defns) => defns.get(ref.ident.name) match {
          case None => failM(UnresolvedDefinition(ref, ns, ref.loc))
          case Some(defn) => Types.resolve(defn.tpe, ns, program)
        }
      }
    } else {
      // Case 2: Qualified. Lookup the namespace.
      program.definitions.get(ref.namespace) match {
        case None =>
          throw new RuntimeException(s"namespace ${ref.namespace} not found") // TODO: namespace doesnt exist.
        case Some(nm) => nm.get(ref.ident.name) match {
          case None => ??? // TODO: name doesnt exist in namespace.
          case Some(defn) => Types.resolve(defn.tpe, ns, program)
        }
      }
    }
  }

  /**
    * Returns the declared type of the given `tag`.
    */
  // TODO: Better to lookup the defn, and then get its type?
  private def lookupTagType(name: Name.QName, tag: Name.Ident, ns: Name.NName, program: Program): InferMonad[Type] = {
    /**
      * Lookup the tag name in all enums across all namespaces.
      */
    val matches = mutable.Set.empty[NamedAst.Declaration.Enum]
    for ((ns, decls) <- program.enums) {
      for ((enumName, decl) <- decls) {
        for ((tagName, caze) <- decl.cases) {
          if (tag.name == tagName) {
            matches += decl
          }
        }
      }
    }

    if (matches.isEmpty) {
      throw new RuntimeException("Tag not found") // TODO: Replace by error handling.
    } else if (matches.size == 1) {
      val NamedAst.Declaration.Enum(sym, cases, tpe, loc) = matches.head
      Types.resolve(tpe, ns, program)
    } else {
      // TODO: Use the current namespace, and or enum name.
      throw new RuntimeException("Ambigious tag name")
    }
  }

  /**
    * TODO: DOC
    */
  private def lookupTable(qname: Name.QName, ns: Name.NName, program: Program): NamedAst.Table = {
    if (qname.isUnqualified) {
      val tables = program.tables(ns)
      tables.get(qname.ident.name) match {
        case None => throw new RuntimeException(s"Unknown table ${qname}")
        case Some(table) => table
      }
    } else {
      ???
    }
  }

  /**
    * Returns a substitution from formal parameters to their declared types.
    *
    * @param params  the formal parameters.
    * @param ns0     the current namespace.
    * @param program the program.
    */
  private def getSubstFromFormalParams(params: List[NamedAst.FormalParam], ns0: Name.NName, program: Program): InferMonad[List[Type]] = {
    sequenceM(params map {
      case NamedAst.FormalParam(sym, tpe, loc) => Types.resolve(tpe, ns0, program) flatMap {
        case resolvedType => unifyM(sym.tvar, resolvedType)
      }
    })
  }

  private def compat(ps: List[NamedAst.FormalParam], subst: Substitution): List[Ast.FormalParam] = ps map {
    case NamedAst.FormalParam(sym, tpe, loc) => Ast.FormalParam(sym.toIdent, subst(???))
  }

}
