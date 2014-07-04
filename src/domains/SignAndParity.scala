package domains

import impl.logic.{HornClause, Predicate, Term, Type}
import syntax.Symbols._

object SignAndParity {

  val Lattice = Type.Constructor2("SP", Sign.lattice, Parity.Lattice)

  val Leq = Set(
    HornClause(
      head = Predicate("SP.Leq", List(
        Term.Constructor2("SP", Term.Variable("s1"), Term.Variable("p1")),
        Term.Constructor2("SP", Term.Variable("s2"), Term.Variable("p2"))
      )),
      body = Set(
        Predicate("Sign.Leq", List(Term.Variable("s1"), Term.Variable("s2"))),
        Predicate("Parity.Leq", List(Term.Variable("p1"), Term.Variable("p2")))
      ))
  )

  val Join = Set(
    HornClause(
      head = Predicate("SP.Join", List(
        Term.Constructor2("SP", Term.Variable("s1"), Term.Variable("p1")),
        Term.Constructor2("SP", Term.Variable("s2"), Term.Variable("p2")),
        Term.Constructor2("SP", Term.Variable("s3"), Term.Variable("p3"))
      )),
      body = Set(
        Predicate("Sign.Join", List(Term.Variable("s1"), Term.Variable("s2"), Term.Variable("s3"))),
        Predicate("Parity.Join", List(Term.Variable("p1"), Term.Variable("p2"), Term.Variable("p3")))
      )))

  val Sum = Set(
    HornClause(
      head = Predicate("SP.Sum", List(
        Term.Constructor2("SP", Term.Variable("s1"), Term.Variable("p1")),
        Term.Constructor2("SP", Term.Variable("s2"), Term.Variable("p2")),
        Term.Constructor2("SP", Term.Variable("s3"), Term.Variable("p3"))
      )),
      body = Set(
        Predicate("Sign.Sum", List(Term.Variable("s1"), Term.Variable("s2"), Term.Variable("s3"))),
        Predicate("Parity.Sum", List(Term.Variable("p1"), Term.Variable("p2"), Term.Variable("p3")))
      )
    )
  )

}
