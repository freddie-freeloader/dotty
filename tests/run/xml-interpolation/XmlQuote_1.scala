import scala.quoted._
import scala.tasty.Tasty

import scala.language.implicitConversions

case class Xml(parts: String, args: List[Any])

object XmlQuote {

  implicit class SCOps(ctx: StringContext) {
    inline def xml(args: => Any*): Xml = ~XmlQuote.impl('(this), '(args))
  }

  def impl(receiver: Expr[SCOps], args: Expr[Seq[Any]])
          (implicit tasty: Tasty): Expr[Xml] = {
    import tasty._
    import Term._

    def abort(msg: String): Nothing =
      throw new QuoteError(msg)

    // for debugging purpose
    def pp(tree: Tree): Unit = {
      println(tree.show)
      println(tasty.showSourceCode.showTree(tree))
    }

    def liftListOfAny(lst: List[Term]): Expr[List[Any]] = lst match {
      case x :: xs  =>
        val head = x.toExpr[Any]
        val tail = liftListOfAny(xs)
        '{ ~head :: ~tail }
      case Nil => '(Nil)
    }

    def isStringConstant(tree: Term) = tree match {
      case Literal(_) => true
      case _ => false
    }

    def isSCOpsConversion(tree: Term) =
      tree.symbol.fullName == "XmlQuote$.SCOps"

    def isStringContextApply(tree: Term) =
      tree.symbol.fullName == "scala.StringContext$.apply"

    // XmlQuote.SCOps(StringContext.apply([p0, ...]: String*)
    val parts = receiver.toTasty.underlyingArgument match {
      case Apply(conv, List(Apply(fun, List(Typed(Repeated(values), _)))))
          if isSCOpsConversion(conv) &&
             isStringContextApply(fun) &&
             values.forall(isStringConstant) =>
        values.collect { case Literal(Constant.String(value)) => value }
      case tree =>
        abort(s"String literal expected, but ${tree.show} found")
    }

    // [a0, ...]: Any*
    val Typed(Repeated(args0), _) = args.toTasty.underlyingArgument

    val string = parts.mkString("??")
    '(new Xml(~string.toExpr, ~liftListOfAny(args0)))
  }
}
