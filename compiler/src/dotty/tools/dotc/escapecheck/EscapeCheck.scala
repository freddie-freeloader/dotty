package dotty.tools.dotc
package escapecheck

import core._
import Types._
import Contexts.Context
import Symbols._
import Decorators._
import Phases._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd


class EscapeCheck extends Phase {
  import tpd._

  override def run(implicit ctx: Context): Unit = {
    val curTree = ctx.compilationUnit.tpdTree

    traverser((),curTree)(ctx.fresh.setLocalMode(firstClass))
  }

  override def phaseName = "escapeCheck"

  /*
  We pass on to pieces of information for each recursive step
   */
  val traverser : TreeTraverser = new TreeTraverser {
    override def traverse(tree: Tree)(implicit ctx: Context): Unit = tree match {
      case _ : Ident =>

        if (isSecondClass(tree)) {
          if (ctx.localMode.eq(firstClass)) {
            ctx.error(em"Found local ${tree.symbol} in first class position.", tree.pos)
          } else {
            ctx.enclosingFunctions
               .filter(isFirstClass(_))
               .filter(!tree.symbol.denot.isContainedIn(_))
               .foreach { s =>
                 ctx.error(em"Found local ${tree.symbol} inside first class $s.", tree.pos)
               }
          }

        } else {
          // Identifier is first class therefore it is legal
          ()
        }

      // Anonymous function
      case Block(stats @ (fn: DefDef) :: Nil, Closure(_, fnRef, tpe)) if fnRef.symbol == fn.symbol =>
        // Infer whether this is a local function or not
        if (ctx.localMode.eq(secondClass))
          fn.symbol.denot.setFlag(Flags.LocalMod)
          /*
           TODO: Does the above work considering what we do in the `Assign` case?
           I mean:
           - we set flag for symbol here
           - In Assign we check the type of the tree

           Also this is stateful and probably just works because we check the left side of apply first
           and after that check the mode of this node
            */

        traverse(fn.rhs)(ctx.fresh.setLocalMode(firstClass)
                                  .addEnclosingFunction(fn.symbol))

      case tree : ValDef =>
        if (isSafeDef(tree))
          ()
        else
          traverseWithLocalMode(tree.rhs, getClassiness(tree))

      case tree : DefDef =>
        if (isSafeDef(tree))
          ()
        else
          traverse(tree.rhs)(ctx.fresh.setLocalMode(firstClass)
                                      .addEnclosingFunction(tree.symbol))

      case Assign(lhs, rhs) =>
        traverseWithLocalMode(lhs, secondClass)
        traverseWithLocalMode(rhs, firstClass)

      case Apply(fn, args) =>
        traverseWithLocalMode(fn, secondClass)

        val isLocalMethod = fn.tpe match {
          case tpe : CachedTermRef => tpe.underlying.isLocalMethod
          case tpe => tpe.isLocalMethod
        }

        if (isLocalMethod)
          args.foreach(traverseWithLocalMode(_, secondClass))
        else
          args.foreach(traverseWithLocalMode(_, firstClass))


      case Block(stats, expr) =>
        stats.foreach(traverseWithLocalMode(_, secondClass))
        traverse(expr)

      // Class definition
      case TypeDef(_, tmpl @ Template(_, _, _, _)) =>
        tmpl.body.foreach(traverseWithLocalMode(_,secondClass))

      case Return(expr, _) =>
        traverseWithLocalMode(expr, firstClass)

      case If(cond, thenp, elsep) =>
        traverseWithLocalMode(cond, secondClass)
        traverse(thenp)
        traverse(elsep)

      case Match(selector, cases) =>
        // Todo: scala-escape uses first class here but is this correct?
        traverseWithLocalMode(selector, firstClass)
        cases.foreach(traverse)

      case Try(expr, cases, finalizer) =>
        (expr :: cases).foreach(traverse)
        traverseWithLocalMode(finalizer, secondClass)

      case CaseDef(_, guard, body) =>
        traverseWithLocalMode(guard, secondClass)
        traverse(body)

      case WhileDo(cond,body) =>
        traverseWithLocalMode(cond, secondClass)
        traverseWithLocalMode(body, secondClass)

      // Todo
      //case SeqLiteral(elems, _) => ???

      case Select(qualifier, name) =>
        traverse(qualifier)
        // Todo: Should we check whether the name references a second class thing? I'd say yes!
        if (isSecondClass(tree)) {
          if (ctx.localMode.eq(firstClass))
            ctx.error(em"Found local ${tree.symbol} in first class position.", tree.pos)
          else {
            for (s <- ctx.enclosingFunctions) {
              if (!tree.symbol.denot.isContainedIn(s) && isFirstClass(s))
                ctx.error(em"Found local ${tree.symbol} inside first class $s.", tree.pos)
            }
          }

        }

      /*
       TODO: `import` itself does not change the classiness of something and just changes the namespace, right?
       It seems to work that way at the moment. We should write a test for that
        */
      case Import(expr, _) =>
        traverseWithLocalMode(expr, secondClass)

      case PackageDef(_, stats) =>
        stats.foreach(traverseWithLocalMode(_, secondClass))

      case TypeApply(fn, _) =>
        traverse(fn)

      case Typed(expr, _) =>
        traverse(expr)

      case SeqLiteral(elems, _) =>
        elems.foreach(traverseWithLocalMode(_, firstClass))

      case TypeDef(_, rhs) =>
        traverseWithLocalMode(rhs, secondClass)

      case NamedArg(_, arg) =>
        traverse(arg)


      case _: This => ()
      case _: New => ()
      case _: Literal => ()
      case _: Closure => ()
      case EmptyTree => ()

      case _ =>
        //println(s"Encountered unhandled node ${tree.getClass}!")
        traverseChildren(tree)
    }

    // TODO: I think we don't need this
    def traverseWithBoundary(tree: tpd.Tree, s: Symbol)(implicit ctx: Context): Unit= {
      traverse(tree)(ctx.fresh.addEnclosingFunction(s))
    }

    def traverseWithLocalMode(tree: tpd.Tree, classiness: Types.Type)(implicit ctx: Context): Unit= {
      traverse(tree)(ctx.fresh.setLocalMode(classiness))
    }
  }

  def isSafeDef(tree: tpd.Tree)(implicit ctx: Context) : Boolean =
    tree.symbol.denot.flags.is(Flags.Safe)

  def isSecondClass(tree: tpd.Tree)(implicit ctx: Context) : Boolean =
    isSecondClass(tree.symbol)

  def isSecondClass(s: Symbol)(implicit ctx: Context) : Boolean =
    s.denot.flags.is(Flags.LocalMod)

  def isFirstClass(s: Symbol)(implicit ctx: Context) : Boolean =
    !isSecondClass(s)


  def getClassiness(tree: tpd.Tree)(implicit ctx: Context) : Type =
    if (isSecondClass(tree.symbol)) secondClass else firstClass

  def firstClass(implicit ctx: Context): Type = ctx.definitions.NothingType
  def secondClass(implicit ctx: Context): Type = ctx.definitions.AnyType


}
