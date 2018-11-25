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

  val traverser : TreeTraverser = new TreeTraverser {
    override def traverse(tree: Tree)(implicit ctx: Context): Unit = tree match {
      case _ : Ident =>

        if (isSecondClass(tree)) {
          if (ctx.localMode.eq(firstClass))
            ctx.error(em"Found local ${tree.symbol} in first class position.", tree.pos)
          else {
            for (s <- ctx.boundary) {
              if (!tree.symbol.denot.isContainedIn(s) && !isSecondClass(s))
                ctx.error(em"Found local ${tree.symbol} inside $s.", tree.pos)
            }
          }

        }


      case Block(stats @ (fn: DefDef) :: Nil, Closure(_, fnRef, tpt)) if fnRef.symbol == fn.symbol =>
        if (ctx.localMode.eq(secondClass))
          fn.symbol.denot.setFlag(Flags.LocalMod)

        traverse(fn.rhs)(ctx.fresh.setLocalMode(firstClass).addBoundary(fn.symbol))

      case tree : ValDef =>
        if (isSafeDef(tree))
          ()
        else
          traverseWithLocalMode(tree.rhs, getClassiness(tree))

      case tree : DefDef =>
        if (isSafeDef(tree))
          ()
        else
          traverse(tree.rhs)(ctx.fresh.setLocalMode(firstClass).addBoundary(tree.symbol))

      case Assign(lhs, rhs) =>
        traverseWithLocalMode(rhs, getClassiness(lhs))

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
        traverseWithLocalMode(expr, firstClass)

      case _ => traverseChildren(tree)
    }

    def traverseWithBoundary(tree: tpd.Tree, s: Symbol)(implicit ctx: Context): Unit= {
      traverse(tree)(ctx.fresh.addBoundary(s))
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

  def getClassiness(tree: tpd.Tree)(implicit ctx: Context) : Type =
    if (isSecondClass(tree.symbol)) secondClass else firstClass

  def firstClass(implicit ctx: Context): Type = ctx.definitions.NothingType
  def secondClass(implicit ctx: Context): Type = ctx.definitions.AnyType


}
