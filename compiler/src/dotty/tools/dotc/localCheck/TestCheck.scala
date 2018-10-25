package dotty.tools.dotc

import core._
import Types._
import Contexts.Context
import Symbols._
import Decorators._
import Phases._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Annotations.ConcreteAnnotation


/** A no-op transform to ensure that the compiled sources have no Phantom types in casts */
class TestCheck extends Phase {
  import tpd._

  override def run(implicit ctx: Context): Unit = {
    val curTree = ctx.compilationUnit.tpdTree
    traverser((),curTree)
  }

  override def phaseName = "testCheck"

  // Snd-Mode
  val traverser : TreeTraverser = new TreeTraverser {
    override def traverse(tree: Tree)(implicit ctx: Context): Unit = tree match {
      case x : ValOrDefDef => {
        check((), x.rhs)
      }
      case Apply(fn,args) => {
        // We are trying to get the denotations of the args :(
        if (fn.denot.info.paramNamess.nonEmpty) {
          val argsWithNames = args.zip(fn.denot.info.paramNamess.head)
          for {
            awn: (Trees.Tree[Type], Names.TermName) <- argsWithNames
          } yield {

            if (isLocal(awn._1) && !fn.denot.info.stripPoly.paramAnnoss.contains(awn._2)) {
              ctx.error("Found local " ++ awn._1.denot.toString ++ " as non-local argument of " ++ fn.denot.toString)
            }
          }
        }
        traverseChildren(tree)
      }
      case Assign(lhs,rhs) => {
        if (!(isLocal(lhs))) {
          check((), rhs)
        } else {
          traverseChildren(tree)
        }
      }
      case _ => traverseChildren(tree)
    }
  }

  // Fst-Mode

  val check : TreeTraverser = new TreeTraverser {
    override def traverse(tree: Tree)(implicit ctx: Context): Unit = tree match {
      case Block(e1,e2) => {
        traverser((),e1)
        check((),e2)
      }
      case Ident(_) => {
        val denotation: SymDenotations.SymDenotation = tree.tpe.termSymbol.denot
        if (isLocal(tree)) ctx.error("Found local value " ++ denotation.toString) else ()
      }
      case _ =>
    }
  }

  def isLocal(tree : tpd.Tree)(implicit ctx: Context) : Boolean = {
    val denotation: SymDenotations.SymDenotation = tree.tpe.termSymbol.denot
    val isLocal = {
      val annos = denotation.annotations
      var isLocal = false
      var isSafe = false
      for {
        anno <- annos

      } yield {
        if (anno.symbol.asClass.toString.contains("local")) isLocal = true
        if (anno.symbol.asClass.toString.contains("safe")) isSafe = true
      }
      // Ignore @safe for now
      isLocal
    }
    isLocal
  }

  def isSafe(tree : tpd.Tree)(implicit ctx: Context) : Boolean = {
    val denotation: SymDenotations.SymDenotation = tree.tpe.termSymbol.denot
    val isSafe = {
      val annos = denotation.annotations
      var isLocal = false
      var isSafe = false
      for {
        anno <- annos

      } yield {
        if (anno.symbol.asClass.toString.contains("local")) isLocal = true
        if (anno.symbol.asClass.toString.contains("safe")) isSafe = true
      }
      // Ignore @safe for now
      isSafe
    }
    isSafe
  }




}
