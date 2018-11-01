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
    // Create local context
    val lctx = ctx.fresh

    // Set mode to local[Any] (i.e. second-class)
    lctx.setLocalMode(anyTpe)
    traverser((),curTree)(lctx)
  }

  override def phaseName = "testCheck"

  // Snd-Mode
  val traverser : TreeTraverser = new TreeTraverser {
    override def traverse(tree: Tree)(implicit ctx: Context): Unit = tree match {
      case Ident(_) => {

        val classiness: Type = getClassiness(tree)

        if (!(classiness <:< ctx.localMode)) {
          val denotation: SymDenotations.SymDenotation = tree.tpe.termSymbol.denot
          val className = classiness.typeSymbol.denot.name.toSimpleName.debugString
          ctx.error("Found local["++ className ++ "] value " ++ denotation.toString)
        }
      }

      case x : ValDef => {

        val classiness = getClassiness(tree)

        val lctx = ctx.fresh
        lctx.setLocalMode(classiness)
        this((), x.rhs)(lctx)
      }

      case x : DefDef => {

        val classiness = getClassiness(tree)

        val lctx = ctx.fresh
        lctx.setLocalMode(nothingTpe)
        this((), x.rhs)(lctx)
      }

      case Apply(fn,args) => {
        // We are trying to get the denotations of the args :(
        if (fn.denot.info.paramNamess.nonEmpty) {
          val argsWithNames = args.zip(fn.denot.info.paramNamess.head)
          for {
            awn: (Trees.Tree[Type], Names.TermName) <- argsWithNames
          } yield {

            if (hasLocalAnnotation(awn._1) && !fn.denot.info.stripPoly.paramAnnoss.contains(awn._2)) {
              ctx.error("Found local " ++ awn._1.denot.toString ++ " as non-local argument of " ++ fn.denot.toString)
            }
          }
        }
        traverseChildren(tree)
      }
      case Assign(lhs,rhs) => {
        if (!(hasLocalAnnotation(lhs))) {
          this((), rhs)
        } else {
          traverseChildren(tree)
        }
      }
      case _ => traverseChildren(tree)
    }
  }

  // Fst-Mode

  /*
  val check : TreeTraverser = new TreeTraverser {
    override def traverse(tree: Tree)(implicit ctx: Context): Unit = tree match {
      case Block(e1,e2) => {
        traverser((),e1)
        check((),e2)
      }
      case _ =>
    }
  }
  */

  def nothingTpe(implicit ctx: Context) : Type = defn.NothingClass.denot.typeRef

  def anyTpe(implicit ctx: Context) : Type = defn.AnyClass.denot.typeRef

  def getClassiness(tree: tpd.Tree)(implicit ctx: Context) : Type = {

    // If first-class we just return Nothing
    if (!hasLocalAnnotation(tree))
      return nothingTpe

    var typeParam: Type = null

    try {
      typeParam = tree.tpe.termSymbol.denot.annotations.head.asInstanceOf[ConcreteAnnotation].t.tpe.asInstanceOf[AppliedType].args.head
    } catch {
      case _: Throwable =>
        // TODO We generously assume that there is just no type param to local in this case
        return anyTpe
    }

    typeParam
  }

  def hasLocalAnnotation(tree : tpd.Tree)(implicit ctx: Context) : Boolean = {
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
