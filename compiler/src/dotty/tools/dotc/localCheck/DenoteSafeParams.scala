package dotty.tools.dotc

import core._
import Types._
import Flags._
import Contexts.Context
import Symbols._
import Decorators._
import Phases._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Annotations.ConcreteAnnotation
import dotty.tools.dotc.core.Names.Name


/** A no-op transform to ensure that the compiled sources have no Phantom types in casts */
class DenoteSafeParams extends Phase {
  import tpd._

  override def run(implicit ctx: Context): Unit = {
    val curTree = ctx.compilationUnit.tpdTree
    traverser((),curTree)
  }

  override def phaseName = "denoteSafeParams"

  val traverser : TreeTraverser = new TreeTraverser {
    override def traverse(tree: tpd.Tree)(implicit ctx: Context): Unit = tree match {
      case Ident(_) => {
        if (tree.denot.name.toSimpleName.toString.contains("bar") || tree.denot.name.toSimpleName.toString.contains("baz")) {
          // denot.info.isLocalMethod
          if (tree.denot.asSymDenotation.is(LocalMod)) {
            print("Yay!")
            return ()
          }
        }
        ()
      }
      case Apply(f,x) => {
        if (f.denot.name.toSimpleName.toString.contains("bar")) {
          // denot.info.isLocalMethod
          if (tree.denot.asSymDenotation.is(LocalMod)) {
            print("Yay!")
            return ()
          }
        }
        ()
      }
        /*
      case DefDef(_,_,paramss,_,_) if paramss.headOption.getOrElse(List()).nonEmpty => {
        var tempAnnos : Set[Name] = Set()
        for {
          params : Seq[Trees.ValDef[Types.Type]] <- paramss
        } yield for {
          param <- params
        } yield {
          if (isLocal(param)) {
            tempAnnos += param.name
          }
        }
        tree.denot.info.stripPoly match {
          case x : LambdaType => {
            x.paramAnnos = tempAnnos
          }
          case _ =>
        }
        traverseChildren(tree)
      }
        */
      case _ => traverseChildren(tree)
    }
  }
  def isLocal(tree : tpd.Tree)(implicit ctx: Context) : Boolean = {
    val denotation: SymDenotations.SymDenotation = tree.tpe.termSymbol.denot
    val isLocal = {
      val annos = denotation.annotations
      if (annos.nonEmpty) annos.head.symbol.asClass.toString.contains("local") else false
    }
    isLocal
  }



}
