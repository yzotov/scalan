package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scalan.compilation.lms.LmsBackendFacade

trait WhileExpExt extends WhileExp { self: LmsBackendFacade =>
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = {
    e match {
      case While(c,b) =>
        While(f(c),f(b)).asInstanceOf[Def[A]]
      case _ =>
        super.mirrorDef(e,f)
    }
  }
}

trait ListOpsExpExt extends ListOpsExp { self: LmsBackendFacade =>
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = {
    e match {
      case ListPrepend(xs,a) =>
        ListPrepend(f(xs),f(a)).asInstanceOf[Def[A]]
      case ListCons(a,xs) =>
        ListCons(f(a),f(xs)).asInstanceOf[Def[A]]
      case _ =>
        super.mirrorDef(e,f)
    }
  }
}

trait FunctionsExpExt extends FunctionsExp { self: LmsBackendFacade =>
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = {
    e match {
      case Lambda(fu, x, y) =>
        Lambda(f(fu),f(x),f(y)).asInstanceOf[Def[A]]
      case _ =>
        super.mirrorDef(e,f)
    }
  }
}
