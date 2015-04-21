package scalan

import scalan.common.Default
import scala.reflect.runtime.universe._
import scalan.compilation.{GraphVizConfig, GraphVizExport}

/**
 * Created by slesarenko on 17/01/15.
 */

trait BaseTypes extends Base { self: Scalan =>

  trait TypeWrapper[TBase, TExt] extends Reifiable[TExt] {
    def wrappedValueOfBaseType: Rep[TBase]
  }

  class BaseElemEx[TBase, TExt](extE: =>Elem[TExt])
                               (implicit override val tag: WeakTypeTag[TBase], z: Default[TBase])
    extends BaseElem[TBase] { self =>
    def getWrapperElem = extE
    override protected def getName = s"BT[${super.getName},${getWrapperElem.name}]"
  }

  class BaseElemEx1[A, TExt, CBase[_]]
    (extE: =>Elem[TExt])(implicit val eItem: Elem[A], val cont: Cont[CBase], z: Default[CBase[A]])
    extends BaseElemEx[CBase[A], TExt](extE)(cont.tag(eItem.tag), z) {
  }

  trait ExCompanion0[TBase] {
    //def defaultVal: Default[TBase]
  }

  trait ExCompanion1[TBase[_]] {
    //def defaultVal[A]: Default[TBase[A]]
  }

  final val ContainerLength = "ContainerLength"
  final val ContainerApply = "ContainerApply"
}
 trait BaseTypesSeq extends BaseTypes { scalan: ScalanSeq =>
   class SeqBaseElemEx[TBase, TExt](extE: =>Elem[TExt])
                                (implicit override val tag: WeakTypeTag[TBase], z: Default[TBase])
     extends BaseElemEx[TBase, TExt](extE) {
     override protected def getDefaultRep = {
       val defaultOfWrapper = getWrapperElem.defaultRepValue.asInstanceOf[TypeWrapper[TBase, TExt]]
       defaultOfWrapper.wrappedValueOfBaseType
     }
   }
   class SeqBaseElemEx1[A, TExt, CBase[_]]
       (extE: =>Elem[TExt])
       (implicit override val eItem: Elem[A],
                 override val cont: Cont[CBase],
                              z: Default[CBase[A]])
     extends BaseElemEx1[A, TExt, CBase](extE) {
     override protected def getDefaultRep = {
       val defaultOfWrapper = getWrapperElem.defaultRepValue.asInstanceOf[TypeWrapper[CBase[A], TExt]]
       defaultOfWrapper.wrappedValueOfBaseType
     }
   }
 }

trait BaseTypesExp extends BaseTypes with GraphVizExport { scalan: ScalanExp =>
  class ExpBaseElemEx[TBase, TExt](extE: =>Elem[TExt])
                                  (implicit override val tag: WeakTypeTag[TBase], z: Default[TBase])
    extends BaseElemEx[TBase, TExt](extE) {
    override protected def getDefaultRep = getWrapperElem.defaultRepValue.asInstanceOf[Rep[TBase]]
  }
  class ExpBaseElemEx1[A, TExt, CBase[_]]
    (extE: =>Elem[TExt])
    (implicit override val eItem: Elem[A],
               override val cont: Cont[CBase],
                               z: Default[CBase[A]])
    extends BaseElemEx1[A, TExt, CBase](extE)
  {
    override protected def getDefaultRep = getWrapperElem.defaultRepValue.asInstanceOf[Rep[CBase[A]]]
  }

  override protected def nodeColor(sym: Exp[_])(implicit config: GraphVizConfig) = sym.elem match {
    case _: BaseElemEx[_, _] => "blue"
    case _ => super.nodeColor(sym)
  }
}