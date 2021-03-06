package scalan
package compilation
package lms
package cxx
package sharedptr

import java.io.File

import scalan.it.BaseItTests
import scalan.linalgebra.{LinearAlgebraExamples, MatricesDslExp, VectorsDslExp}

class JNILinAlgItTests extends BaseItTests{
  class ProgExp extends LinearAlgebraExamples with ScalanCommunityExp with ScalanCommunityDslExp with GraphVizExport with LmsCompilerCxx with JNIBridge with CommunityBridge with CommunityMethodMappingDSL with VectorsDslExp with MatricesDslExp { self =>
    val lms = new CommunityCxxShptrLmsBackend

    lazy val ddmvm_jni = fun { p: Rep[JNIType[(Array[Array[Double]], Array[Double])]] =>
      JNI_Pack(ddmvm(JNI_Extract(p)))
    }

    lazy val dsmvm_jni = fun { p: Rep[JNIType[(Array[Array[Double]], (Array[Int], (Array[Double], Int)))]] =>
      JNI_Pack(dsmvm(JNI_Extract(p)))
    }

    lazy val sdmvm_jni = fun { p: Rep[JNIType[(Array[(Array[Int], (Array[Double], Int))], Array[Double])]] =>
      JNI_Pack(sdmvm(JNI_Extract(p)))
    }

    lazy val ssmvm_jni = fun { p: Rep[JNIType[(Array[(Array[Int], (Array[Double], Int))], (Array[Int], (Array[Double], Int)))]] =>
      JNI_Pack(ssmvm(JNI_Extract(p)))
    }

    lazy val fdmvm_jni = fun { p: Rep[JNIType[((Array[Double], Int), Array[Double])]] =>
      JNI_Pack(fdmvm(JNI_Extract(p)))
    }

    lazy val fsmvm_jni = fun { p: Rep[JNIType[((Array[Double], Int), (Array[Int], (Array[Double], Int)))]] =>
      JNI_Pack(fsmvm(JNI_Extract(p)))
    }
  }

  val subfolder = "mvm-cxx"
  val prog = new ProgExp
  implicit val cfg = prog.defaultCompilerConfig
  val dir = new File(prefix, subfolder)

  test("ddmvm_jni") {
    prog.buildExecutable(dir, dir, "ddmvm", prog.ddmvm_jni, GraphVizConfig.default)
  }
  test("dsmvm_jni") {
    prog.buildExecutable(dir, dir, "dsmvm", prog.dsmvm_jni, GraphVizConfig.default)
  }
  test("sdmvm_jni") {
    prog.buildExecutable(dir, dir, "sdmvm", prog.sdmvm_jni, GraphVizConfig.default)
  }
  test("ssmvm_jni") {
    prog.buildExecutable(dir, dir, "ssmvm", prog.ssmvm_jni, GraphVizConfig.default)
  }
  test("fdmvm_jni") {
    prog.buildExecutable(dir, dir, "fdmvm", prog.fdmvm_jni, GraphVizConfig.default)
  }
  test("fsmvm_jni") {
    prog.buildExecutable(dir, dir, "fsmvm", prog.fsmvm_jni, GraphVizConfig.default)
  }
}
