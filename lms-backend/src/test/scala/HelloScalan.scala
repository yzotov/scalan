// important: this example is in README. If you change it, please also change README.md!

import java.io.File

import scalan.community.{ScalanCommunityDslExp, ScalanCommunityDslSeq, ScalanCommunityDsl}
import scalan.compilation.lms.{CommunityLmsBackend, CommunityBridge}
import scalan.compilation.lms.scala_.LmsCompilerScala
import scalan.it.BaseItTests

// ScalanCommunityDsl includes ScalanCommunity and all DSLs defined in that project
trait HelloScalan extends ScalanCommunityDsl {
  lazy val run = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matr[Double] = RowMajorMatrix(PArray(m.map { r: Arr[Double] => DenseVector(PArray(r))}))
    val vector: Vec[Double] = DenseVector(PArray(v))
    (matrix * vector).coords.arr
  }
  // example input
  val matrix = Array(Array(1.0, 2.0), Array(3.0, 5.0))
  val vector = Array(2.0, 3.0)
  val input = (matrix, vector)
}

// to run: lms-backend/test:runMain HelloScalanSeq
object HelloScalanSeq extends HelloScalan with ScalanCommunityDslSeq {
  def result = run(input)

  def main(args: Array[String]) = {
    println(result.mkString(","))
  }
}

// to run: lms-backend/test:runMain HelloScalanExp
object HelloScalanExp extends HelloScalan with ScalanCommunityDslExp with LmsCompilerScala {
  def makeBridge[A, B] = new CommunityBridge[A, B] {
    val scalan = HelloScalanExp
    val lms = new CommunityLmsBackend
  }

  def result = {
    // output directory
    val dir = new File("it-out")
    val compiled = buildExecutable(
      dir,
      // generated class name
      "HelloScalan1",
      // function to compile
      run,
      // should .dot file showing program IR be generated
      false)
    // not necessary if you just want to generate
    // and compile the program
    execute(compiled, "HelloScalan1", input, run)
  }

  def main(args: Array[String]): Unit = {
    println(result.mkString(","))
  }
}

class ReadmeExampleItTests extends BaseItTests {
  test("Examples from README run") {
    HelloScalanExp.result shouldEqual HelloScalanSeq.result
  }
}