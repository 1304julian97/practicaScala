import org.scalatest._
import org.scalatest.Matchers._

class TestRanges extends FunSuite {

  test("Rangos con range")
  {
    val rango1 = Range(0,10)
    println(rango1)
    rango1.size shouldBe 10
    rango1.step shouldBe 1
    rango1(0) shouldBe 0
    rango1.last shouldBe 9
  }

  test("Range con 3 argumetnos")
  {
    val rango1 = Range(0,10,3)
    rango1.size shouldBe 4
    rango1(0) shouldBe 0
    rango1(1) shouldBe 3
    rango1.step shouldBe 3
    rango1.last  shouldBe 9
  }

  test("metodo contains")
  {
    val rango = Range(0,3)
    rango.contains(1) shouldBe true
  }

  test("metodo inclusive")
  {
    val rango = Range(0,3).inclusive
    rango.contains(3) shouldBe true
  }


}
