import org.scalatest.FunSuite
import org.scalatest.Matchers._

class TestPartiallyAppliedFunctions extends FunSuite{

  test("Evitar Argumentos")
  {
    def suma(a:Int,b:Int,c:Int) = a+b+c
    val resultado = suma _

    resultado(1,2,3) shouldBe 6

  }

  test("Esperando el argumento faltante")
  {
    def suma(a:Int,b:Int,c:Int) = a+b+c
    val suma2 = suma(1,2,_:Int)
    val resultadoSuma = suma(1,2,3)
    val resultadoSuma2 = suma2(3)
    resultadoSuma == resultadoSuma2 shouldBe true
  }

  test("Esperando dos argumentos faltantes")
  {
    def suma(a:Int,b:Int,c:Int) = a+b+c
    val suma2 = suma(1,_:Int,_:Int)
    val resultadoSuma = suma(1,2,3)
    val resultadoSuma2 = suma2(2,3)
    resultadoSuma == resultadoSuma2 shouldBe true
  }

  test("Currying")
  {
    def suma(a:Int,b:Int,c:Int) = a+b+c

    val sumaCurrying = (suma _).curried
    val resultadoSuma = suma(1,2,3)
    val resultadoSumaCurrying = sumaCurrying(1)(2)(3)
    resultadoSuma == resultadoSumaCurrying shouldBe true
  }

  /*
  Tener presente que el guion bajo en estos casos significa una especie de espera, es decir, donde esté ubicado el guion
  significará que la función espera ese parametro. Por lo tanto si una variable se le asigna la función de un método
  y este en sus argumentos contiene un guion bajo, significa que este espera, un valor, o lo que es equivalente, una
  nueva función
   */
  test("Currying2")
  {
    def customFilter(f: Int => Boolean)(xs: List[Int]) = xs filter f
    def onlyEven(x: Int) = x % 2 == 0
    val xs = List(12, 11, 5, 20, 3, 13, 2)
    customFilter(onlyEven)(xs) should be(List(12,20,2))

    val onlyEvenFilter = customFilter(onlyEven) _
    onlyEvenFilter(xs) should be(List(12,20,2))
  }

}
