import org.scalatest.FunSuite
import org.scalatest.Matchers._

class TestPartialFunctions extends FunSuite{

  test("partial function")
  {
    val doubleEvens: PartialFunction[Int, Int] =
      new PartialFunction[Int, Int] {
        //States that this partial function will take on the task
        def isDefinedAt(x: Int) = x % 2 == 0

        //What we do if this partial function matches
        def apply(v1: Int) = v1 * 2
      }

    val tripleOdds: PartialFunction[Int, Int] = new PartialFunction[Int, Int] {
      def isDefinedAt(x: Int) = x % 2 != 0

      def apply(v1: Int) = v1 * 3
    }

    val whatToDo = doubleEvens orElse tripleOdds //Here we chain the partial functions together

    whatToDo(3) should be(9)
    whatToDo(4) should be(8)
  }

  test("partial Function2")
  {
    val parcial1: PartialFunction[Int,Int] ={
      case x if (x==2) => x*3
    }
    val parcial2: PartialFunction[Int,Int] ={
      case y if(y==3) => y*2
    }

    val funcionFinal = parcial1 orElse parcial2
    val resultadoFuncionFinal = funcionFinal(2)
    val resultadoFuncionFinal2 = funcionFinal(3)

    //Si no pasa por ninguna de las condiciones de las funciones parciales arroja un error.
    assertThrows[scala.MatchError]{funcionFinal(4)}
    assertResult(6){resultadoFuncionFinal}
    assertResult(6){resultadoFuncionFinal2}
  }

  test("partial Function con andThen")
  {
    val parcial1: PartialFunction[Int,Int] ={
      case x if (x==2) => x*3
    }
    val parcial2: PartialFunction[Int,Int] ={
      case y if(y==3) => y*2
    }

    val lambda = (x:Int) => x*10

    val funcionFinal = parcial1 orElse parcial2 andThen lambda
    val resultadoFuncionFinal = funcionFinal(2)
    val resultadoFuncionFinal2 = funcionFinal(3)

    assertResult(60){resultadoFuncionFinal}
    assertResult(60){resultadoFuncionFinal2}
  }

  test("partial Function con andThen2")
  {
    val doubleEvens: PartialFunction[Int, Int] = {
      case x if (x % 2) == 0 ⇒ x * 2
    }
    val tripleOdds: PartialFunction[Int, Int] = {
      case x if (x % 2) != 0 ⇒ x * 3
    }

    val printEven: PartialFunction[Int, String] = {
      case x if (x % 2) == 0 ⇒ "Even"
    }
    val printOdd: PartialFunction[Int, String] = {
      case x if (x % 2) != 0 ⇒ "Odd"
    }

    val whatToDo = doubleEvens orElse tripleOdds andThen (printEven orElse printOdd)

    whatToDo(3) should be("Odd")
    whatToDo(4) should be("Even")
  }

}
