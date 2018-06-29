import org.scalatest._
import org.scalatest.Matchers._

class TestLambdas extends FunSuite {

  test("Test Lambdas argumento y retorno iguales"){
    def lambda1 = {x:Int =>x+1}
    def lambda2 =  (x:Int) => x+1

    val resultadoLambda1 =lambda1(4)
    val resultadoLambda2 = lambda2(5)
    assertResult(5){resultadoLambda1}
    assertResult(6){resultadoLambda2}
  }

  test("Test Lambdas argumento y retorno diferentes"){
    def lambda1 = (x:Int,y:String) =>{
      val val1 = x+1
      val val2 = val1+" "+y
      val2
    }
    val resultadoLambda1 = lambda1(1,"Hola soy una Lambda")
    print(resultadoLambda1)
    assertResult("2 Hola soy una Lambda"){resultadoLambda1}
  }

  test("Test lambdas con apply"){
    def lambda = (x:Int,y:Int) =>{
      x+y
    }
    val resultadoLambda = lambda.apply(1,2)
    val resultadoLambda2 = lambda(1,2)
    val `result1andhalf` = lambda.apply(4,5)

    assertResult(3){resultadoLambda}
    resultadoLambda shouldBe resultadoLambda2
    assertResult(`result1andhalf`){9}
  }

  test("Test lambdas Scala exercise")
  {
    def lambda = { x: Int ⇒
      x + 1
    }
    def lambda2 = (x: Int) => x + 2
    val lambda3 = (x: Int) ⇒ x + 3

    val lambda4 = new Function1[Int, Int] {
      def apply(v1: Int): Int = v1 - 1
    }

    def lambda5(x: Int) = x + 1

    val result = lambda(3)
    val `result1andhalf` = lambda.apply(3)

    val result2 = lambda2(3)
    val result3 = lambda3(3)
    val result4 = lambda4(3)
    val result5 = lambda5(3)

    result should be(4)
    result1andhalf should be(4)
    result2 should be(5)
    result3 should be(6)
    result4 should be(2)
    result5 should be(4)
  }

  test("modificar variables desde lambdas"){
    var numero = 0;

    def lambda = (x:Int)=>x+numero
    val resultado1 = lambda(4)
    numero=2
    val resultado2 = lambda(4)
    assertResult(4){resultado1}
    resultado2 shouldBe 6
  }

  test("Lambda con un parametro lambda"){
    val lambda = (x:Int,y:Int,f:(Int,Int)=>Int) => f(x,y) + y
    val g = (x:Int,y:Int)=>x+y

    val resultado1 = lambda(2,3,g)
    assertResult(8){resultado1}
  }

  test("Lambda que retorna otra lambda"){
    val lambda =(x:Int) =>{
      def lambdaRetorno = (y:Int)=>y+x
      lambdaRetorno
    }

    val resultado = lambda(5)(4)

    lambda(5).isInstanceOf[Function1[Int,Int]] shouldBe true
    resultado shouldBe(9)
  }

  test("test scala exercices2")
  {
    def makeUpper(xs: List[String]) = xs map {
      _.toUpperCase
    }

    def makeWhatEverYouLike(xs: List[String], sideEffect: String ⇒ String) =
      xs map sideEffect

    makeUpper(List("abc", "xyz", "123")) should be(List("ABC", "XYZ", "123"))

    makeWhatEverYouLike(List("ABC", "XYZ", "123"), { x ⇒
      x.toLowerCase
    }) should be(List("abc", "xyz", "123"))

    //using it inline
    val myName = (name: String) => s"My name is $name"
    makeWhatEverYouLike(List("John", "Mark"), myName) should be(List("My name is John", "My name is Mark")
    )

    List("Scala", "Erlang", "Clojure") map (_.length) should be(List(5, 6, 7))
  }





}
