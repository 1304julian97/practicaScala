package fixtures

import org.scalatest.{FlatSpec, Matchers, Outcome, fixture}
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future



class AdServiceMock(slogann:String) extends AdService {
  val slogan = this.slogann
  override def getSlogan(): Future[String] = Future(slogann)
}



class TestWithoutFixture extends FlatSpec with ScalaFutures with Matchers {
  behavior of "ChocolateFactory"

  it should "produce small chocolate" in {

    // Given
    val adService = new AdServiceMock("Best chocolate in Poland!")
    val printer = new PackagingPrinter(adService)
    val factory = new ChocolateFactory(printer)

    // When
    val chocolate = factory.produceSmall().futureValue

    // Then
    chocolate.bar shouldBe ChocolateBar(3, 3)
    chocolate.packaging.label should include (adService.slogan)
    chocolate.packaging.label should include("9 cubes")
  }

}



/*
Esta clase usa fixture.FlatSpec para usar la herramienta propia de ScalaTest.
Consideraciones:
**No olvidar importar import org.scalatest.{FlatSpec, Matchers, Outcome, fixture}
*
*
* El método con override firma y argumentos deben de dejarse tal y como están, su cuerpo puede ser modificado.
* La variable override deberá de contener los tipos los cuales va a retornar el método.
* El cuerpo debe de tener la preparación de todos los objetos que van a implementar este fixture, al final el retorno será los objetos serparados por coma.
* En cada uno de los tests que sea necesario, invoque o instnacie esos objetos utilizando la palabra val seguido por los nombres de los objetos que usted asignó en la variable que se sobre escribió
*   ver linea 70
*
* Utilice esas variables al gusto.
*
* Nota: El método override es ejecutado  justo antes de cada test y es obligatorio poner el fixture en todos los tests.
*
*
 */
class TestWithFixture extends fixture.FlatSpec with ScalaFutures with Matchers {
    behavior of "ChocolateFactory"

    override type FixtureParam = (AdServiceMock, ChocolateFactory,Int)

    override protected def withFixture(test: OneArgTest): Outcome = {
      println("Ejecutando fixture")
      val adService = new AdServiceMock("Best chocolate in Poland!")
      val printer = new PackagingPrinter(adService)
      val factory = new ChocolateFactory(printer)
      test(adService, factory,3)
    }


    it should "produce small chocolate" in { fixture =>
      println("test1")

      // Given
      val (adService, factoryt,v) = fixture


      // When
      val chocolate = factoryt.produceSmall().futureValue

      // Then
      chocolate.bar shouldBe ChocolateBar(3, 3)
      chocolate.packaging.label should include(adService.slogan)
      chocolate.packaging.label should include("9 cubes")
    }

  it should "example tests" in {fixture =>
    println("Test2")
  }


  }


/*
Esta clase usa traits para usar los fixtures.

Tiene un comportamiento similar solo que aquí se evita sobreescribir los métodos, solo es necesario crear el trait y ya por consecuencia las variables quedarán listas para usar.
Es el mas adecuado para los tests, puesto que no todos tiene la necesidad de ejcutar el TestApp
 */

class TestFixtureWithTraitSpec extends FlatSpec with ScalaFutures with Matchers {
  behavior of "ChocolateFactory"

  trait TestApp {
    println("Ejecutando fixture")
    val adService = new AdServiceMock("Best best best")
    val printer = new PackagingPrinter(adService)
    val factory = new ChocolateFactory(printer)
  }

  it should "produce small chocolate" in new TestApp {

    println("test1 trait")
    // When
    val chocolate = factory.produceSmall().futureValue

    // Then
    chocolate.bar shouldBe ChocolateBar(3, 3)
    chocolate.packaging.label should include(adService.slogan)
    chocolate.packaging.label should include("9 cubes")
  }

  it should "second" in new TestApp {
    println("test2 trait")
  }

  it should "third" in{
    println("test 3 trait")
  }
}


/*
funciona igual que usando fixtures puros, es necesario en todos los tests usar el fixture creado como  función.
 */

class TestFixtureWithFunctions extends FlatSpec with ScalaFutures with Matchers {
  behavior of "ChocolateFactory"

  type Fixture = (AdServiceMock, ChocolateFactory)

  def testApp(slogan: String = "Best best best!")(test: Fixture => Unit): Unit = {
    println("ejecutando fixtures")
    val adService = new AdServiceMock(slogan)
    val printer = new PackagingPrinter(adService)
    val factory = new ChocolateFactory(printer)
    test((adService, factory))
  }

  it should "produce small chocolate" in testApp() { fixture =>

    println("test1 functions")
    // Given
    val (adService, factory) = fixture

    // When
    val chocolate = factory.produceSmall().futureValue

    // Then
    chocolate.bar shouldBe ChocolateBar(3, 3)
    chocolate.packaging.label should include(adService.slogan)
    chocolate.packaging.label should include("9 cubes")
  }

  it should "second test" in testApp(){ fixture =>
    println("test2 functions")
  }
}


