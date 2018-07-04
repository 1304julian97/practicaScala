import org.scalatest.FunSuite
import org.scalatest.Matchers._

class TestTraits  extends FunSuite{

  test("traits"){
    trait Evento1{
      def accion(s:String):String
    }

    class Televisor extends Evento1{
      override def accion(s: String): String = {
        "Hola soy un retorno"
      }
    }

    val televisor = new  Televisor

    televisor.accion("Hola") shouldBe "Hola soy un retorno"

  }

  test("polimorfismo de un trait")
  {
    case class Event(name: String)

    trait EventListener {
      def listen(event: Event): String
    }

    class MyListener extends EventListener {
      def listen(event: Event): String = {
        event match {
          case Event("Moose Stampede") ⇒
            "An unfortunate moose stampede occurred"
          case _ ⇒ "Nothing of importance occurred"
        }
      }
    }

    val myListener = new MyListener

    myListener.isInstanceOf[MyListener] should be(true)
    myListener.isInstanceOf[EventListener] should be (true)
    myListener.isInstanceOf[Any] should be(true)
    myListener.isInstanceOf[AnyRef] should be(true)
  }

  test("trait mixing")
  {
    trait B {
      def bId = 2
    }

    trait C{
      def cId = 4
    }

    trait A {
      C => def aId = 1
    }


    val obj = new A with C
    val obj2 = new A with B
    (obj.aId + obj.cId) should be(5)
  }

}
