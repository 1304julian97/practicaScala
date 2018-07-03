import org.scalatest.FunSuite
import org.scalatest.Matchers._

class TestImplicits extends FunSuite{

  test("Implicits")
  {
    abstract class SemiGroup[A] {
      def add(x: A, y: A): A
    }
    abstract class Monoid[A] extends SemiGroup[A] {
      def unit: A
    }
    implicit object StringMonoid extends Monoid[String] {
      def add(x: String, y: String): String = x concat y
      def unit: String = ""
    }
    implicit object IntMonoid extends Monoid[Int] {
      def add(x: Int, y: Int): Int = x + y
      def unit: Int = 0
    }
    def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
      if (xs.isEmpty) m.unit
      else m.add(xs.head, sum(xs.tail))
    println(sum(List(1, 2, 3)))
    println(sum(List("a", "b", "c")))

  }


  test("Implicits2")
  {
    abstract  class Multiplicacion[A]
    {
      def realizarOperacion(lista:List[A]):A
    }

    implicit object implicitoEnteros extends Multiplicacion[Int]
    {
       def realizarOperacion(lista: List[Int]): Int = lista.foldRight(1)(_*_)
    }
    implicit object implicitoString extends Multiplicacion[String]
    {
      def realizarOperacion(lista: List[String]): String = lista.reduce((x,y)=>x+"*"+y)
    }

    def realizarMultplicacion[A](lista:List[A])(implicit multi: Multiplicacion[A]):A ={
      multi.realizarOperacion(lista)
    }

    val multiplicacionEnteros = realizarMultplicacion(List(1,2,3,4))
    val multiplicacionString = realizarMultplicacion(List("Julian","Carvajal","Montoya"))
    println(multiplicacionEnteros)
    println(multiplicacionString)

  }

  test("implicit3")
  {
    class KoanIntWrapper(val original: Int) {
      println(original)
      def isOdd = original % 2 != 0
    }

    implicit def thisMethodNameIsIrrelevant(value: Int) = new KoanIntWrapper(value)

    19.isOdd shouldBe true
    20.isOdd shouldBe false
  }

  test("implicit4")
  {
    class String2(val valor:String){
      println(valor)
      def validarLongitudDe10 = valor.size ==10
    }
    implicit def c(valore:String)  = new String2(valore)

    "1234567890".validarLongitudDe10 shouldBe true
    "113".validarLongitudDe10 shouldBe false
  }

  test("implicit 3 deduccion")
  {
    def howMuchCanIMake_?(hours: Int)(implicit dollarsPerHour: BigDecimal) =
      dollarsPerHour * hours

    implicit val hourlyRate = BigDecimal(34)

    howMuchCanIMake_?(30) shouldBe 1020
  }

  test("implicit 3 ambiguo")
  {
    /*
    Tener en cuenta que el compilador no es capaz de interpretar cual de los dos implicit debe de poner, por lo tanto
    es necesario mandarlos como parametros
     */
    def cuantoCuesta(horas:Int)(implicit pesosXHora:Int) = pesosXHora*horas

    implicit  val pesosXHora1 = 80000
    implicit  val pesosXHora2 = 100000

    cuantoCuesta(10)(pesosXHora1) shouldBe 800000
    cuantoCuesta(10)(pesosXHora2) shouldBe 1000000

  }

  test("implicit con valores por defecto")
  {
    def howMuchCanIMake_?(hours: Int, amount: BigDecimal = 34, currencyName: String = "Dollars") =
      (amount * hours).toString() + " " + currencyName

    howMuchCanIMake_?(30) should be("1020 Dollars")

    howMuchCanIMake_?(30, 95) should be("2850 Dollars")
  }

}
