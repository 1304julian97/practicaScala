import org.scalatest._
import org.scalatest.Matchers._

class TestFormatting extends  FunSuite{

  test("concatenar con .format")
  {
    val s = "Julian"
    val string2 = "Hello %s".format(s)

    assertResult("Hello Julian"){string2}
  }

  test("caracteres de escape"){
    val c = 'a' //unicode for a
    val d = '\141' //octal for a
    val e = '\"'
    val f = '\\'

    "%c".format(c) should be("a")
    "%c".format(d) should be("a")
    "%c".format(e) should be("\"")
    "%c".format(f) should be("\\")
  }

  test("caracteres con enteros")
  {
    val numero = 13
    val string = "Juan tiene %d" format numero
    assertResult("Juan tiene 13"){string}
  }

  test("con 2 atributos")
  {
    val numero = 1
    val string = "litro"
    val respuesta = "ya me tome %d %s".format(numero,string)
    assertResult("ya me tome 1 litro"){respuesta}
  }

  test("con 3 atributos")
  {
    val numero = 1
    val string = "litro"
    val string2 = "de aguardiente"
    val respuesta = "ya me tome %d %s %s".format(numero,string,string2)
    respuesta shouldBe "ya me tome 1 litro de aguardiente"
  }


}
