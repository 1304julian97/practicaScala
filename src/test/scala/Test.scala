import org.scalatest._
import org.scalatest.Matchers._

class Test extends FunSuite{

  test("Mi primer Test Scala"){
    var var1 = 1
    var var2 = 1
    var igualdad = var1 == var2
    assertResult(true){igualdad}
  }

  test("Instancia de Una Clase"){
    var segundaClase = new SegundaClase(1,2)
    assertResult(1){segundaClase.xx}
    assertResult(2){segundaClase.yy}
  }

  test("Instancia de un objeto con atributos objetos"){
    var empresa = new Empresa("S4N",1987)
    var persona = new Persona("Julian","Carvajal",1997,empresa)
    "S4N" should be(persona.empresa.razonSocial)
  }


  test("try key word"){
    try {
      throw new Error("Hi!!")
    }
    finally {
      println("all good here")
    }
  }

}
