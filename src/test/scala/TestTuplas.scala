import org.scalatest._
import org.scalatest.Matchers._

class TestTuplas extends FunSuite{

  test("test tupla")
  {
    val tupla = ("Julian","Carvajal","Rionegro",1997)
    val nombre = tupla._1
    val apellido = tupla._2
    val ciudad = tupla._3
    val anioNacimiento = tupla._4

    assertResult("Julian"){nombre}
    assertResult("Carvajal"){apellido}
    assertResult("Rionegro"){ciudad}
    assertResult(1997){anioNacimiento}
  }

  test("test asigancion de nombres a los valores de la tupla"){
    val tupla = ("Julian","Carvajal","Rionegro",1997)
    val (nombre,apellido,ciudad, anioNacimiento) = tupla
    assertResult("Julian"){nombre}
    assertResult("Carvajal"){apellido}
    assertResult("Rionegro"){ciudad}
    assertResult(1997){anioNacimiento}
  }

  test("test con swap"){
    val tupla = ("Julian",1)
    val tupla2 = tupla.swap
    val (nombre,numero) = tupla
    val (numero2,nombre2) = tupla2
    nombre shouldBe nombre2
    numero shouldBe numero
  }

}
