import org.scalatest._
import org.scalatest.Matchers._

class TestMap extends FunSuite {

  test("mapas")
  {

    val mapa = Map(1->"Enero",2->"Febrero",3->"Abril")
    val mapa2 = mapa + (4->"Marzo")
    mapa2.contains(1) shouldBe true
    mapa2.contains(4) shouldBe true
    mapa.contains(4) shouldBe( false)
  }

  test("obtener valores de un mapa")
  {
    val mapa = Map(1->"Enero",2->"Febrero",3->"Abril")

    val listaDeValores = mapa.values
    listaDeValores.filter(_=="Enero")shouldBe List("Enero")
    val head = listaDeValores.head
    val ultimo = listaDeValores.last
    assertResult("Enero"){head}
    assertResult("Abril"){ultimo}
  }

  test("agregar valores repetidos")
  {
    val mapa = Map(1->"Enero",2->"Febrero",3->"Marzo")
    val mapa2 = mapa+(1->"Diciembre")

    mapa2(1) shouldBe "Diciembre"

  }

}
