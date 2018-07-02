import java.util.NoSuchElementException

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
    val listaValues = mapa2.values


    mapa2(1) shouldBe "Diciembre"
    listaValues.filter(x=>x=="Enero").size should be (0)

  }

  test("Buscar elemento que no existe")
  {
    val mapa = Map(1->"Enero",2->"Febrero",3->"Marzo")
    assertThrows[NoSuchElementException]{mapa(5)}
    assertResult("Abril"){mapa.getOrElse(5,"Abril")}

  }

  test("Buscar elemento que no existe 2")
  {
    val mapa = Map(1->"Enero",2->"Febrero",3->"Marzo") withDefault("holas")
    val mapa2 = Map(1->"Enero",2->"Febrero",3->"Marzo") withDefaultValue ("Falta un dato2")

    mapa(4) shouldBe 's'
    mapa2(4) shouldBe "Falta un dato2"

  }

  test("eliminar elementos de un mapa"){
    val mapa = Map(1->"Enero",2->"Febrero",3->"Marzo")
    val mapa2 = mapa - 1
    mapa.contains(1) shouldBe true
    mapa2.contains(1) shouldBe false
  }









}
