import org.scalatest._
import org.scalatest.Matchers._


class TestSet extends  FunSuite {

  test("Set elementos unicos")
  {
    val set1 = Set("Julian","Carvajal","Montoya")
    val set2 = set1+"Julian"
    set2.size shouldBe 3
  }

  test("Set con elementos de diferente tipo")
  {
    val set1 = Set("Julian",2,'J',1.1)
    set1.size shouldBe 4
  }

  test("verificar contenido")
  {
    val set1 = Set("Julian","Carvajal","Montoya")
    val contieneElemnento = set1("Johana")
    val contieneElemento2 = set1("Montoya")
    assert(!contieneElemnento)
    assert(contieneElemento2)
  }

  test("eliminar contenido")
  {
    val set1 = Set("Julian","Montoya","Carvajal")
    val set2 = set1-"Carvajal"
    set2.contains("Julian") shouldBe true
    set2.contains("Carvajal") shouldBe false
    set1.contains("Carvajal") shouldBe true
  }

  test("Eliminar multiples elementos al tiempo")
  {
    val set1 = Set("Julian","Carvajal","Montoya","Hola")
    val set2 = set1 --List("Julian","Carvajal","Montoya")

    set1.size shouldBe 4
    set2.size shouldBe 1
  }

  test("Eliminar un elemento que no existe")
  {
    val set1 = Set("Julian","Carvajal","Montoya")
    val set2 = set1 - "Hola"
    set1.equals(set2) shouldBe true
  }

  test("Interseccion de 2 conejuntos")
  {
    val set1 = Set("Julian","Carvajal","Montoya")
    val set2 = Set("Cindy","Johana","Carvajal","Montoya")
    val settIntersecado = set1 intersect set2
    settIntersecado shouldBe Set("Carvajal","Montoya")
  }

  test("Union de 2 conjuntos")
  {
    val set1 = Set("Julian","Carvajal","Montoya")
    val set2 = Set("Cindy","Johana","Carvajal","Montoya")
    val setUnion = set1 union  set2
    val setUnion2 = set2 | set1
    setUnion shouldBe Set("Julian","Carvajal","Montoya","Johana","Cindy")
    setUnion should equal(setUnion2)
  }

  test("Diferencia de 2 conjuntos")
  {
    val set1 = Set("Julian","Carvajal","Montoya")
    val set2 = Set("Cindy","Johana","Carvajal","Montoya")

    val setDiferencia = set2 diff set1
    val setDiferencia2 = set1 diff set2
    setDiferencia shouldBe Set("Cindy","Johana")
    setDiferencia2 shouldBe Set("Julian")
  }





}
