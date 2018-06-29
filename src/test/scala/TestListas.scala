import org.scalatest._
import org.scalatest.Matchers._


class TestListas extends FunSuite {
  test("Test listas con head"){
    val lista = List(1,2,3,4)
    val head  = lista.head
    assertResult(1){head}
  }

  test("test Listas con tail"){
    val lista = List(1,2,3,4)
    val cola = lista.tail
    cola should equal( List(2,3,4))
  }

  test("test listas filter"){
    val lista = List(1,2,3,4,5)
    val listaFiltrada = lista.filter(x=> x<4)

    listaFiltrada should equal(List(1,2,3))
  }

  test("test listas map")
  {
    val lista = List(1,2,3,4,5,6)
    val listaMap = lista.filter(x=>x<=3).map(x => {
      x match {
        case 1 => "uno"
        case 2 => "dos"
        case 3 => "tres"
        case 4 => "cuatro"
        case 5 => "cinco"
        case 6 => "seis"
        case _ => "No aplica"
      }
    })
    assertResult(List("uno","dos","tres")){listaMap}
  }

  test("test listas map con {")
  {
    val a = List(1, 2, 3,5)

    val listaMap = a.map {
      val numero = 5
      _ * numero}
    val listafiltrada = a.filter {
      val numero = 5
      _ % numero == 0
    }
    assertResult(List(5,10,15,25)){listaMap}
    assertResult(List(5)){listafiltrada}

  }

  test("test listas reducir")
  {
    val a = List(1, 3, 5, 7)
    a.reduceRight(_ + _) should equal(16)
    a.reduceLeft(_ * _) should equal(105)
  }

  test("fold vs reduce")
  {
    //fold y reduce hacen lo mismo, con la diferncia que el reduce no utilzia un valor inicial
    val a = List(1, 3, 5, 7)
    a.foldLeft(0)(_ + _) should equal(16)
    a.foldLeft(10)(_ + _) should equal(26)
    a.foldLeft(1)(_ * _) should equal(105)
    a.foldLeft(0)(_ * _) should equal(0)
  }

  test("uso de cuatro puntos AGREGA")
  {
    val a = List(1, 3, 5, 7)
    val b = 0 :: a
    b shouldBe List(0,1,3,5,7)
  }

  test("uso de 6 puntos agregar una lista a otra")
  {
    val cabeza = List(1,2)
    val cola = List(3,4)
    val listaCompleta = cabeza:::cola
    assertResult(List(1,2,3,4)){listaCompleta}
  }

  test("test cuatro puntos")
  {
    val d = Nil
    val c = 3 :: d
    val b = 2 :: c
    val a = 1 :: b

    a should be(List(1,2 ,3 ))
    a.tail should be(List(2,3))
    b.tail should be(List(3))
    c.tail should be(Nil)
  }





}