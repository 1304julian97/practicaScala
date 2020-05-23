import org.scalatest._
import org.scalatest.Matchers._


class TestListas extends FunSuite {
  test("Test listas con head") {
    val lista = List(1, 2, 3, 4)
    val head = lista.head
    assertResult(1) {
      head
    }
  }

  test("test Listas con tail") {
    val lista = List(1, 2, 3, 4)
    val cola = lista.tail
    cola should equal(List(2, 3, 4))
  }

  test("test listas filter") {
    val lista = List(1, 2, 3, 4, 5)
    val listaFiltrada = lista.filter(x => x < 4)

    listaFiltrada should equal(List(1, 2, 3))
  }

  test("test listas map") {
    val lista = List(1, 2, 3, 4, 5, 6)
    val listaMap = lista.filter(x => x <= 3).map(x => {
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
    assertResult(List("uno", "dos", "tres"))(listaMap)
  }

  test("Nil") {
    val list = Nil
    val result = list.map(_.toString)

    val optionList = Option(Nil)

    val list2 = List.empty
    val result2 = list2.map(_.toString)

    assertResult(result)(Nil)
    assertResult(result2)(Nil)
    assertResult(result)(result2)
    assertResult(result)(List.empty)
    assertResult(optionList)(Some(Nil))
  }

  test("test listas map con {") {
    val a = List(1, 2, 3, 5)

    val listaMap = a.map {
      val numero = 5
      _ * numero
    }
    val listafiltrada = a.filter {
      val numero = 5
      _ % numero == 0
    }
    assertResult(List(5, 10, 15, 25)) {
      listaMap
    }
    assertResult(List(5)) {
      listafiltrada
    }

  }

  test("test listas reducir") {
    val a = List(1, 3, 5, 7)
    a.reduceRight(_ + _) should equal(16)
    a.reduceLeft(_ * _) should equal(105)
  }

  test("fold vs reduce") {
    //fold y reduce hacen lo mismo, con la diferncia que el reduce no utilzia un valor inicial
    val a = List(1, 3, 5, 7)
    a.foldLeft(0)(_ + _) should equal(16)
    a.foldLeft(10)(_ + _) should equal(26)
    a.foldLeft(1)(_ * _) should equal(105)
    a.foldLeft(0)(_ * _) should equal(0)
  }

  test("uso de cuatro puntos AGREGA") {
    val a = List(1, 3, 5, 7)
    val b = 0 :: a
    b shouldBe List(0, 1, 3, 5, 7)
  }

  test("uso de 6 puntos agregar una lista a otra") {
    val cabeza = List(1, 2)
    val cola = List(3, 4)
    val listaCompleta = cabeza ::: cola
    assertResult(List(1, 2, 3, 4)) {
      listaCompleta
    }
  }

  test("test cuatro puntos") {
    val d = Nil
    val c = 3 :: d
    val b = 2 :: c
    val a = 1 :: b

    a should be(List(1, 2, 3))
    a.tail should be(List(2, 3))
    b.tail should be(List(3))
    c.tail should be(Nil)
  }


  test("Listas de enteros a string") {
    val lista1 = List(1, 2)
    val lista2 = lista1.map(x => x.toString)
    lista2 should be(List("1", "2"))
  }


  test("validar numero primo") {
    val numero1 = 23
    val resultado = validarSiEsPrimo(numero1)
    true should not be resultado
  }


  test("validar numero primo 2") {
    val numero1 = 24
    val resultado = validarSiEsPrimo(numero1)
    false should be(resultado)
  }

  test("validar serie  5ta  posición") {
    val i = 5
    val resultado = obtenerNumeroDeFibonacci(i)
    resultado should be(2)
  }


  def obtenerNumeroDeFibonacci(indice: Int): Int = {
    var listaInicial = (List(0, 1), 1)
    val numeroSiguiente = List.range(1, indice - 2).map(x => {
      val lista = (listaInicial._1 ::: List(listaInicial._1.sum)).tail
      listaInicial = (lista, lista.sum)
      listaInicial
    })
    print(numeroSiguiente)
    numeroSiguiente.head._2
  }


  def validarSiEsPrimo(numeroAEvaluar: Int): Boolean = {
    val listaDeDivisores = List.range(1, numeroAEvaluar + 1).map(x => numeroAEvaluar % x == 0).fold(false)((act, acc) => act || act)
    return listaDeDivisores;
  }

  test("listas ++ y ::") {

    val list = List("1", "2", "3")
    val f = list.++("4")
    val f2 = "0" :: f
    f shouldBe List("1", "2", "3", '4')
    f2 shouldBe List("0", "1", "2", "3", '4')
  }

  test("listas head and end") {
    val lista = 1 to 10
    val f = lista.end
    val f2 = lista.head
    f.shouldBe(10)
    f2.shouldBe(1)
  }

  test("for all method") {
    val lista = 1 to 10
    val f = lista.forall(_ < 80)
    println(f)
  }


  test("AGREGAR ELEMENTOS") {
    val lista = List(1, 2, 3, 4, 5, 5)
    val f = lista.++("d") //Agrega elementos al final de la lista
    val f2 = 1 :: lista //Agrega elementos al principio de la lista
    val f3 = f ::: f2 // mezcla dos listas en el ese mismo orden
    println(f)
    println(f2)
    println(f3)
  }

  test("collect") {
    val lista = List(1, 2, 3, 4, 5, 5)
    val f: PartialFunction[Int, Int] = {
      case x => x / 2
    }
    val h = lista.collect(f)
    println(h)

    val f2: PartialFunction[Int, String] = {
      case 1 => "hola 1"
    }
    val resultado2 = lista.collect(f2)
    println(resultado2)

    val resultado3 = lista.collect({ case x if x == 1 || x == 2 => x })
    println(resultado3)

  }

  test("drop") {
    val lista = List(5, 1, 2, 3, 4, 5)
    val f = lista.drop(3)
    val f2 = lista.drop(6)
    val f3 = lista.drop(-2)
    val f4 = lista.drop(0)
    val f5 = lista.dropRight(1)
    val f6 = lista.dropRight(-4)
    val f7 = lista.dropWhile(_ < 4)
    //Si el número es mayor que el tamaño  de la lista retorna lista vacía.
    // Si el número es negativo retorna la misma lista.
    println(f2)
    println(f)
    println(f3)
    println(f4)
    println(f5)
    println(f6)
    println(f7)
  }

  test("reverse") {
    val lista = List(1, 2, 3, 4, 5)
    val f = lista.reverse
    f shouldBe (List(5, 4, 3, 2, 1))
  }


  test("slice") {
    //SACA UNS SUBLISTA de acuerdo a los indices que se manden en los argumentos, inicio y fin respectivamente indexando en la lista
    // desde la posición cero. El último indice es excluyente.
    val lista = List(1, 2, 3, 4, 5, 6)
    val f = lista.slice(-3, 10)
    val f2 = lista.slice(0, 3)
    val f3 = lista.slice(1, 4)
    println(f)
    println(f2)
    println(f3)
  }


  test("map conserv, no entiendo nada :/") {
    var x = new String("j")
    var y = x
    println(x eq y)
    x = new String("h")
    println(x eq y)
    println(x)
    println(y)
    val lista = List(1, 2, 3, 4, 5)
    val lista2 = List("1", "2", "3", "4")
    val f = lista2.mapConserve(x => new String("a"))
    println(f)
  }

  test("reverse raro") {
    //reversa la lista que se manda como argumento y es agreagada de primero a la otra lista
    val x = List(1, 2, 3, 4)
    val f = x.reverse_:::(List(4, 5, 6))
    println(f)
  }

  test("string prefix") {
    //NO SIRVE PARA NADA SOLO RETORNA LA PALABRA List
    val x = List(1, 2, 3, 4, 5)
    val f = x.stringPrefix
    println(f)
  }

  test("SPAN") {
    //EVALUA HASTA QUE FALLE LA CONDICIÓN Y PARTE LA LISTA PRINCIPAL EN DOS LISTAS
    val lista = List(20, -1, -30, 1, 2, 3, 4, 5)
    val f = lista.span(x => x < 3)
    println(f)
  }

  test("split") {
    //divide la lista aacutal en dos listas por medio de sus indices, el índice es excluyente
    val lista = List(1, 2, 3, 4, 5)
    val f = lista.splitAt(3)
    println(f)
  }

  test("take") {
    val lista = List(1, 2, 3, 4, 5)
    val f = lista.take(2)
    val f2 = lista.take(3)
    val f3 = lista.take(10)
    println(f)
    println(f2)
    println(f3)
  }

  test("lkasdf")
  {
    val lista = List(1,2,3,4,5)

  }





}
