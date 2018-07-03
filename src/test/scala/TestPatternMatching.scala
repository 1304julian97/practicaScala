import org.scalatest._
import org.scalatest.Matchers._

class TestPatternMatching  extends FunSuite {


  //Averiguar
  test("patter maching con tuplas")
  {
    val opcion = 2
    val respuesta = opcion match {
      case 1=>(1,2,3)
      case 2=>(3,4,5)
      case 3=>(6,7,8)
      case _=>(1,2)
    }
    respuesta shouldBe (3,4,5)

    respuesta.isInstanceOf[(Int,Int,Int)] shouldBe true

    println(respuesta.getClass)
  }

  test("test con tuplas")
  {
    def goldilocks(expr: Any) = expr match {
      case ("porridge", "Papa") ⇒ "Papa eating porridge"
      case ("porridge", "Mama") ⇒ "Mama eating porridge"
      case ("porridge", "Baby") ⇒ "Baby eating porridge"
      case _ ⇒ "what?"
    }

    goldilocks(("porridge", "Mama")) should be(
      "Mama eating porridge"
    )
  }

  test("con tuplas usandog guion bajo")
  {
    def metodo(expr:Any):String = expr match {
      case("Julian",_) => "Julian _"
      case ("Julian","Carvajal") => "Julian Carvajal"
      case ("hola","Mundo") => "hola Mundo"
    }

    val respueata = metodo(("Julian","Lo que sea"))
    respueata shouldBe "Julian _"
  }

  test("reutlizando expresiones")
  {
    def metodo(expr:(String,String)):String = expr match {
      case  ("Hamburguesa",anonimo) => anonimo+" comio "+expr._1
      case ("Perro Caliente",anonimo) => anonimo+" comio "+expr._1
    }
    val respuesta = metodo(("Hamburguesa","Julian"))
    val respuesta2 = metodo(("Perro Caliente","Johana"))

    assertResult("Julian comio Hamburguesa"){respuesta}

    respuesta2 shouldBe "Johana comio Perro Caliente"
  }

  test("test con variable estable")
  {
    val foodItem = "porridge"

    def goldilocks(expr: Any) = expr match {
      case (`foodItem`, _) ⇒ "eating"
      case ("chair", "Mama") ⇒ "sitting"
      case ("bed", "Baby") ⇒ "sleeping"
      case _ ⇒ "what?"
    }

    goldilocks(("porridge", "Papa")) should be("eating")
    goldilocks(("chair", "Mama")) should be("sitting")
    goldilocks(("porridge", "Cousin")) should be("eating")
    goldilocks(("beer", "Cousin")) should be("what?")
  }

  test("pattern matching con listas")
  {
    val elemento = List(1,2,3) match {
      case x::xs =>xs
      case _ => 0
    }

    elemento shouldBe  List(2,3)
  }

  test("pattern matching con listas de un elemento")
  {
    val elemento = List(1) match {
      case x::xs =>xs
      case _ => 0
    }

    elemento shouldBe  Nil
  }

  test("pattern matching con lista vacia")
  {
    //como la lista viene vacía se va al pordefecto o sea cero
    val elemento = List() match {
      case x::xs =>xs
      case _ => 0
    }


    val elemento2 = List() match {
      case x::xs =>x
      case _ => 0
    }

    elemento shouldBe 0
    elemento2 shouldBe  0
  }

  test("elementos exactos")
  {
    val r = List(1, 2, 3) match {
      case x :: y :: z :: tail ⇒ tail
      case _ ⇒ 0
    }

    r == Nil should be(true)
  }

  test("con 4 elementos")
  {
    val resultado = List(1,2,3,4,5) match {
      case a::b::c::d => d.tail
      case _ => 0
    }
    resultado shouldBe List(5)
  }






}
