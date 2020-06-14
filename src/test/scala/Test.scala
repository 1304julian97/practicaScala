import org.scalatest._
import org.scalatest.Matchers._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

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


  test("monadas con map")
  {
    val futuro = Future({
      Thread.sleep(10000)
      println("Terminó la Espera")
      "hola"
    })
    val option = Option("HEllo")
    val either = Right("Bonjour")


    val f: Future[Option[String]] = futuro.map(x=> option.map(y=> x+y).map(z => z+either.getOrElse(z+"j")))

    println(f.isCompleted)
    //val sdk = f.result(2 second)
    val ad = f.onComplete({
      case Success(e) => println(e.get)
      case Failure(e) => println(e.getCause.getMessage)
    })

  }


  test("promesa")
  {
    val p = Promise[String]()
    val f = p.future

    val producer = Future {
      println(Thread.currentThread.getName)
      println("me metí al futuro")
      val r = esperar(10)
      p.success(r)
      "hello"
    }
    esperar(10)
    //println(s"${Thread.currentThread.getName} => Hilo actual")
    f onComplete  {
      case Success(e) => println(e+"tataa")
      case Failure(ex) => println(ex)
    }
    p.success("h")
    val consumer = Future {
      println(Thread.currentThread.getName)
      println("hola de nuevo")
      f.foreach( r =>
        println(r)
      )
    }
    Thread.sleep(15000)
    println("terminé")

  }

  def esperar(tiempo:Int): String =
  {
    Thread.sleep(tiempo*1000)
    println("oeeeeee")
    "Terminé de esperar "+tiempo
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
