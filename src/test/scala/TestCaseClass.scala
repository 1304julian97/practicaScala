import org.scalatest._
import org.scalatest.Matchers._


class TestCaseClass extends FunSuite {

  test("Case class")
  {
    case class Dog(name: String, breed: String)
    val d1 = Dog("Scooby", "Doberman")
    d1.toString should be("Dog(Scooby,Doberman)")
  }

  test("Case Class Mutable")
  {
    //No recomendable mutar objetos
    //Si tiene la palabra reservada var se deja mutar por el contario no
    case class Dog(var name: String,var breed: String)
    val d1 = Dog("Scooby", "Doberman")

    d1.name should be("Scooby")
    d1.breed should be("Doberman")

    d1.name = "Scooby Doo"
    d1.breed = "Pastor"

    d1.name should be("Scooby Doo")
    d1.breed should be("Pastor")
  }

  test("Case class copiar objetos cambiando sus atributos")
  {
    case class Dog(nombre:String,raza:String)
    val perro1 = Dog("Pacho","Labrador")
    val perro2 = perro1.copy("Lukas")
    val perro3 = perro1.copy("Newton","Pastor")

    perro1 == new Dog("Pacho","Labrador") shouldBe true
    perro2 == new Dog("Lukas","Labrador") shouldBe true
    perro3 == new Dog("Newton","Pastor") shouldBe true

  }

  test("Case class en desorden")
  {
    case class Persona(nombre:String,apellido:String,edad:Int = 0)
    val persona = Persona("Julian","Carvajal")
    val persona2 = Persona("Julian","Carvajal",21)
    val persona3 = Persona(apellido = "Carvajal",nombre = "Julian")
    val persona4 = Persona(edad = 21,nombre = "Julian",apellido = "Carvajal")
    persona3 == persona shouldBe true
    persona4 == persona2 shouldBe true
  }

  test("Convertir case class en una tupla") {
    case class Persona(nombre: String, apellido: String, edad: Int)
    val persona = Persona("Julian", "Carvajal", 21)
    val persontaOptionTupla = Persona.unapply(persona)

    persontaOptionTupla.get == ("Julian", "Carvajal", 21) shouldBe true
    val personaTupla = persontaOptionTupla.get

    personaTupla._1 shouldBe "Julian"
    personaTupla._2 shouldBe "Carvajal"
    personaTupla._3 shouldBe 21
  }

  test("un case class es seriablizable")
  {
    case class PersonCC(firstName: String, lastName: String)
    val indy = PersonCC("Indiana", "Jones")

    indy.isInstanceOf[Serializable] should be(true)

    class Person(firstName: String, lastName: String)
    val junior = new Person("Indiana", "Jones")

    junior.isInstanceOf[Serializable] should be(false)
  }

}
