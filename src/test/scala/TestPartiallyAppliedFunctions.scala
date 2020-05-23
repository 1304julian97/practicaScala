import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.immutable

class TestPartiallyAppliedFunctions extends FunSuite{

  test("Evitar Argumentos")
  {
    def suma(a:Int,b:Int,c:Int) = a+b+c
    val resultado = suma _

    resultado(1,2,3) shouldBe 6

  }

  test("Esperando el argumento faltante")
  {
    def suma(a:Int,b:Int,c:Int) = a+b+c
    val suma2 = suma(1,2,_:Int)
    val resultadoSuma = suma(1,2,3)
    val resultadoSuma2 = suma2(3)
    resultadoSuma == resultadoSuma2 shouldBe true
  }

  test("Esperando dos argumentos faltantes")
  {
    def suma(a:Int,b:Int,c:Int) = a+b+c
    val suma2 = suma(1,_:Int,_:Int)
    val resultadoSuma = suma(1,2,3)
    val resultadoSuma2 = suma2(2,3)
    resultadoSuma == resultadoSuma2 shouldBe true
  }

  test("Currying")
  {
    def suma(a:Int,b:Int,c:Int) = a+b+c

    val sumaCurrying = (suma _).curried
    val resultadoSuma = suma(1,2,3)
    val resultadoSumaCurrying = sumaCurrying(1)(2)(3)
    resultadoSuma == resultadoSumaCurrying shouldBe true
  }

  /*
  Tener presente que el guion bajo en estos casos significa una especie de espera, es decir, donde esté ubicado el guion
  significará que la función espera ese parametro. Por lo tanto si una variable se le asigna la función de un método
  y este en sus argumentos contiene un guion bajo, significa que este espera, un valor, o lo que es equivalente, una
  nueva función
   */
  test("Currying2")
  {
    def customFilter(f: Int => Boolean)(xs: List[Int]) = xs filter f
    def onlyEven(x: Int) = x % 2 == 0
    val xs = List(12, 11, 5, 20, 3, 13, 2)
    customFilter(onlyEven)(xs) should be(List(12,20,2))

    val onlyEvenFilter = customFilter(onlyEven) _
    onlyEvenFilter(xs) should be(List(12,20,2))
  }

  /*
  abstract class Alimento
  abstract class Ingrediente
  case class Liquido(sabor:String,cantidad:String) extends Alimento
  case class Sopa(sopaDe:String,tamanio:Int) extends Alimento
  case class Grasa(cantidad:Int,unidad:String) extends Ingrediente
  case class Carbohitdrado(cantidad:Int,unidad:String) extends Ingrediente
  case class Proteina(cantidad:Int,unidad:String) extends Ingrediente
  case class Seco (proteina:Proteina,carbo:Carbohitdrado,grasa:Grasa) extends Alimento
  case class Almuerzo(seco:Seco, sopa:Sopa, pasante:Liquido)
  class Cuenta{
    var valor:Int = 0

    def setValor(valor:Int) = {this.valor = valor}
  }

  object domainServices{
    def jfalsdk(proteina: Proteina):Unit={}

  }



  sealed trait Moneda
  case object COP extends Moneda
  case object USD extends Moneda{
     def imprimir(): Unit ={
       println("USD")
     }
  }



  type PREPARACION = List[Ingrediente] => Alimento
  def prepararAlimento(ingredientes:List[Ingrediente])(preparacion:PREPARACION) = preparacion.apply(ingredientes)


  def prepararSeco25gr:PREPARACION = {
    val preparacion:PREPARACION = x =>{
      val proteina = Proteina(25,x(0).asInstanceOf[Proteina].unidad)
      val carbo = Carbohitdrado(25,x(1).asInstanceOf[Carbohitdrado].unidad)
      val grasa = Grasa(25,x(2).asInstanceOf[Grasa].unidad)
      Seco(proteina,carbo,grasa)
    }
    preparacion
  }

  def prepararSecoConArgumento(cantidadPorPorcion: Int): PREPARACION = {
    val preparacion: PREPARACION = x => {
      val proteina = Proteina(cantidadPorPorcion, x(0).asInstanceOf[Proteina].unidad)
      val carbo = Carbohitdrado(cantidadPorPorcion, x(1).asInstanceOf[Carbohitdrado].unidad)
      val grasa = Grasa(cantidadPorPorcion, x(2).asInstanceOf[Grasa].unidad)
      Seco(proteina, carbo, grasa)
    }
    preparacion
  }


  def preparaSopa:PREPARACION = {
    x:List[Ingrediente] => {
      Sopa("Arroz",3)
    }
  }

  def prepararLiquido:PREPARACION={
    x:List[Ingrediente] => {
      Liquido("Naranja","Grande")
    }
  }



  def servirAlmuerzo(seco:PREPARACION,ingredientesSeco: List[Ingrediente])(sopa:PREPARACION,ingredientesSopa:List[Ingrediente])(pasante:PREPARACION,ingredientesPasante:List[Ingrediente]): Almuerzo =
  {
    Almuerzo(seco.apply(ingredientesSeco).asInstanceOf[Seco],sopa.apply(ingredientesSopa).asInstanceOf[Sopa],pasante.apply(ingredientesPasante).asInstanceOf[Liquido])

  }

  def prepararSeco(proteina:Proteina)(carbo:Carbohitdrado)(grasa: Grasa):Seco={
    println(s"proteina lista ${proteina.cantidad}")
    println(s"carbo listo ${carbo.unidad}")
    println(s"grasa lista ${grasa.cantidad}")
    Seco(proteina,carbo,grasa)
  }

  def prepararLiquido(sabor:String)(cantidad:String):Liquido={
    println(s"el sabor es $sabor")
    println(s"la cantidad es $cantidad")
    Liquido(sabor,cantidad)
  }

  test("Tarea Kmell 1 curring test preparar almuerzo"){
    val liquido = Liquido("Naranja","Grande")

    val parcial: String => Liquido = prepararLiquido(liquido.sabor)

    val f = parcial.apply(liquido.cantidad)

    f shouldBe(liquido)


  }

  test("Tarea Kmell 2 preparar seco "){
    val f = prepararSecoConArgumento(10)
    val proteina = Proteina(10,"gr")
    val carbo = Carbohitdrado(10,"kg")
    val grasa = Grasa(10,"ton")
    val secoEsperado = Seco(proteina, carbo, grasa)
    val lista = List(proteina,carbo,grasa)

    val seco = f(lista)

    seco shouldBe(secoEsperado)


  }
  */

  sealed trait Alimento{
    val proteina:Proteina
    val carbo:Carbohitdrado
  }
  sealed trait Ingrediente{
    val cantidad:Int
    val unidad:String
  }
  case class Liquido(proteina: Proteina,carbo:Carbohitdrado,cantidadDeAzucar:Int) extends Alimento
  case class Sopa(proteina:Proteina,carbo:Carbohitdrado) extends Alimento
  case class Grasa(cantidad:Int,unidad:String) extends Ingrediente
  case class Carbohitdrado(cantidad:Int,unidad:String) extends Ingrediente
  case class Proteina(cantidad:Int,unidad:String) extends Ingrediente
  case class Seco (proteina:Proteina,carbo:Carbohitdrado,grasa:Grasa) extends Alimento
  case class Almuerzo(seco:Seco, sopa:Sopa, pasante:Liquido)
  class Cuenta{
    var valor:Int = 0

    def setValor(valor:Int) = {this.valor = valor}
  }

  object domainServices{
    def jfalsdk(proteina: Proteina):Unit={}

  }



  sealed trait Moneda
  case object COP extends Moneda
  case object USD extends Moneda{
    def imprimir(): Unit ={
      println("USD")
    }
  }



  type PREPARACION = List[Ingrediente] => Alimento
  def prepararAlimento(ingredientes:List[Ingrediente])(preparacion:PREPARACION) = preparacion.apply(ingredientes)


  def prepararSeco25gr:PREPARACION = {
    val preparacion:PREPARACION = x =>{
      val proteina = Proteina(25,x.head.unidad)
      val carbo = Carbohitdrado(25,x(1).unidad)
      val grasa = Grasa(25,x(2).unidad)
      Seco(proteina,carbo,grasa)
    }
    preparacion
  }

  def prepararSecoConArgumento(cantidadPorPorcion: Int): PREPARACION = {
    val preparacion: PREPARACION = x => {


      val proteina2 = x.collect {
        case ing: Proteina  => Proteina(cantidadPorPorcion, ing.unidad)
        case ing: Carbohitdrado  => Carbohitdrado(cantidadPorPorcion, ing.unidad)
        case ing: Grasa   => Grasa(cantidadPorPorcion, ing.unidad)
        case _ => throw new Exception("Boom!")

      }


      val proteina = Proteina(cantidadPorPorcion, x(0).asInstanceOf[Proteina].unidad)
      val carbo = Carbohitdrado(cantidadPorPorcion, x(1).asInstanceOf[Carbohitdrado].unidad)
      val grasa = Grasa(cantidadPorPorcion, x(2).asInstanceOf[Grasa].unidad)
      Seco(proteina, carbo, grasa)
    }
    preparacion
  }


  def preparaSopa:PREPARACION = {
    x:List[Ingrediente] => {
      val tupla = x match {
        case List(a:Proteina,b:Carbohitdrado,_*) => (a,b)
        case _ => throw new Exception("Ingredientes no disponibles")
      }
      Sopa(tupla._1,tupla._2)
    }
  }

  def prepararLiquido:PREPARACION={
    x:List[Ingrediente] => {
      val tupla = x match {
        case List(a:Proteina,b:Carbohitdrado,_*) => (a,b)
        case _ => throw new Exception("Ingredientes no disponibles")
      }
      Liquido(tupla._1,tupla._2,4)
    }
  }



  def servirAlmuerzo(seco:PREPARACION,ingredientesSeco: List[Ingrediente])(sopa:PREPARACION,ingredientesSopa:List[Ingrediente])(pasante:PREPARACION,ingredientesPasante:List[Ingrediente]): Almuerzo =
  {
    Almuerzo(seco.apply(ingredientesSeco).asInstanceOf[Seco],sopa.apply(ingredientesSopa).asInstanceOf[Sopa],pasante.apply(ingredientesPasante).asInstanceOf[Liquido])

  }

  def prepararSeco(proteina:Proteina)(carbo:Carbohitdrado)(grasa: Grasa):Seco={
    println(s"proteina lista ${proteina.cantidad}")
    println(s"carbo listo ${carbo.unidad}")
    println(s"grasa lista ${grasa.cantidad}")
    Seco(proteina,carbo,grasa)
  }

  def prepararLiquido(carbo:Carbohitdrado)(prote:Proteina)(cantidadAzucar:Int):Liquido={
    Liquido(prote,carbo,cantidadAzucar)
  }

  test("Tarea  lista a tupla con match"){
    val liquido = Liquido(Proteina(2,"gr"),Carbohitdrado(2,"kg"),4)
    val lista = List(liquido.proteina,liquido.carbo)
    val liquidoRespuesta = prepararLiquido.apply(lista)

    liquidoRespuesta shouldBe(liquido)

  }


  test("Tarea  lista a tupla con match sin orden"){
    val liquido = Liquido(Proteina(2,"gr"),Carbohitdrado(2,"kg"),4)
    val lista = List(liquido.carbo,liquido.proteina)
    val liquidoRespuesta = () => prepararLiquido.apply(lista)

    assertThrows[Exception]{liquidoRespuesta.apply()}
  }

  test("Tarea Kmell 1 curring test preparar almuerzo"){
    val liquido = Liquido(Proteina(2,"gr"),Carbohitdrado(2,"kg"),3)

    val parcial: Proteina => Int => Liquido = prepararLiquido(liquido.carbo)
    val parcial2: Int => Liquido = parcial(liquido.proteina)
    val parcial3 = parcial2.apply(liquido.cantidadDeAzucar)

    parcial3 shouldBe(liquido)


  }

  test("Tarea Kmell 2 preparar seco "){
    val f = prepararSecoConArgumento(10)
    val proteina = Proteina(10,"gr")
    val carbo = Carbohitdrado(10,"kg")
    val grasa = Grasa(10,"ton")
    val secoEsperado = Seco(proteina, carbo, grasa)
    val lista = List(proteina,carbo,grasa)

    val seco = f(lista)

    seco shouldBe(secoEsperado)


  }





}
