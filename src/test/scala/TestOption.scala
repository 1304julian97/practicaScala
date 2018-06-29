import org.scalatest.FunSuite
import org.scalatest.Matchers._


class TestOption extends  FunSuite{

  test("Primer Test con Option"){
    val someString: Option[String] = Some("Julián")
    someString should be (Some("Julián"))
  }

  test("Test con Option null"){
    val someString:Option[String] = Some(null)
    assertResult(Some(null)){
      someString
    }
  }

  test("Option Con Condicional True"){
    val claseInstanciaObjetos = new ClaseInstanciaObjetos()
    val actual:Option[String] = claseInstanciaObjetos.metodoOption(true)
    actual should be(Some("Julian"))
  }

  test("Option con condicional False"){
    val claseInstanciaObjetos = new ClaseInstanciaObjetos
    val actual:Option[String] = claseInstanciaObjetos.metodoOption(false)
    assertResult(None){actual}
  }

  test("Option con objeto y retorno None"){
    val claseInstanciaObjetos = new ClaseInstanciaObjetos
    val actual:Option[Persona] = claseInstanciaObjetos.metodoOptionConPersona(false)
    //Devuelve la excepción
    intercept[NoSuchElementException] {
      actual.get.nombre
    }
    //El correcto para el assert
    assertThrows[NoSuchElementException]{
      actual.get.nombre
    }

  }

  test("Option con getOrElse"){
    val claseInstanciaObjetos = new ClaseInstanciaObjetos
    val actual:Option[String]  = claseInstanciaObjetos.metodoOption(false)
    val actual2:String =  actual getOrElse "Defecto"
    assertResult("Defecto"){
      actual2
    }
  }



}
