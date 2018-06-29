import org.scalatest._
import org.scalatest.Matchers._


class TestObjects extends FunSuite{

  object myObject{
    val atributo1 = 1
    val atributo2 = "Uno"
    var atributoVariable = 1;
  }

  test("Test Object sencillo"){
    assertResult(1){myObject.atributo1}
    myObject.atributo2 shouldBe "Uno"
  }


  //No debido
  test("Modiicar un atributo de un object"){
    myObject.atributoVariable = 2;
    assertResult(2){myObject.atributoVariable}
  }

  test("Variables privadas"){
    val mascota  = new Mascota("Pacho",4)
    val numeroDePatas = mascota.Mascota.numeroDePatass
    assertResult(4){numeroDePatas}
  }



}
