package cucumbervariables
import cucumber.api.junit.Cucumber
import java.util.{Arrays => ar}


  object Variables {
    val features:Array[String] = Array("./src/test/resources/com/proteccion/advance/viabilidad")
    val glue:Array[String] = Array("com.proteccion.advance.viabilidad.steps")
    val tags:Array[String] = Array("~@IGNORE","~@SINDATA")
  }





