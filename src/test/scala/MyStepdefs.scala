import cucumber.api.java8.{En => English}
import cucumber.api.java.en.{Given, When, Then}

class MyStepdefs(s:String,s2:String) extends English {


  @Given("^something$")
  @throws[Throwable]
  def something(): Unit = {
    print("Holaaa!!")
  }

}
