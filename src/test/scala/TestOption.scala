import org.scalatest.Matchers._
import org.scalatest.{Outcome, fixture}


class TestOption extends  fixture.FunSuite  {

  test("Primer Test con Option"){ ()=>
    val someString: Option[String] = Some("Julián")
    someString should be (Some("Julián"))
  }

  test("Test con Option null"){      ()=>
    val someString:Option[String] = Some(null)
    assertResult(Some(null)){
      someString
    }
  }

  test("Option Con Condicional True"){   ()=>
    val claseInstanciaObjetos = new ClaseInstanciaObjetos()
    val actual:Option[String] = claseInstanciaObjetos.metodoOption(true)
    actual should be(Some("Julian"))
  }

  test("Option con condicional False"){   ()=>
    val claseInstanciaObjetos = new ClaseInstanciaObjetos
    val actual:Option[String] = claseInstanciaObjetos.metodoOption(false)
    assertResult(None){actual}
  }

  test("Option con objeto y retorno None"){()=>
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
  test("Option con getOrElse"){   ()=>
    val claseInstanciaObjetos = new ClaseInstanciaObjetos
    val actual:Option[String]  = claseInstanciaObjetos.metodoOption(false)
    val actual2:String =  actual getOrElse "Defecto"
    assertResult("Defecto"){
      actual2
    }
  }

  test("Option con punto"){            ()=>
    val claseInstanciaObjetos = new ClaseInstanciaObjetos
    val actualOption = claseInstanciaObjetos.metodoOption(true)
    val actualvalor = actualOption.getOrElse("Defecto")
    assertResult("Julian"){actualvalor}
  }

  test("Option sin punto y con valor por defecto"){   ()=>
    val actualOption = new ClaseInstanciaObjetos().metodoOption(false)
    val actualValor = actualOption.getOrElse("Default")
    val actualValor2 = actualOption getOrElse "Default2"
    actualValor should be("Default")
    assertResult("Default2"){actualValor2}
  }

  test("Option con isEmpty"){              ()=>
    val actualOptionSome = new ClaseInstanciaObjetos().metodoOption(true)
    val actualOptionNone = new ClaseInstanciaObjetos().metodoOption(false)
    assertResult(false){actualOptionSome.isEmpty}
    assertResult(false){actualOptionSome isEmpty}
    actualOptionNone.isEmpty should be (true)
    assertResult(true){actualOptionNone isEmpty}
  }

  test("Option pattern maching"){      ()=>
    val actualOptionSome = new ClaseInstanciaObjetos().metodoOption(true)
    val actualOptionNone = new ClaseInstanciaObjetos().metodoOption(false)
    val valorOptionSome = actualOptionSome match {
      case Some(valor) => valor
      case None => "Sin Valor"
    }
    val valorOptionNone = actualOptionNone match {
      case Some(valor) => valor
      case None => "Sin Valor"
    }

    assertResult("Julian"){valorOptionSome}
    valorOptionNone should be("Sin Valor")

  }

  test("Pattern maching con _"){     ()=>
    //el _ devuelve el interior del estado que se está evaluando, en el caso de  number y noNumber devuelve 3 y None respectivo
    val number: Option[Int] = Some(3)
    val noNumber: Option[Int] = None
    val result1 = number.map(_ * 1.5)
    val result2 = noNumber.map(_ * 1.5)
    assertResult(Some(4.5)){result1}
    result2 should be(None)
  }

  test("map _ con valoresDiferentes"){  ()=>
    val numero = Some(3)
    val numeroNone:Option[Int] = None
    val stringNumero = numero.map(_+" Es un numero")
    val stringNumeroNone = numeroNone.map(_+"Esto ni me interesa por ser none")
    assertResult(Some("3 Es un numero")){stringNumero}
    stringNumeroNone should be(None)
  }

  test("Option _ y fold"){      ()=>
    //el primer argumento es un valor por defecto en caso tal que el segundo no funcione
    // el resultado del fold debe de ser del mismo tipo que el original
    val numero = Some(3)
    val numeroNone:Option[Int] = None
    val resultado1 = numero.fold(1)(_*3)
   

    val resultado2 = numeroNone.fold(1)(x=>x*3)
    assertResult(9){resultado1}
    resultado2 should be(resultado2)
  }


  test("fold y objetos"){     ()=>
    val persona = new Persona("Julian","Carvajal",1997,null)
    val personaSome:Option[Persona] = Some(persona)
    val personaNone:Option[Persona] = None
    val resultado1:Persona =personaSome.fold( new Persona("Johana","Carvajal",1991,null))(p
    =>new Persona(p.nombre,p.apellido,1996,null))


    val resultado2:Persona = personaNone.fold(new Persona("Johana","Carvajal",1991,null))(p=>p)

    assertResult("Julian"){resultado1.nombre}
    assertResult("Carvajal"){resultado1.apellido}
    assertResult(1996){resultado1.anioNacimiento}
    assertResult(null){resultado1.empresa}

    assertResult("Johana"){resultado2.nombre}
    assertResult("Carvajal"){resultado2.apellido}
    assertResult(1991){resultado2.anioNacimiento}
    assertResult(null){resultado2.empresa}
  }


  test("flatmap"){      ()=>
    val option = Option("Julian")
    val option2 = option.flatMap(x=>Option(x+"Julian"))
    assertResult(option2)(Some("JulianJulian"))

  }

   test("collect "){     ()=>
     val option = Option("asda")
     val s = Some("http").collect{case "http" => "HTTP"}
     val s2 = Some("http").filter(x=> x.equals("http")).map(x=>"HTTP")
     val s3 = Some("1").collect{case "1" => Integer.parseInt("1")}
     val s4 = option collect {case "http" => "HTTP"}
     println(s3)
     assertResult(s)(s2)
     assertResult(s4)(None)
   }

  test("contains"){   ()=>
    val option = Option("asdf")
    val f = option.contains("asdf")
    val f2 = option.contains("kldas")
    assertResult(f)(true)
    assertResult(f2)(false)
  }

  test("exists"){       ()=>
    val option = Option("julian")
    val f = option.exists(x=> x+"n"=="juliann")
    val f2 = option.exists(x=> x+"j"=="julian")
    assertResult(f)(true)
    assertResult(f2)(false)
  }

  test("size ? no srive para nada"){   ()=>
    val option = Option("julian")
    val optionList = Option(1 to 20)
    val optionNone = None
    val f = option.size
    val f2 = optionNone.size
    val f3 = optionList.size
    println(f)
    println(f2)
    println(f3)
  }

  test("filter not"){            ()=>
    val option = Option("julian")
    val f = option.filterNot(_.equals("julian"))
    assertResult(f)(None)
  }

  test("flaten"){                ()=>
    val option = Option(Option("julian"))
    val f = option.flatten
    assertResult(f)(Some("julian"))
  }


  test("for all"){         ()=>
    val option = Option("julian")
    val option2 = None
    val f = option.forall(_.equals("julian"))
    val f2 = option2.forall(_.equals("hola"))
    assertResult(f)(true)
    assertResult(f2)(true)
  }

  test("for each"){   ()=>
    val option = Option("julian")
    val option2 = None
    option2.foreach(x=> println(x))
    option.foreach(x=>println(x))
  }

  test("orElse"){  ()=>
    val option = None
    val f = option.orElse(Option("val"))
    assertResult(f)(Some("val"))
  }

  test("to left, to right"){  ()=>
    val option = Option("aljdks")
    val f = option.toLeft(() => "julian")
    val f2 = None toLeft "julian"
    assertResult(f.isLeft)(true)
    assertResult(f2.isRight)(true)
    val f3 = option toRight "holaa"
    val f4 = None toRight( ()=> "Julian")
    assertResult(f3.isRight)(true)
    assertResult(f4.isLeft)(true)
  }

  test("with filter no lo entiendo")
  {          f=>
    val option = Option("julianl")
    val f = option.withFilter(x=>x.equals("julian"))
    val f2 = f.map(x=>x+1)
    println(f2)

  }

  test("option con match")
  {    f=>
    val option = Option("julian")
    val s = option match {                            
      case Some(s) => s //HACER ALGO
      case _ => "paila" // HACER ALGO
    }
    println(f)
  }

  override protected def withFixture(test: OneArgTest):Outcome = {
    println("***************fixutre start********************")
    test("hello")

  }

  override type FixtureParam = String
}
