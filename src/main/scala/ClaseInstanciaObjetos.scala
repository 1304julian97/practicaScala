class ClaseInstanciaObjetos {

  def metodoOption(validar:Boolean):Option[String]={
    if (validar){
      Some("Julian")
    }
    else {
      None
    }

  }

  def metodoOptionConPersona(validar:Boolean):Option[Persona]={
    if(validar){
      Some(new Persona("Julian","Carvajal",1997,null))
    }
    else{
      None
    }
  }

}
