package generators.controllers

import generators.utils.OutputHelpers

object ApplicationControllerGenerator extends OutputHelpers{

  def code : String = {
    s"""
import play.api.mvc._
// import play.api.mvc.Session._
import views.html

class Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }

}
    """.trim()
  }

  def indent(code: String): String = code//code.split("\n").mkString("\n"+"  ")

  override def writeToFile(folder:String, pkg: String, fileName: String="Application.scala"){
    super.writeToFile(folder, pkg, fileName)
  }

}
