package generators.models.slick

import generators.utils._

import slick.model.{Column, Table}

import slick.model.ForeignKeyAction._
import slick.ast.ColumnOption

object JsonGenerator {
  def generate(outputFolder : String, pkg : String) = {

    val appConfig = AppConfigParser.getAppConfig

    val model = new ModelProvider(appConfig).model

    val foreignKeyInfo = new ForeignKeyInfo(model)

    new SlickJsonGenerator(model).writeToFile(outputFolder, pkg)
  }
}

class SlickJsonGenerator(model : slick.model.Model) extends OutputHelpers with GeneratorHelpers {

  override def code: String = objectCode

  override def indent(code: String): String = code

  def objectCode : String = {
    s"""
import models._
import play.api.libs.json._
import java.sql.Timestamp
import play.api.libs.functional.syntax._
import play.api.libs.json._
import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format.{DateTimeFormat,ISODateTimeFormat}

object JsonFormat {
	final val ISODateFormat = "yyyy-MM-dd'T'HH:mm:ss'Z'"
	final val ISOFmt = DateTimeFormat.forPattern(ISODateFormat)

	implicit val tsReads = Reads[Timestamp] (js => js.validate[String].map[Timestamp]( tsString => new Timestamp(ISOFmt.parseDateTime(tsString).getMillis)))
	implicit val tsWrites = Writes[Timestamp] (js => JsString((new DateTime(js.getTime, DateTimeZone.UTC)).toString(ISODateFormat)))
	implicit val fmt: Format[Timestamp] = Format(tsReads, tsWrites)

${methods}

}""".trim()
  }

  //	tbl.name.table.toCamelCase
//		implicit val crewRowReads = Json.reads[CrewRow]
//	implicit val crewRowWrites = Json.writes[CrewRow]
//	implicit val rideRowReads = Json.reads[RideRow]
//	implicit val rideRowWrites = Json.writes[RideRow]

def methods : String = {  
	model.tables.map { tbl  => 
s"""  
  implicit val ${tbl.name.table.toCamelCase.uncapitalize}RowReadsJson = Json.reads[${tbl.name.table.toCamelCase.capitalize}Row]
	implicit val ${tbl.name.table.toCamelCase.uncapitalize}RowWritesJson = Json.writes[${tbl.name.table.toCamelCase.capitalize}Row]
"""			  
	}.mkString("\n")
}

  override def writeToFile(folder:String, pkg: String, fileName: String= "JsonFormat.scala") {
      writeStringToFile(packageCode(pkg), folder, pkg, fileName)
    }
}
