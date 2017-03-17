package generators.models.slick

import generators.utils.{AppConfigParser, ModelProvider}

// trait ExtraCode extends Table(_) {
//	def codeEntityOnly: Seq[String] = definitions.flatMap(_.getEnabled).map(_.docWithCode)
// }

object TablesGenerator{
		
	
	def generate(outputFolder : String) = {

    val appConfig = AppConfigParser.getAppConfig

    val pkg = appConfig.modelsPackage

    val modelProvider = new ModelProvider(appConfig)

    val slickDriverPath = appConfig.slickDriverPath

    val mainModel = modelProvider.model
    val codegen = new slick.codegen.SourceCodeGenerator(mainModel) {
    	override def Table = new Table(_) {
	    	override def autoIncLastAsOption = true
				override def definitions = Seq[Def](EntityType, PlainSqlMapper, TableClass, TableValue )
				/** Generates the complete code for this table and its subordinate generators.
				    @group Basic customization overrides */
				override def code: Seq[String] = definitions.flatMap(_.getEnabled).map(_.docWithCode)
    	}
    	  
    	
    	override def code = {
    "import slick.model.ForeignKeyAction\n" +
    ( if(tables.exists(_.hlistEnabled)){
        "import slick.collection.heterogeneous._\n"+
        "import slick.collection.heterogeneous.syntax._\n"
      } else ""
    ) +
    ( if(tables.exists(_.PlainSqlMapper.enabled)){
        "// NOTE: GetResult mappers for plain SQL are only generated for tables where Slick knows how to map the types of all columns.\n"+
        "import slick.jdbc.{GetResult => GR}\n"
      } else ""
    ) +
    (if(ddlEnabled){
      "\n/** DDL for all tables. Call .create to execute. */" +
      (
        if(tables.length > 5)
          "\nlazy val schema: profile.SchemaDescription = Array(" + tables.map(_.TableValue.name + ".schema").mkString(", ") + ").reduceLeft(_ ++ _)"
        else if(tables.nonEmpty)
          "\nlazy val schema: profile.SchemaDescription = " + tables.map(_.TableValue.name + ".schema").mkString(" ++ ")
        else
          "\nlazy val schema: profile.SchemaDescription = profile.DDL(Nil, Nil)"
      ) +
      "\n@deprecated(\"Use .schema instead of .ddl\", \"3.0\")"+
      "\ndef ddl = schema" +
      "\n\n"
    } else "") +
    tables.map(_.code.mkString("\n")).mkString("\n\n")
  }

    	// "(?s)(// case class.*\n)/\\*\\* GetResult".r.findAllMatchIn(test).map(x => x.group(1))
    	// "(?s)(// case class.*\n)/\\*\\* GetResult".r.replaceAllIn(test, "/** GetResult")
    	// Seq[Def](EntityType).flatMap(_.getEnabled).map(_.docWithCode)
    	override def packageCode(profile: String, pkg: String, container: String, parentType: Option[String]) : String = {
    		val test = code
    		// /** Entity class storing rows of table
    		val caseClassCode = "(?s)(/\\*\\* Entity class storing rows of table.*?\n)/\\*\\* GetResult implicit for".r
    				.findAllMatchIn(test).map(x => x.group(1))
    				.mkString("\n")
    		val remainingCode = "(?s)(/\\*\\* Entity class storing rows of table.*?\n)/\\*\\* GetResult implicit for".r
    				.replaceAllIn(test, "/** GetResult")
      s"""
package ${pkg}
// AUTO-GENERATE Slick data model
/** Stand-alone Slick data model for immediate use */
object ${container}Def extends {
  val profile = $profile
} with ${container}
class ${container}Def extends {
  val profile = $profile
} with ${container}
// Case Classes HERE
${caseClassCode}
/** Slick data model trait for extension, choice of backend or usage in the cake pattern. (Make sure to initialize this late.) */
trait ${container}${parentType.map(t => s" extends $t").getOrElse("")} {
  val profile: slick.driver.JdbcProfile
  import profile.api._
  // SUPPORTING CONVERTERS
  ${indent(remainingCode)}
}
      """.trim()
    	}

    
    }
    codegen.writeToFile(slickDriverPath, outputFolder, pkg)
  }
}