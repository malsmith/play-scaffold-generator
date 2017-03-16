package generators.models.slick

import generators.utils.{AppConfigParser, ModelProvider}

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
    	}
    }
    codegen.writeToFile(slickDriverPath, outputFolder, pkg)
  }
}