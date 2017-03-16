package generators.models.slick

import generators.utils._

import slick.model.{Column, Table}

import slick.model.ForeignKeyAction._

object SlickDaoObjectGenerator {
  def generate(outputFolder : String) = {

    val appConfig = AppConfigParser.getAppConfig

    val pkg = appConfig.modelsPackage

    val model = new ModelProvider(appConfig).model

    val foreignKeyInfo = new ForeignKeyInfo(model)

    model.tables map { table =>
      new SlickDaoObjectGenerator(table, foreignKeyInfo).writeToFile(outputFolder, pkg)
    }
  }
}

class SlickDaoObjectGenerator(table : Table, foreignKeyInfo : ForeignKeyInfo) extends OutputHelpers with SlickDaoGeneratorHelpers with GeneratorHelpers {

  val mainTableInfo = new TableInfo(table)

  override val rowName: String = mainTableInfo.name.toLowerCase

  override val rowNameCamelCased : String = rowName.toCamelCase

  override val primaryKeyColumns: Seq[Column] = mainTableInfo.primaryKeyColumns

  override val tableRowName: String = mainTableInfo.tableRowName
  override val queryObjectName: String = mainTableInfo.queryObjectName

  val objectName = mainTableInfo.daoObjectName
  
  val modelObjectName = mainTableInfo.modelObjectName

  override val fieldsForSimpleName = {
    mainTableInfo.selectColumns.map{ col =>
      if(col.nullable) standardColumnName(col.name) + ".getOrElse(\"\")"
      else standardColumnName(col.name)
    }
  }

  override def code: String = objectCode

  override def indent(code: String): String = code

  def objectCode : String = {
    s"""
import javax.inject.{ Inject, Singleton }

import models.Tables.${modelObjectName}Row
import play.api.db.slick.{ DatabaseConfigProvider, HasDatabaseConfigProvider }
import slick.driver.JdbcProfile
import scala.concurrent.Future

@Singleton()
class ${modelObjectName}Dao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends Tables with HasDatabaseConfigProvider[JdbcProfile] {

${imports}

${methods}

}""".trim()
  }

  def imports : String = {
    Seq(importCode("driver.api._"))
        .mkString("\n")
  }

  def methods : String = {
    (fixedMethods ++ dynamicMethods).mkString("\n\n")
  }

  def fixedMethods : Seq[String] = {
    if(mainTableInfo.isSimpleJunctionTable) fixedMethodsForSimpleJunctionTable
    else fixedMethodsForTable
  }

  def fixedMethodsForTable : Seq[String] = {
    val cascadeChildData = foreignKeyInfo.foreignKeysReferencedTable(table.name).filter(fk => fk.onDelete.action == Restrict.action || fk.onDelete.action == NoAction.action).map{ fk =>
      val tab = new TableInfo(foreignKeyInfo.tablesByName(fk.referencingTable))
      (tab, fk)
    }

    val saveMethod : String= {
      val autoIncCol = primaryKeyColumns.find(isAutoIncColumn(_))
      autoIncCol match {
        case Some(col) => saveReturnIdMethodCode(col)
        case None => saveSimpleMethodCode
      }
    }

    saveMethod +: Seq(findByQueryMethodCode(primaryKeyColumns),
                      findByPrimaryKeyMethodCode,
                      updateMethodCode,
                      deleteMethodCode(cascadeChildData),
                      findAllMethodCode)
  }

  def fixedMethodsForSimpleJunctionTable: Seq[String] = {

    Seq(saveSimpleMethodCode,
        deleteJunctionMethodCode(table.foreignKeys))
  }

  def dynamicMethods : Seq[String] = {

    val tableChildrenInfo = foreignKeyInfo.parentChildrenTablesInfo(table.name)

    val columnsReferenced = foreignKeyInfo.foreignKeysReferencedTable(table.name).map(_.referencedColumns).distinct

    val formOptions = columnsReferenced.map(cols => formOptionsMethodCode(cols.head.name))

    val uniqueFindByMethods = columnsReferenced.filterNot(_.equals(primaryKeyColumns)).map(cols => Seq(findByQueryMethodCode(cols),
              findByUniqueMethodCode(cols),
              deleteByMethodCode(cols))).flatten

    val findByMethods = table.foreignKeys.map { fk =>
      val columns = fk.referencingColumns
      Seq(findByQueryMethodCode(columns),
          findByMethodCode(columns),
          deleteByMethodCode(columns))
    }.flatten

    val joinedByJunctionTableMethods = tableChildrenInfo.filter(tabInfo => tabInfo.isJunctionTable || tabInfo.isSimpleJunctionTable).map{ childTableInfo =>
      val foreignKeyToFirstSide = childTableInfo.foreignKeys.filter(_.referencedTable == table.name).head
      val foreignKeyToSecondSide = childTableInfo.foreignKeys.filter(_.referencedTable != table.name).head

      findByJunctionTableMethodsCode(childTableInfo, foreignKeyToFirstSide, foreignKeyToSecondSide)
    }

    formOptions ++ uniqueFindByMethods ++ findByMethods ++ joinedByJunctionTableMethods
  }

  override def writeToFile(folder:String, pkg: String, fileName: String= objectName +  ".scala") {
      writeStringToFile(packageCode(pkg), folder, pkg, fileName)
    }
}
