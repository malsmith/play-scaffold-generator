package generators.utils

import slick.model.{Column, ForeignKey, Table}

class TableInfo(val table : Table) extends GeneratorHelpers{

  val columns : Seq[Column] = table.columns

  val foreignKeys : Seq[ForeignKey] = table.foreignKeys

  val name : String = table.name.table

  val nameCamelCased : String = name.toCamelCase

  val nameCamelCasedUncapitalized : String = nameCamelCased.uncapitalize

  val listName : String = nameCamelCasedUncapitalized + "s"

  val daoObjectName : String = nameCamelCased + "Dao"

  val modelObjectName : String = nameCamelCased.capitalize

  val tableRowName : String = nameCamelCased + "Row"

  val queryObjectName : String = nameCamelCased

  val parserName : String = nameCamelCased + "Parser"

  val primaryKeyOpt = columns.find(_.options.contains(slick.ast.ColumnOption.PrimaryKey))

  val primaryKeyColumns : Seq[Column]= {
    table.primaryKey match {
      case Some(pk) => pk.columns
      case None => primaryKeyOpt match {
        case Some(col) => Seq(col)
        case None => Seq(columns.head)
      }
    }
  }

  val formName = nameCamelCasedUncapitalized + "Form"

  val controllerName = nameCamelCased + "Controller"

  val viewsPackage = name.toLowerCase

  val tableConfig = TablesConfigParser.getTableConfigForName(name)

  lazy val listColumns : Seq[Column] = getListColumns

  private def getListColumns : Seq[Column] = {

    val listCols : Option[Seq[Column]] = tableConfig.map( cfg => cfg.listColumns.map(mapColumnNamesToColumns(_))).flatten

    listCols match {
      case Some(cols) if cols.nonEmpty => cols
      case Some(cols) if cols.isEmpty => columns.take(5)
      case None => columns.take(5)
    }
  }

  lazy val selectColumns : Seq[Column] = getSelectColumns

  private def getSelectColumns : Seq[Column] = {

    val selectCols : Option[Seq[Column]] = tableConfig.map( cfg => cfg.selectColumns.map(mapColumnNamesToColumns(_))).flatten

    selectCols match {
      case Some(cols) if cols.nonEmpty => cols
      case Some(cols) if cols.isEmpty => columns.take(5)
      case None => columns.take(5)
    }
  }

  private def mapColumnNamesToColumns(names : List[String]) : Seq[Column] = {
    val filteredColumns = names.map{ colName =>
      columns.find(_.name == colName)
    }.filter(_.isDefined).map(_.get)

    filteredColumns
  }

  lazy val isJunctionTable = getIsJunctionTable

  private def getIsJunctionTable : Boolean = {
    val isJunctionFromConfig : Option[Boolean] = tableConfig.map( cfg => cfg.isJunctionTable.map(value => value)).flatten

    isJunctionFromConfig match {
      case Some(isJunction) => isJunction
      case None => false
    }
  }

  val isSimpleJunctionTable = defaultIsJunctionTableCheck

  private def defaultIsJunctionTableCheck : Boolean = table.primaryKey.isEmpty && !columns.exists(_.options.contains(slick.ast.ColumnOption.PrimaryKey)) && foreignKeys.length == 2
import slick.ast.ColumnOption
  
  lazy val desiredColumnOrder: Seq[Int] = {
	  val withIndex = columnsPositional.zipWithIndex
	  val cc  = withIndex(0)._1
	  cc.options.contains(ColumnOption.AutoInc)
	  if(true)
    // put auto inc column last
	    (withIndex.filterNot( _._1.options.contains(ColumnOption.AutoInc )) ++ withIndex.filter( _._1.options.contains(ColumnOption.AutoInc))).map(_._2)
	  else
    	withIndex.map(_._2)
	}
final lazy val columnsPositional: IndexedSeq[Column] = this.columns.toIndexedSeq
/** Column code generators in the desired user-facing order. */
final lazy val autoIncColumns: Seq[Column] = desiredColumnOrder.map(columnsPositional)
/** Column code generators indexed by db column name */
final lazy val columnsByName: Map[String,Column] = columns.map(c => c.name -> c).toMap
/** Primary key code generator, if this table has one */
// final lazy val primaryKey: Option[PrimaryKey] = primaryKey.map(PrimaryKey)
/** Foreign key code generators */
// final lazy val foreignKeys: Seq[ForeignKey] = model.foreignKeys.map(ForeignKey)
/** Index code generators */
//final lazy val indices: Seq[Index] = model.indices.map(Index)

  
/*  
  val primaryKeyColumns : Seq[Column]= {
    table.primaryKey match {
      case Some(pk) => pk.columns
      case None => primaryKeyOpt match {
        case Some(col) => Seq(col)
        case None => Seq(columns.head)
      }
    }
  }

  */
}
