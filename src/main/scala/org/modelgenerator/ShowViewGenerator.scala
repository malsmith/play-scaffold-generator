package org.modelgenerator

import scala.slick.model.{Column, Table}

class ShowViewGenerator(table : Table) extends GeneratorHelpers with ViewHelpers {

  override val columns: Seq[Column] = table.columns

  val tableName = table.name.table

  override val title: String = "Show " + tableName

  val tableRowName = tableName.toCamelCase + "Row"

  val controllerName = tableName.toCamelCase + "Controller"

  override val arguments = Seq((tableName, "Tables." + tableRowName))

  override def imports: String = ""

  override def bodyCode: String = {
    s"""
<h2>${tableName}</h2>
${fields}

${buttons}
""".trim()
  }

  def fields = {
    (columns map {col =>
      if(col.nullable) s"<p>${col.name} : ${printOptionalField(col.name)}</p>"
      else s"<p>${col.name} : @${tableName}.${col.name}</p>"
    }).mkString("\n")
  }

  def printOptionalField(field : String) = {
    s"""
@${tableName}.${field}.map { ${field} =>
          @${field}
        }
""".trim()
  }

  def buttons = {
    s"""
<div class="btn-group">
  <a href="@routes.${controllerName}.list" class="btn btn-success">List</a>
  <a href="@routes.${controllerName}.edit(${tableName}.${primaryKeyName})" class="btn btn-info">Edit</a>
  <a href="@routes.${controllerName}.delete(${tableName}.${primaryKeyName})" class="btn btn-danger">Delete</a>
</div>
""".trim()
  }
}