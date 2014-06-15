package generators.slick.views

import generators.utils.GeneratorHelpers

import scala.slick.model.{Table, Column}
import generators.slick.utils.TableInfo

class ListViewGenerator(table : Table) extends ViewHelpers with GeneratorHelpers{

  val tableInfo = new TableInfo(table)

  val columns: Seq[Column] = tableInfo.columns

  val tableName = tableInfo.name

  override val title: String = tableName + " list"

  val tableRowName = tableInfo.tableRowName

  val listName = tableInfo.listName

  val controllerName = tableInfo.controllerName

  val primaryKeyName = tableInfo.primaryKeyName

  val fieldsAmount = 5

  override val arguments = Seq((listName, "List[Tables." + tableRowName + "]"))

  override def imports: String = ""

  override def bodyCode: String = {
    s"""
<div class="panel panel-default">
    <div class="panel-heading">
        <h3 class="panel-title">${listName.toUpperCase}</h3>
        <div class="pull-right btn-group">
            <a href="@routes.${controllerName}.create" class="btn btn-primary btn-xs">Add new ${tableName}</a>
        </div>
    </div>
    <div class="panel-body">
        <table class="table table-hover table-bordered table-responsive table-middle">
            <thead>
                ${headers}
            </thead>
            <tbody>
            @for(${tableName} <- ${listName}) {
                <tr>
                    ${rows}
                    <td class="text-center">
                        ${buttons}
                    </td>
                </tr>
            }
            </tbody>
        </table>
    </div>
</div>
""".trim()
  }

  def headers = {
    (columns.take(fieldsAmount).map("<th>" + _.name + "</th>") :+ "<th class=\"text-center\">Actions</th>").mkString("\n")
  }

  def rows = {
    (columns.take(fieldsAmount) map { col =>
      if(col.nullable)  printOptionalField(standardColumnName(col.name))
      else printField(standardColumnName(col.name))
    }).mkString("\n")
  }

  def printField(field : String) = {
    s"<td>@${tableName}.${field}</td>"
  }

  def printOptionalField(field : String) = {
    s"""
<td>@${tableName}.${field}.map { ${field} =>
      @${field}
    }
</td>
""".trim()
  }

  def buttons = {
    s"""
<div class="btn-group">
  <a href="@routes.${controllerName}.show(${tableName}.${primaryKeyName})" class="btn btn-success">Show</a>
  <a href="@routes.${controllerName}.edit(${tableName}.${primaryKeyName})" class="btn btn-warning">Edit</a>
  <a href="@routes.${controllerName}.delete(${tableName}.${primaryKeyName})" class="btn btn-danger">Delete</a>
</div>
""".trim()
  }

}
