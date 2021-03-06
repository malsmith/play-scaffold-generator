package generators.views

import generators.utils.GeneratorHelpers

import slick.model.{ForeignKey, Column}

trait FormViewGeneratorHelpers extends GeneratorHelpers{

  val columns : Seq[Column]

  val controllerName : String

  val formName : String

  val submitButtonText : String

  val formAction : String

  val autoIncDefaultValue : String = "@value"

  val foreignKeys : Seq[ForeignKey]

  def form = {
    s"""
  @form(routes.${controllerName}.${formAction}, 'role -> "form", 'class -> "col-sm-offset-2 col-sm-8") {

  ${formFields}

${actions}

}""".trim()
  }

  def formFields : String = {
    (columns map (col => packedField(formField(col)))).mkString("\n")
  }

  def packedField(field : String) = {
    s"""
<div class="form-group">
  ${field}
</div>
     """.trim
  }

  def formField(column : Column) = {
    if(isAutoIncColumn(column)){
      inputAutoIncFieldCode(standardColumnName(column.name))
    }
    else if(foreignKeys.exists(_.referencingColumns.head.name.equals(column.name))){
      val fk = foreignKeys.find(_.referencingColumns.head.name.equals(column.name)).get
      val formOptionsName = fk.referencedTable.table.toCamelCase.uncapitalize + "OptionsBy" + makeColumnsAndString(fk.referencedColumns)
      if(column.nullable) {
        selectOptionalCode(standardColumnName(column.name), formOptionsName)
      }
      else {
        selectCode(standardColumnName(column.name), formOptionsName)
      }
    }
    else {
      convertTypeToInput(column)
    }
  }

  def convertTypeToInput(column : Column) = {
    val (name, tpe) = (standardColumnName(column.name), column.tpe)
    tpe match {
      case "java.sql.Date" => inputDateCode(name)
      case "Boolean" => checkboxCode(name)
      case "java.sql.Blob" => inputFileCode(name)
      case "java.sql.Time" => inputTimeCode(name)
      case "java.sql.Timestamp" => inputDateTimeCode(name)
      case "java.sql.Clob" => inputFileCode(name)
      case _ => inputTextCode(name)
    }
  }

  def inputTextCode(fieldName : String) = {
    s"""@inputText(${formName}("${fieldName}"), '_label -> "${fieldName}", 'class -> "form-control")"""
  }

  def inputAutoIncFieldCode(fieldName : String) = {
    s"""
@input(${formName}("${fieldName}"), '_label -> None, '_showConstraints -> false) { (id, name, value, args) =>
    <input type="hidden" name="@name" id="@id" value="${autoIncDefaultValue}" @toHtmlArgs(args)>
}""".trim()
    }

  def inputDateCode(fieldName : String) = {
    s"""@inputDate(${formName}("${fieldName}"), '_label -> "${fieldName}", 'class -> "form-control")"""
  }

  def inputTimeCode(fieldName : String) = {
    s"""
@input(${formName}("${fieldName}"), '_label -> "${fieldName}", 'class -> "form-control") { (id, name, value, args) =>
    <input type="time" name="@name" id="@id" value="@value" @toHtmlArgs(args)>
}""".trim()
  }

  def inputDateTimeCode(fieldName : String) = {
    s"""
@input(${formName}("${fieldName}"), '_label -> "${fieldName}", 'class -> "form-control") { (id, name, value, args) =>
    <input type="datetime" name="@name" id="@id" value="@value" @toHtmlArgs(args)>
}""".trim()
  }

  def inputFileCode(fieldName : String) = {
    s"""@inputFile(${formName}("${fieldName}"), '_label -> "${fieldName}", 'class -> "form-control")"""
  }

  def checkboxCode(fieldName : String) = {
    s"""@checkbox(${formName}("${fieldName}"), '_label -> "${fieldName}", 'class -> "form-control")"""
  }

  def selectCode(fieldName : String, optionsName : String) = {
    s"""@select(${formName}("${fieldName}"), ${optionsName}, '_label -> "${fieldName}", 'class -> "form-control")"""
  }

  def selectOptionalCode(fieldName : String, optionsName : String) = {
    s"""@select(${formName}("${fieldName}"), ${optionsName}, '_label -> "${fieldName}", '_default -> "--${fieldName}--", 'class -> "form-control")"""
  }

  def actions = {
    s"""
<div class="form-group actions">
    <div class="col-sm-10">
        <button type="submit" class="btn btn-success">${submitButtonText}</button>
        <a href="@routes.${controllerName}.list" class="btn btn-default">Cancel</a>
    </div>
</div>
""".trim()
  }
}
