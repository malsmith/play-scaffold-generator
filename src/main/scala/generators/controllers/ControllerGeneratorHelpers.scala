package generators.controllers

import generators.utils.{GeneratorHelpers, TableInfo, StringUtils}

import slick.model.{ForeignKey, Column}

trait ControllerGeneratorHelpers extends GeneratorHelpers{

  val tableName : String

  val controllerName : String

  val viewsPackage : String

  val daoObjectName : String

  val formName : String

  val primaryKeyColumns : Seq[Column]

  val parentDaoObjectsAndReferencedColumn : Seq[(TableInfo, Column)]

  val childsData : Seq[(TableInfo, ForeignKey)]

  def indexMethod = {
    s"""
def index = Action.async {
  Future(Redirect(routes.${controllerName}.list))
}""".trim()
  }

  def indexJunctionMethod = {
    s"""
def index = Action.async {
  Redirect(routes.${controllerName}.create)
}""".trim()
  }

  def listMethod = {
    s"""
def list = Action.async {
	${daoObjectName}.findAll.map { res =>
  	Ok(views.html.${viewsPackage}.list(res))
  }
}""".trim()
  }

  def createMethod = {
    s"""
def create = Action { implicit request =>
  Ok(views.html.${viewsPackage}.createForm(${formName}${formOptions}))
}""".trim()
  }

  def saveMethod = {

    val showArgs = makeArgsWithObjectWithoutTypes("row", primaryKeyColumns)

    s"""
def save = Action.async { implicit request =>
  ${formName}.bindFromRequest.fold(
      formWithErrors => {
      Future(BadRequest(views.html.${viewsPackage}.createForm(formWithErrors${formOptions})))
    },
    formData => {
      ${daoObjectName}.save(formData).map ( res =>
      	Redirect(routes.${controllerName}.show(res))
      )
    }
  )
}""".trim()
  }

  def saveJunctionMethod = {
    s"""
def save = Action.async { implicit request =>
  ${formName}.bindFromRequest.fold(
      formWithErrors => {
      Future(BadRequest(views.html.${viewsPackage}.createForm(formWithErrors${formOptions})))
    },
    formData => {
      ${daoObjectName}.save(formData).map( res =>
      	Redirect(routes.${controllerName}.create).flashing("success" -> "${tableName.toCamelCase} saved")
      )
    }
  )
}""".trim()
  }

  def showMethod = {
    s"""
def show(${makeArgsWithTypes(primaryKeyColumns)}) = Action.async {
  ${daoObjectName}.findByPrimaryKey(Some(${makeArgsWithoutTypes(primaryKeyColumns)})).map(res => res match {
    	case None => BadRequest("Not found")
    	case Some(obj) => {
    	  ${childsFinders}
      	Ok(views.html.${viewsPackage}.show(obj${showViewOptions}))
    }
  })
}""".trim()
  }

  def childsFinders = {
    childsData.map{ child =>
      childFinder(child._1, child._2)
    }.mkString("\n\t\t\t")
  }

  private def childFinder(child : TableInfo, fk : ForeignKey) = {
    val childName = child.nameCamelCasedUncapitalized + "sBy" + makeColumnsAndString(fk.referencingColumns)

    val findMethod = makeFindByMethodName(fk.referencingColumns)

    val findMethodArgs = makeArgsWithObjectWithoutTypes("obj", fk.referencedColumns)

    s"val ${childName} = ${child.daoObjectName}.${findMethod}(${findMethodArgs})"
  }

  def showUniqueMethod(columns : Seq[Column]) = {

    val showByMethodName = makeShowByMethodName(columns)

    val findByMethodName = makeFindByMethodName(columns)

    val showArgs = makeArgsWithObjectWithoutTypes("obj", primaryKeyColumns)

    s"""
def ${showByMethodName}(${makeArgsWithTypes(columns)}) = Action.async {
  ${daoObjectName}.${findByMethodName}(${makeArgsWithoutTypes(columns)}).map(res => res match {
  	case None => BadRequest("Not existed")
  	case Some(obj) => {
	      Redirect(routes.${controllerName}.show(${showArgs}))
    }
  })
}
""".trim()
  }

  def editMethod = {
    s"""
def edit(${makeArgsWithTypes(primaryKeyColumns)}) = Action.async {
  ${daoObjectName}.findByPrimaryKey(Some(${makeArgsWithoutTypes(primaryKeyColumns)})).map(res => res match {
  	case None => NotFound
  	case Some(obj) => {
	      Ok(views.html.${viewsPackage}.editForm(${formName}.fill(obj)${formOptions}))
    }
  })
}""".trim()
  }

  def updateMethod = {

    val showArgs = makeArgsWithObjectWithoutTypes("formData", primaryKeyColumns)

    s"""
def update = Action.async { implicit request =>
  ${formName}.bindFromRequest.fold(
      formWithErrors => {
      Future(BadRequest(views.html.${viewsPackage}.editForm(formWithErrors${formOptions})))
    },
    formData => {
      ${daoObjectName}.update(formData).map { res =>
      		// Go ahead
      }
    	Future(Redirect(routes.${controllerName}.show(${showArgs}.get)))
    }
  )
}""".trim()
  }

  def deleteMethod = {
    s"""
def delete(${makeArgsWithTypes(primaryKeyColumns)}) = Action.async {
  ${daoObjectName}.delete(Some(${makeArgsWithoutTypes(primaryKeyColumns)})).map(res =>
  	Redirect(routes.${controllerName}.list)
  )
}""".trim()
  }

  def deleteSimpleJunctionMethod(junctionTableInfo : TableInfo) = {

    val idColumns = junctionTableInfo.foreignKeys.map{ fk =>
      makeArgsWithTypes(fk.referencingColumns)
    }.mkString(", ")

    val deleteArgs = junctionTableInfo.foreignKeys.map{ fk =>
      makeArgsWithoutTypes(fk.referencingColumns)
    }.mkString(", ")

    val parentPk = junctionTableInfo.foreignKeys.filter(_.referencedTable.table.equals(tableName)).head.referencingColumns.head.name

    val parentPkName = standardColumnName(parentPk)

    s"""
def delete${junctionTableInfo.nameCamelCased}(${idColumns}) = Action.async {
  ${junctionTableInfo.daoObjectName}.delete(${deleteArgs})
  Redirect(routes.${controllerName}.show(${parentPkName}))
}
""".trim()
  }

  private def showViewOptions = {
    if(childsData.isEmpty){
      ""
    } else {
      ", " + childsData.map(data => data._1.nameCamelCasedUncapitalized + "sBy" + makeColumnsAndString(data._2.referencingColumns)).mkString(", ")
    }
  }

  private def formOptions = {
    if(parentDaoObjectsAndReferencedColumn.isEmpty){
      ""
    } else {
      ", " + parentDaoObjectsAndReferencedColumn.map(data => data._1.daoObjectName + ".formOptionsBy" + makeColumnsAndString(Seq(data._2))).mkString(", ")
    }
  }
}
