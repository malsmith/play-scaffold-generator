package generators.models.slick

import generators.utils.{TableInfo, GeneratorHelpers}

import slick.model.{Column, ForeignKey}

trait SlickDaoGeneratorHelpers extends GeneratorHelpers{

  val rowName : String

  val rowNameCamelCased : String

  val tableRowName : String

  val primaryKeyColumns : Seq[Column]

  val queryObjectName : String

  val fieldsForSimpleName : Seq[String]
  
  def saveReturnIdMethodCode(autoIncId : Column) = {
  	val tmpColumns = List(autoIncId)
  	val returnType = makeArgsTypes(tmpColumns)  
  	val idNames = if (tmpColumns.length == 1) makeArgsWithObjectWithoutTypes("_", tmpColumns) else "(" +  makeArgsWithObjectWithoutTypes("row", tmpColumns)+")" 

  	s"""
def save(${rowName}: ${tableRowName}) : Future[$returnType] = db.run {
  this.${queryObjectName} += ${rowName}
}
def insertAll(${rowName}List: List[${tableRowName}]) : Future[Seq[$returnType]] = db.run {
	val tmpInsert = this.${queryObjectName} returning this.${queryObjectName}.map(${idNames})
  tmpInsert ++= ${rowName}List
}""".trim()
  }

  def saveSimpleMethodCode = {
/*
 * def save(employee: EmployeeRow) : EmployeeRow = {
  Employee returning Employee.map(_.id) into((row, id) => row.copy(id = id)) insert(employee)
}
	def save(employee: EmployeeRow): Future[Int] = db.run {
		this.Employee += employee
		// Employee += employee
		// Employee returning Employee.map(_.id) into((row, id) => row.copy(id = id)) insert(employee)
	}

 * 
 */
  	val returnType = if (primaryKeyColumns.length == 1) makeArgsTypes(primaryKeyColumns) else "(" + makeArgsTypes(primaryKeyColumns)+")" 
  	val idNames = if (primaryKeyColumns.length == 1) makeArgsWithObjectWithoutTypes("_", primaryKeyColumns) else "(" +  makeArgsWithObjectWithoutTypes("row", primaryKeyColumns)+")" 
  	
    s"""
def save(${rowName}: ${tableRowName}) : Future[$returnType] = db.run {
	this.${queryObjectName} returning this.${queryObjectName}.map(_.id)
  this.${queryObjectName} += ${rowName}
}

def insertAll(${rowName}List: Seq[${tableRowName}]) : Future[Seq[$returnType]] = db.run {
	val tmpInsert = this.${queryObjectName} returning this.${queryObjectName}.map(${idNames})
  tmpInsert ++= ${rowName}List
}""".trim()
  }
  
  def findAllMethodCode = {
    s"""
def findAll = db.run {
  ${queryObjectName}.to[List].result
}""".trim()
  }
  
  def findByPrimaryKeyMethodCode = {

    val methodArgs = makeArgsWithTypesAutoInc(primaryKeyColumns)

    val queryName = makeFindByQueryCompiledMethodName(primaryKeyColumns)

    val queryArgs = makeArgsWithoutTypes(primaryKeyColumns)
/*
 * 	def findByIdQuery(id: Int) = db.run {
		this.Employee.filter(_.id === id).result.headOption
	}
def findByIdQuery(id : Column[Int]) = {
  Employee.filter(row => row.id === id)
}
 * 
 */
    s"""
def findByPrimaryKey(${methodArgs}) = db.run {
  this.${queryName}(${queryArgs}).result.headOption
}""".trim()
  }
  
  def deleteMethodCode(childData : Seq[(TableInfo, ForeignKey)]) = {

    val methodArgs = makeArgsWithTypesAutoInc(primaryKeyColumns)

    val queryArgs = makeArgsWithoutTypes(primaryKeyColumns)

    val findQuery = makeFindByQueryCompiledMethodName(primaryKeyColumns)

    val childsCode = {
      if(childData.nonEmpty){
s"""
  val objOption = findByPrimaryKey(${queryArgs})

  objOption match {
    case Some(obj) => {
      ${childData.map(child => deleteChilds(child._1, child._2)).mkString("\n\t\t\t")}
    }
    case None => "Not finded"
  }
""".trim()
      } else ""
    }
    // ${childsCode}
    s"""
def delete(${methodArgs}) = db.run {
  ${findQuery}(${queryArgs}).delete
}""".trim()
  }

  def deleteChilds(childTabInfo : TableInfo, fk : ForeignKey) = {

    val deleteQuery = makeDeleteByMethodName(fk.referencingColumns)

    val queryArgs = makeArgsWithObjectWithoutTypes("obj", fk.referencedColumns)

    s"${childTabInfo.daoObjectName}.${deleteQuery}(${queryArgs})"
  }

  def deleteJunctionMethodCode(foreignKeys : Seq[ForeignKey]) = {

    val idColumns = foreignKeys.map{ fk =>
      makeArgsWithTypes(fk.referencingColumns)
    }.mkString(", ")

    val findingColumns = foreignKeys.map{ fk =>
      makeSlickRowComparing(fk.referencingColumns)
    }.mkString(" && ")

    s"""
def delete(${idColumns}) = db.run {

  val queryFindById = for {
      row <- ${queryObjectName} if ${findingColumns}
    } yield row

  queryFindById.delete
}""".trim()
  }
  
  def updateMethodCode = {

    val queryName = makeFindByQueryCompiledMethodName(primaryKeyColumns)

    val queryArgs = makeArgsWithObjectWithoutTypes("updatedRow", primaryKeyColumns)
    val rowArgs = makeArgsWithObjectWithoutTypes("_", primaryKeyColumns)

    s"""
def update(updatedRow: ${tableRowName}) = db.run {
  this.${queryObjectName}.filter(${rowArgs} === ${queryArgs}).update(updatedRow)
}""".trim()
  }

  def findByQueryMethodCode(columns : Seq[Column]) = {

    val args = makeArgsWithTypesAutoInc(columns)

    val rowComparingArgs = makeSlickRowComparing(columns)

    val methodName = makeFindByQueryMethodName(columns)

    val compiledName = makeFindByQueryCompiledMethodName(columns)
    
    val compiledArgs = makeArgsWithoutTypes(columns)
/*
 * 	def findByIdQuery(id: Int) = db.run {
		this.Employee.filter(_.id === id).result.headOption
	}
def findByIdQuery(id : Column[Int]) = {
  Employee.filter(row => row.id === id)
}
 * 
 */
  s"""
def ${compiledName}(${args}) = {
  this.${queryObjectName}.filter(row => ${rowComparingArgs})
}

def ${methodName}(${args}) = db.run {
  ${compiledName}(${compiledArgs}).result.headOption
}
""".trim()
  }

  def findByMethodCode(columns : Seq[Column]) = {

    val args = makeArgsWithTypes(columns)

    val methodName = makeFindByMethodName(columns)

    val compiledName = makeFindByQueryCompiledMethodName(columns)

    val compiledArgs = makeArgsWithoutTypes(columns)

  s"""
def ${methodName}(${args}) = db.run {
  ${compiledName}(${compiledArgs}).to[List].result
}
""".trim()
  }

  def findByUniqueMethodCode(columns : Seq[Column]) = {

    val args = makeArgsWithTypesAutoInc(columns)

    val methodName = makeFindByMethodName(columns)

    val compiledName = makeFindByQueryCompiledMethodName(columns)

    val compiledArgs = makeArgsWithoutTypes(columns)

  s"""
def ${methodName}(${args}) = db.run {
  ${compiledName}(${compiledArgs}).result.headOption
}
""".trim()
  }

  def deleteByMethodCode(columns : Seq[Column]) = {

    val args = makeArgsWithTypesAutoInc(columns)

    val methodName = makeDeleteByMethodName(columns)

    val compiledName = makeFindByQueryCompiledMethodName(columns)

    val compiledArgs = makeArgsWithoutTypes(columns)

  s"""
def ${methodName}(${args}) = db.run {
	${compiledName}(${compiledArgs}).delete
}
""".trim()
  }

  def findByJunctionTableMethodsCode(junctionTableInfo : TableInfo, foreignKeyToFirstSide : ForeignKey, foreignKeyToSecondSide : ForeignKey) = {

    val secondSideReferencingColumns = foreignKeyToSecondSide.referencingColumns

    val queryName = makeFindByQueryMethodName(secondSideReferencingColumns)

    val queryArgs = makeArgsWithColumnTypes(secondSideReferencingColumns)

    val junctionRow = junctionTableInfo.name.uncapitalize

    val joiningColumns = {
      "row => " + ((foreignKeyToFirstSide.referencedColumns.map(_.name) zip foreignKeyToFirstSide.referencingColumns.map(_.name)).map{
        case (lcol,rcol) => "row." + standardColumnName(lcol) + " === " + junctionRow + "." + standardColumnName(rcol)
      }.mkString(" && "))
    }

    val findJunctionArgs = makeArgsWithoutTypes(secondSideReferencingColumns)

    val resultListName = rowNameCamelCased.uncapitalize+"s"

    val compiledName = makeFindByQueryCompiledMethodName(secondSideReferencingColumns)

    val findByMethodName = makeFindByMethodName(secondSideReferencingColumns)

    val findByMethodArgs = makeArgsWithTypes(secondSideReferencingColumns)

  s"""
def ${queryName}(${queryArgs}) = {
  for {
    ${junctionRow} <- ${junctionTableInfo.daoObjectName}.${queryName}(${findJunctionArgs})
    ${resultListName} <- ${queryObjectName}.filter(${joiningColumns})
  } yield ${resultListName}
}

val ${compiledName} = Compiled(${queryName} _)

def ${findByMethodName}(${findByMethodArgs}) : List[${tableRowName}] = {
  ${compiledName}(${findJunctionArgs}).list
}
""".trim()
  }

  def formOptionsMethodCode(colName : String) = {

    val id = standardColumnName(colName)

    val byName = id.capitalize

    s"""
def formOptionsBy${byName} : Future[List[(String, String)]] = {
	val result = Await.result(db.run(${queryObjectName}.to[List].result),Duration.Inf)
  Future(result.map{ row =>
    (row.${id}.get.toString, ${fieldsForSimpleName.map("row." + _).mkString(" + \" \" + ")})
  })
}""".trim()
  }
  
}
