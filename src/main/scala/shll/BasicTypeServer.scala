package shll

case class TyList(ty: AST) extends AST
case class TyInt(v: Int) extends AST

case class BasicTypeServer () extends TypeServer {
  def isConcreteType(t: AST): Boolean = t match {
    case TyInt(_) => true
    case TyList(ty) => isConcreteType(ty)
    case _ => false
  }

  def isList(ty: AST): Boolean = ty.isInstanceOf[TyList]

  def getListElementType(ty: AST): AST = ty.asInstanceOf[TyList].ty

  def isInt(ty: AST): Boolean = ty.isInstanceOf[TyInt]

  def isBool(ty: AST): Boolean = false

  def isUnit(ty: AST): Boolean = false

  def isDecimal(ty: AST): Boolean = false

  def isString(ty: AST): Boolean = false

  def isChar(ty: AST): Boolean  = false

  def isFunction(ty: AST): Boolean = false

  def getFunctionArgTypes(ty: AST): List[AST] = Nil

  def getFunctionKwArgTypes(ty: AST): List[AST] = Nil

  def isField(ty: AST): Boolean = false

  def getFieldName(ty: AST): String = ""

  def getFieldType(ty: AST): AST = null

  def getFunctionReturnType(ty: AST): AST = null

  def isStruct(ty: AST): Boolean = false

  def getStructFieldTypes(ty: AST): List[(String, AST)] = Nil

  def toDebug(ty: AST): String = ty.toString
}
