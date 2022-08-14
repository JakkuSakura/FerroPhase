package shll

case class TyList(ty: Type)
case class TyInt(v: Int)

case class BasicTypeServer () extends TypeServer {
  def isList(ty: Type): Boolean = ty.isInstanceOf[TyList]

  def getListElementType(ty: Type): Type = ty.asInstanceOf[TyList].ty

  def isInt(ty: Type): Boolean = ty.isInstanceOf[TyInt]

  def isBool(ty: Type): Boolean = false

  def isUnit(ty: Type): Boolean = false

  def isDecimal(ty: Type): Boolean = false

  def isString(ty: Type): Boolean = false

  def isChar(ty: Type): Boolean  = false

  def isFunction(ty: Type): Boolean = false

  def getFunctionArgTypes(ty: Type): List[Type] = Nil

  def getFunctionKwArgTypes(ty: Type): List[Type] = Nil

  def isField(ty: Type): Boolean = false

  def getFieldName(ty: Type): String = ""

  def getFieldType(ty: Type): Type = null

  def getFunctionReturnType(ty: Type): Type = null

  def isStruct(ty: Type): Boolean = false

  def getStructFieldTypes(ty: Type): List[(String, Type)] = Nil

  def toDebug(ty: Type): String = ty.toString
}
