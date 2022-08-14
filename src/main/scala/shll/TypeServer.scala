package shll

trait TypeServer {
  def isList(ty: Type): Boolean
  def getListElementType(ty: Type): Type
  def isInt(ty: Type): Boolean
  def isBool(ty: Type): Boolean
  def isUnit(ty: Type): Boolean
  def isDecimal(ty: Type): Boolean
  def isString(ty: Type): Boolean
  def isChar(ty: Type): Boolean
  def isFunction(ty: Type): Boolean
  def getFunctionArgTypes(ty: Type): List[Type]
  def getFunctionKwArgTypes(ty: Type): List[Type]
  def isField(ty: Type): Boolean
  def getFieldName(ty: Type): String
  def getFieldType(ty: Type): Type
  def getFunctionReturnType(ty: Type): Type
  def isStruct(ty: Type): Boolean
  def getStructFieldTypes(ty: Type): List[(String, Type)]

  def toDebug(ty: Type): String
}
