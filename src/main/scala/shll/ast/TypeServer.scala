package shll.ast

trait TypeServer {
  def isConcreteType(t: AST): Boolean = ???
  def isList(ty: AST): Boolean = ???
  def getListElementType(ty: AST): AST = ???
  def isInt(ty: AST): Boolean = ???
  def isBool(ty: AST): Boolean = ???
  def isUnit(ty: AST): Boolean = ???
  def isDecimal(ty: AST): Boolean = ???
  def isString(ty: AST): Boolean = ???
  def isChar(ty: AST): Boolean = ???
  def isFunction(ty: AST): Boolean = ???
  def getFunctionArgTypes(ty: AST): List[AST] = ???
  def getFunctionKwArgTypes(ty: AST): List[AST] = ???
  def isField(ty: AST): Boolean = ???
  def getFieldName(ty: AST): String = ???
  def getFieldType(ty: AST): AST = ???
  def getFunctionReturnType(ty: AST): AST = ???
  def isStruct(ty: AST): Boolean = ???
  def getStructFieldTypes(ty: AST): List[(String, AST)] = ???

  def toDebug(ty: AST): String = ???
}
