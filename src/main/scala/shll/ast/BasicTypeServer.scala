package shll.ast

import shll.ast.AST

case class TyList(ty: AST) extends AST
case class TyInt(v: Int) extends AST

case class BasicTypeServer () extends TypeServer {
  override def isConcreteType(t: AST): Boolean = t match {
    case TyInt(_) => true
    case TyList(ty) => isConcreteType(ty)
    case _ => false
  }

  override def isList(ty: AST): Boolean = ty.isInstanceOf[TyList]

  override def getListElementType(ty: AST): AST = ty.asInstanceOf[TyList].ty

  override def isInt(ty: AST): Boolean = ty.isInstanceOf[TyInt]

  override def isBool(ty: AST): Boolean = false

  override def isUnit(ty: AST): Boolean = false

  override def isDecimal(ty: AST): Boolean = false

  override def isString(ty: AST): Boolean = false

  override def isChar(ty: AST): Boolean  = false

  override def isFunction(ty: AST): Boolean = false

  override def getFunctionArgTypes(ty: AST): List[AST] = Nil

  override def getFunctionKwArgTypes(ty: AST): List[AST] = Nil

  override def isField(ty: AST): Boolean = false

  override def getFieldName(ty: AST): String = ""

  override def getFieldType(ty: AST): AST = null

  override def getFunctionReturnType(ty: AST): AST = null

  override def isStruct(ty: AST): Boolean = false

  override def getStructFieldTypes(ty: AST): List[(String, AST)] = Nil

  override def toDebug(ty: AST): String = ty.toString
}
