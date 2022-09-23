package shll.optimizers

import shll.ast.{AST, DefFun, DefStruct}

case class ValueContext(
    private val values: Map[String, AST] = Map.empty,
    private val tyValues: Map[String, AST] = Map.empty,
    private val functions: Map[String, DefFun] = Map.empty,
    private val structs: Map[String, DefStruct] = Map.empty,
    private val parent: Option[ValueContext] = None
) {
  def getParent: Option[this.type] = parent.asInstanceOf[Option[this.type]]
  def getValue(name: String): Option[AST] = {
    values.get(name).orElse(parent.flatMap(_.getValue(name)))
  }
  def withValues(values: Map[String, AST]): ValueContext = {
    ValueContext(values, tyValues, functions, structs, Some(this))
  }
  def getTyValue(name: String): Option[AST] = {
    tyValues.get(name).orElse(parent.flatMap(_.getTyValue(name)))
  }
  def withTyValues(tyValues: Map[String, AST]): ValueContext = {
      ValueContext(values, tyValues, functions, structs, Some(this))
  }
  def getFunction(name: String): Option[DefFun] = {
    functions.get(name).orElse(parent.flatMap(_.getFunction(name)))
  }
  def withFunctions(functions: Map[String, DefFun]): ValueContext = {
      ValueContext(values, tyValues, functions, structs, Some(this))
  }
  def getStruct(name: String): Option[DefStruct] = {
    structs.get(name).orElse(parent.flatMap(_.getStruct(name)))
  }
  def withStructs(structs: Map[String, DefStruct]): ValueContext = {
      ValueContext(values, tyValues, functions, structs, Some(this))
  }

  def from(
            values: Map[String, AST] = Map.empty,
            tyValues: Map[String, AST] = Map.empty,
            functions: Map[String, DefFun] = Map.empty,
            structs: Map[String, DefStruct] = Map.empty,
          ): ValueContext = {
    ValueContext(
      values,
      tyValues,
      functions,
      structs,
      Some(this)
    )
  }
}

