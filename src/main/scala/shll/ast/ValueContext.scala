package shll.ast

import com.typesafe.scalalogging.Logger

case object ValueContext {
  val logger: Logger = Logger[this.type]
}
case class ValueContext(
                         private var values: Map[String, Ast] = Map.empty,
                         private var types: Map[String, DefType] = Map.empty,
                         private var functions: Map[String, DefFun] = Map.empty,
                         private var structs: Map[String, DefStruct] = Map.empty,
                         private val parent: Option[ValueContext] = None
) {
  def getParent: Option[this.type] = parent.asInstanceOf[Option[this.type]]

  def getValueShallow(name: String): Option[Ast] = {
    values.get(name)
  }
  def getValue(name: String): Option[Ast] = {
    values.get(name).orElse(parent.flatMap(_.getValue(name)))
  }
  def withValues(values: Map[String, Ast]): ValueContext = {
    ValueContext(values = values, parent = Some(this))
  }

  def withValue(name: String, value: Ast): ValueContext = {
//    ValueContext.logger.debug(s"Adding value $name -> $value in ${this.hashCode()}")
    withValues(Map(name -> value))
  }
  def updateValue(name: String, value: Ast): Unit = {
    if (values.contains(name)) {
      values = values + (name -> value)
    } else {
      parent match {
        case Some(p) => p.updateValue(name, value)
        case None => throw new RuntimeException(s"Cannot find value $name to update")
      }
    }
  }
  def getType(name: String): Option[Ast] = {
    types.get(name).orElse(parent.flatMap(_.getType(name)))
  }

  def withType(name: String, ty: DefType): ValueContext = {
    withTypes(Map(name -> ty))
  }
  def withTypes(types: Map[String, DefType]): ValueContext = {
    ValueContext(types = types, parent = Some(this))
  }
  def getFunction(name: String): Option[DefFun] = {
    functions.get(name).orElse(parent.flatMap(_.getFunction(name)))
  }
  def withFunction(name: String, func: DefFun): ValueContext = {
    ValueContext(functions = Map(name -> func), parent = Some(this))
  }
  def getStruct(name: String): Option[DefStruct] = {
    structs.get(name).orElse(parent.flatMap(_.getStruct(name)))
  }
  def withStruct(name: String, struct: DefStruct): ValueContext = {
    ValueContext(structs = Map(name -> struct), parent = Some(this))
  }
}
