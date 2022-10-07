package shll.frontends

import shll.ast.*

object Builtins {
  val builtinAsts: Map[String, DeclFun] = Map(
    "if" -> AstHelper.declFun(
      "if",
      List(("cond", AstHelper.tBool), ("then", AstHelper.tAny), ("else", AstHelper.tAny)),
      AstHelper.tAny
    ),
    "while" -> AstHelper.declFun(
      "while",
      List(("cond", AstHelper.tBool), ("body", AstHelper.tAny)),
      AstHelper.tUnit
    ),
    "for" -> AstHelper.declFun(
      "for",
      List(
        ("variable", AstHelper.tIdent),
        ("iterable", AstHelper.tAny),
        ("body", AstHelper.tAny)
      ),
      AstHelper.tUnit
    ),
    "def-fun" -> AstHelper.declFun(
      "def-fun",
      List(
        ("name", AstHelper.tIdent),
        ("params", AstHelper.tParams),
        ("ret", AstHelper.tAny),
        ("body", AstHelper.tAny)
      ),
      AstHelper.tUnit
    ),
    "decl-fun" -> AstHelper.declFun(
      "decl-fun",
      List(
        ("name", AstHelper.tIdent),
        ("params", AstHelper.tParams),
        ("ret", AstHelper.tAny)
      ),
      AstHelper.tUnit
    ),
    "def-val" -> AstHelper.declFun(
      "def-val",
      List(
        ("name", AstHelper.tIdent),
        ("value", AstHelper.tAny)
      ),
      AstHelper.tUnit
    ),
    "def-type" -> AstHelper.declFun(
      "def-type",
      List(
        ("name", AstHelper.tIdent),
        ("params", AstHelper.tParams),
        ("value", AstHelper.tAny)
      ),
      AstHelper.tUnit
    ),
    "def-struct" -> AstHelper.declFun(
      "def-struct",
      List(
        ("name", AstHelper.tIdent),
        ("fields", AstHelper.tFields)
      ),
      AstHelper.tUnit
    ),
    "assign" -> AstHelper.declFun(
      "assign",
      List(
        ("name", AstHelper.tIdent),
        ("value", AstHelper.tAny)
      ),
      AstHelper.tUnit
    ),
    ":" -> AstHelper.declFun(
      ":",
      List(
        ("name", AstHelper.tIdent),
        ("value", AstHelper.tAny)
      ),
      AstHelper.tAny
    ),
    "select" -> AstHelper.declFun(
      "select",
      List(
        ("obj", AstHelper.tAny),
        ("field", AstHelper.tIdent)
      ),
      AstHelper.tAny
    ),
    "fun" -> AstHelper.declFun(
      "fun",
      List(
        ("params", AstHelper.tAny),
        ("returns", AstHelper.tAny),
        ("body", AstHelper.tAny)
      ),
      AstHelper.tAny
    )
    // block is special
    //    "block" -> AstHelper.defFun(
    //      "block",
    //      List(
    //        ("body", AstHelper.tAny)
    //      ),
    //      AstHelper.tUnit
    //    ),
    // list is special, lp, lf
    //    "list" -> AstHelper.defFun(
    //      "list",
    //      List(
    //        ("body", AstHelper.tAny)
    //      ),
    //      AstHelper.tList(AstHelper.tAny)
    //    ),
  )
  val builtinFunctions: Map[String, DeclFun] = Map(
    "==" -> AstHelper.declFun(
      "==",
      List(("lhs", AstHelper.tAny), ("rhs", AstHelper.tAny)),
      AstHelper.tBool
    ),
    "!=" -> AstHelper.declFun(
      "!=",
      List(("lhs", AstHelper.tAny), ("rhs", AstHelper.tAny)),
      AstHelper.tBool
    ),
    ">" -> AstHelper.declFun(
      ">",
      List(("lhs", AstHelper.tAny), ("rhs", AstHelper.tAny)),
      AstHelper.tBool
    ),
    ">=" -> AstHelper.declFun(
      ">=",
      List(("lhs", AstHelper.tAny), ("rhs", AstHelper.tAny)),
      AstHelper.tBool
    ),
    "<" -> AstHelper.declFun(
      "<",
      List(("lhs", AstHelper.tAny), ("rhs", AstHelper.tAny)),
      AstHelper.tBool
    ),
    "<=" -> AstHelper.declFun(
      "<=",
      List(("lhs", AstHelper.tAny), ("rhs", AstHelper.tAny)),
      AstHelper.tBool
    ),
    "+" -> AstHelper.declFun(
      "+",
      List(("lhs", AstHelper.tAny), ("rhs", AstHelper.tAny)),
      AstHelper.tAny
    ),
    "-" -> AstHelper.declFun(
      "-",
      List(("lhs", AstHelper.tAny), ("rhs", AstHelper.tAny)),
      AstHelper.tAny
    ),
    "*" -> AstHelper.declFun(
      "-",
      List(("lhs", AstHelper.tAny), ("rhs", AstHelper.tAny)),
      AstHelper.tAny
    ),
    "/" -> AstHelper.declFun(
      "-",
      List(("lhs", AstHelper.tAny), ("rhs", AstHelper.tAny)),
      AstHelper.tAny
    ),
    "%" -> AstHelper.declFun(
      "-",
      List(("lhs", AstHelper.tAny), ("rhs", AstHelper.tAny)),
      AstHelper.tAny
    ),
    "range" -> AstHelper.declFun(
      "-",
      List(("from", AstHelper.tInt), ("until", AstHelper.tInt)),
      AstHelper.tList(AstHelper.tInt)
    )
  )
  val builtinTypes: Map[String, DefType] = Map(
    "int" -> AstHelper.defType("int", Nil, AstHelper.tInt),
    "bool" -> AstHelper.defType("bool", Nil, AstHelper.tBool),
    "numeric" -> AstHelper.defType("numeric", Nil, AstHelper.tNumeric),
    "string" -> AstHelper.defType("string", Nil, AstHelper.tString),
    "char" -> AstHelper.defType("string", Nil, AstHelper.tChar),
    "list" -> AstHelper.defType("list", List("value"), AstHelper.tList(Ident("value"))),
    "fun" -> DefType(
      Ident("fun"),
      Params(
        List(
          ("params", AstHelper.tParams),
          ("ret", AstHelper.tAny)
        ).map(x => Param(Ident(x._1), x._2))
      ),
      AstHelper.tFun(Ident("params"), Ident("ret"))
    )
  )
}
