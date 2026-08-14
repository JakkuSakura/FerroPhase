package com.ferrophase.intellij.lang

import com.intellij.psi.tree.IElementType

class FerroPhaseTokenType(debugName: String) : IElementType(debugName, FerroPhaseLanguage) {
    override fun toString(): String = "FerroPhaseTokenType." + super.toString()
}

object FerroPhaseTokenTypes {
    @JvmField val KEYWORD = FerroPhaseTokenType("KEYWORD")
    @JvmField val IDENTIFIER = FerroPhaseTokenType("IDENTIFIER")
    @JvmField val STRING = FerroPhaseTokenType("STRING")
    @JvmField val CHAR = FerroPhaseTokenType("CHAR")
    @JvmField val NUMBER = FerroPhaseTokenType("NUMBER")
    @JvmField val LINE_COMMENT = FerroPhaseTokenType("LINE_COMMENT")
    @JvmField val BLOCK_COMMENT = FerroPhaseTokenType("BLOCK_COMMENT")
    @JvmField val MACRO_BANG = FerroPhaseTokenType("MACRO_BANG")
    @JvmField val OPERATOR = FerroPhaseTokenType("OPERATOR")
    @JvmField val PUNCTUATION = FerroPhaseTokenType("PUNCTUATION")
    @JvmField val ATTRIBUTE = FerroPhaseTokenType("ATTRIBUTE")
}
