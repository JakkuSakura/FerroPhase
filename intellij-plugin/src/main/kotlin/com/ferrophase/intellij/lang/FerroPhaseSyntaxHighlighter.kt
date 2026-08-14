package com.ferrophase.intellij.lang

import com.intellij.lexer.Lexer
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors
import com.intellij.openapi.editor.HighlighterColors
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType

class FerroPhaseSyntaxHighlighter : SyntaxHighlighterBase() {

    override fun getHighlightingLexer(): Lexer = FerroPhaseLexer()

    override fun getTokenHighlights(tokenType: IElementType): Array<TextAttributesKey> {
        val key = when (tokenType) {
            FerroPhaseTokenTypes.KEYWORD -> KEYWORD
            FerroPhaseTokenTypes.IDENTIFIER -> IDENTIFIER
            FerroPhaseTokenTypes.STRING -> STRING
            FerroPhaseTokenTypes.CHAR -> CHAR
            FerroPhaseTokenTypes.NUMBER -> NUMBER
            FerroPhaseTokenTypes.LINE_COMMENT -> LINE_COMMENT
            FerroPhaseTokenTypes.BLOCK_COMMENT -> BLOCK_COMMENT
            FerroPhaseTokenTypes.MACRO_BANG -> MACRO_BANG
            FerroPhaseTokenTypes.OPERATOR -> OPERATOR
            FerroPhaseTokenTypes.PUNCTUATION -> PUNCTUATION
            FerroPhaseTokenTypes.ATTRIBUTE -> ATTRIBUTE
            TokenType.BAD_CHARACTER -> BAD_CHARACTER
            else -> null
        }
        return if (key != null) arrayOf(key) else TextAttributesKey.EMPTY_ARRAY
    }

    companion object {
        @JvmField val KEYWORD = createTextAttributesKey(
            "FERROPHASE_KEYWORD", DefaultLanguageHighlighterColors.KEYWORD)
        @JvmField val IDENTIFIER = createTextAttributesKey(
            "FERROPHASE_IDENTIFIER", DefaultLanguageHighlighterColors.IDENTIFIER)
        @JvmField val STRING = createTextAttributesKey(
            "FERROPHASE_STRING", DefaultLanguageHighlighterColors.STRING)
        @JvmField val CHAR = createTextAttributesKey(
            "FERROPHASE_CHAR", DefaultLanguageHighlighterColors.STRING)
        @JvmField val NUMBER = createTextAttributesKey(
            "FERROPHASE_NUMBER", DefaultLanguageHighlighterColors.NUMBER)
        @JvmField val LINE_COMMENT = createTextAttributesKey(
            "FERROPHASE_LINE_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT)
        @JvmField val BLOCK_COMMENT = createTextAttributesKey(
            "FERROPHASE_BLOCK_COMMENT", DefaultLanguageHighlighterColors.BLOCK_COMMENT)
        @JvmField val MACRO_BANG = createTextAttributesKey(
            "FERROPHASE_MACRO_BANG", DefaultLanguageHighlighterColors.METADATA)
        @JvmField val OPERATOR = createTextAttributesKey(
            "FERROPHASE_OPERATOR", DefaultLanguageHighlighterColors.OPERATION_SIGN)
        @JvmField val PUNCTUATION = createTextAttributesKey(
            "FERROPHASE_PUNCTUATION", DefaultLanguageHighlighterColors.DOT)
        @JvmField val ATTRIBUTE = createTextAttributesKey(
            "FERROPHASE_ATTRIBUTE", DefaultLanguageHighlighterColors.METADATA)
        @JvmField val BAD_CHARACTER = createTextAttributesKey(
            "FERROPHASE_BAD_CHARACTER", HighlighterColors.BAD_CHARACTER)
    }
}
