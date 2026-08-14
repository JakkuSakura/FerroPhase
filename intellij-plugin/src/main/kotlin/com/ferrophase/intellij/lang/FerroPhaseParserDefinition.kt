package com.ferrophase.intellij.lang

import com.intellij.lang.ASTNode
import com.intellij.lang.ParserDefinition
import com.intellij.lang.PsiParser
import com.intellij.lexer.Lexer
import com.intellij.openapi.project.Project
import com.intellij.psi.FileViewProvider
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IFileElementType
import com.intellij.psi.tree.TokenSet
import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.extapi.psi.PsiFileBase
import com.intellij.openapi.fileTypes.FileType

class FerroPhaseParserDefinition : ParserDefinition {

    override fun createLexer(project: Project?): Lexer = FerroPhaseLexer()

    override fun createParser(project: Project?): PsiParser = FerroPhaseParser()

    override fun getFileNodeType(): IFileElementType = FILE

    override fun getWhitespaceTokens(): TokenSet = TokenSet.create(TokenType.WHITE_SPACE)

    override fun getCommentTokens(): TokenSet = TokenSet.create(
        FerroPhaseTokenTypes.LINE_COMMENT,
        FerroPhaseTokenTypes.BLOCK_COMMENT,
    )

    override fun getStringLiteralElements(): TokenSet = TokenSet.create(
        FerroPhaseTokenTypes.STRING,
        FerroPhaseTokenTypes.CHAR,
    )

    override fun createFile(viewProvider: FileViewProvider): PsiFile = FerroPhaseFile(viewProvider)

    override fun createElement(node: ASTNode): PsiElement = ASTWrapperPsiElement(node)

    companion object {
        @JvmField val FILE = IFileElementType(FerroPhaseLanguage)
    }
}

class FerroPhaseFile(viewProvider: FileViewProvider) : PsiFileBase(viewProvider, FerroPhaseLanguage) {
    override fun getFileType(): FileType = FerroPhaseFileType

    override fun toString(): String = "FerroPhase File"
}
