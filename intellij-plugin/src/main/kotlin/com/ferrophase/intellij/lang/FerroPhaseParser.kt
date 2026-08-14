package com.ferrophase.intellij.lang

import com.intellij.lang.ASTNode
import com.intellij.lang.PsiBuilder
import com.intellij.lang.PsiParser
import com.intellij.psi.tree.IElementType

/**
 * Deliberately trivial "parser": builds a flat PSI tree (a single root node whose direct
 * children are the leaf tokens produced by [FerroPhaseLexer]). There is no grammar/AST —
 * this plugin only needs enough PSI structure to drive syntax highlighting.
 */
class FerroPhaseParser : PsiParser {
    override fun parse(root: IElementType, builder: PsiBuilder): ASTNode {
        val marker = builder.mark()
        while (!builder.eof()) {
            builder.advanceLexer()
        }
        marker.done(root)
        return builder.treeBuilt
    }
}
