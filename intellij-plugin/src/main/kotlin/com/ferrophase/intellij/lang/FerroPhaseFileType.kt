package com.ferrophase.intellij.lang

import com.intellij.openapi.fileTypes.LanguageFileType
import javax.swing.Icon

object FerroPhaseFileType : LanguageFileType(FerroPhaseLanguage) {
    override fun getName(): String = "FerroPhase"

    override fun getDescription(): String = "FerroPhase source file"

    override fun getDefaultExtension(): String = "fp"

    override fun getIcon(): Icon? = null
}
