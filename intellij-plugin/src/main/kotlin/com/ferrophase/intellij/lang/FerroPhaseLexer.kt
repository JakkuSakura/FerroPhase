package com.ferrophase.intellij.lang

import com.intellij.lexer.LexerBase
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType

/**
 * Hand-written, permissive lexer for FerroPhase (`.fp`) files.
 *
 * This is intentionally simple: it never throws on malformed input (anything it can't
 * classify falls back to [TokenType.BAD_CHARACTER]), and it builds a flat token stream
 * with no grammar/AST awareness. Good enough for syntax highlighting only.
 */
class FerroPhaseLexer : LexerBase() {

    private data class Token(val type: IElementType, val start: Int, val end: Int)

    private var buffer: CharSequence = ""
    private var bufferStart: Int = 0
    private var bufferEnd: Int = 0
    private var tokens: List<Token> = emptyList()
    private var index: Int = 0

    override fun start(buffer: CharSequence, startOffset: Int, endOffset: Int, initialState: Int) {
        this.buffer = buffer
        this.bufferStart = startOffset
        this.bufferEnd = endOffset
        this.tokens = tokenize(buffer, startOffset, endOffset)
        this.index = 0
    }

    override fun getState(): Int = 0

    override fun getTokenType(): IElementType? {
        if (index >= tokens.size) return null
        return tokens[index].type
    }

    override fun getTokenStart(): Int {
        if (index >= tokens.size) return bufferEnd
        return tokens[index].start
    }

    override fun getTokenEnd(): Int {
        if (index >= tokens.size) return bufferEnd
        return tokens[index].end
    }

    override fun advance() {
        if (index < tokens.size) index++
    }

    override fun getBufferSequence(): CharSequence = buffer

    override fun getBufferEnd(): Int = bufferEnd

    companion object {
        private val KEYWORDS = setOf(
            "quote", "splice", "const", "emit", "let", "fn", "gen", "if", "else", "with",
            "try", "catch", "finally", "loop", "while", "for", "in", "match", "mut",
            "await", "async", "unsafe", "return", "break", "continue", "move",
            "struct", "enum", "union", "type", "static", "opaque", "mod", "trait",
            "impl", "where", "use", "extern", "super", "crate", "as", "pub", "defer",
            "self", "Self", "true", "false",
        )

        // Longest-first so we greedily match multi-char operators before their prefixes.
        private val MULTI_PUNCT = listOf(
            "..=", "...", "<<=", ">>=", "..", "::", "=>", "->", "==", "!=", "<=", ">=",
            "&&", "||", "<<", ">>", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=",
        )

        private const val SINGLE_PUNCT = "=+-*/%&|^~@#$?:;,.()[]{}<>"

        private fun isIdentStart(c: Char) = c == '_' || c.isLetter()
        private fun isIdentContinue(c: Char) = c == '_' || c.isLetterOrDigit()

        private fun tokenize(buffer: CharSequence, start: Int, end: Int): List<Token> {
            val tokens = ArrayList<Token>()
            var i = start

            // Shebang on the very first line, e.g. `#!/usr/bin/env fp interpret`.
            if (i == start && i + 1 < end && buffer[i] == '#' && buffer[i + 1] == '!' &&
                !(i + 2 < end && buffer[i + 2] == '[')
            ) {
                val lineEnd = indexOfOrEnd(buffer, i, end, '\n')
                tokens.add(Token(FerroPhaseTokenTypes.LINE_COMMENT, i, lineEnd))
                i = lineEnd
            }

            while (i < end) {
                val c = buffer[i]

                // Whitespace
                if (c.isWhitespace()) {
                    val s = i
                    while (i < end && buffer[i].isWhitespace()) i++
                    tokens.add(Token(TokenType.WHITE_SPACE, s, i))
                    continue
                }

                // Line comments: //, ///, //!
                if (c == '/' && i + 1 < end && buffer[i + 1] == '/') {
                    val s = i
                    val lineEnd = indexOfOrEnd(buffer, i, end, '\n')
                    tokens.add(Token(FerroPhaseTokenTypes.LINE_COMMENT, s, lineEnd))
                    i = lineEnd
                    continue
                }

                // Block comments: /* ... */ (non-nested; tolerant of missing terminator)
                if (c == '/' && i + 1 < end && buffer[i + 1] == '*') {
                    val s = i
                    var j = i + 2
                    while (j < end && !(buffer[j] == '*' && j + 1 < end && buffer[j + 1] == '/')) j++
                    j = if (j < end) j + 2 else end
                    tokens.add(Token(FerroPhaseTokenTypes.BLOCK_COMMENT, s, j))
                    i = j
                    continue
                }

                // Attribute: #[ ... ] (also handles bare shebang-like `#` not at line start)
                if (c == '#') {
                    val s = i
                    if (i + 1 < end && buffer[i + 1] == '[') {
                        var j = i + 2
                        var depth = 1
                        while (j < end && depth > 0) {
                            when (buffer[j]) {
                                '[' -> depth++
                                ']' -> depth--
                                else -> {}
                            }
                            j++
                        }
                        tokens.add(Token(FerroPhaseTokenTypes.ATTRIBUTE, s, j))
                        i = j
                        continue
                    }
                    tokens.add(Token(FerroPhaseTokenTypes.PUNCTUATION, s, s + 1))
                    i++
                    continue
                }

                // Raw strings: r"...", r#"..."#, r##"..."##, ...
                if (c == 'r' && i + 1 < end && (buffer[i + 1] == '"' || buffer[i + 1] == '#')) {
                    val rawEnd = tryLexRawString(buffer, i, end)
                    if (rawEnd != null) {
                        tokens.add(Token(FerroPhaseTokenTypes.STRING, i, rawEnd))
                        i = rawEnd
                        continue
                    }
                }

                // Prefixed string/char literals: b"...", c"...", f"...", t"..."
                if ((c == 'b' || c == 'c' || c == 'f' || c == 't') && i + 1 < end && buffer[i + 1] == '"') {
                    val s = i
                    val strEnd = lexQuoted(buffer, i + 1, end, '"')
                    tokens.add(Token(FerroPhaseTokenTypes.STRING, s, strEnd))
                    i = strEnd
                    continue
                }

                // Cooked strings: "..."
                if (c == '"') {
                    val s = i
                    val strEnd = lexQuoted(buffer, i, end, '"')
                    tokens.add(Token(FerroPhaseTokenTypes.STRING, s, strEnd))
                    i = strEnd
                    continue
                }

                // Char literals / lifetimes: 'a', '\n', 'label
                if (c == '\'') {
                    val s = i
                    val charEnd = lexCharOrLifetime(buffer, i, end)
                    tokens.add(Token(FerroPhaseTokenTypes.CHAR, s, charEnd))
                    i = charEnd
                    continue
                }

                // Numbers
                if (c.isDigit()) {
                    val s = i
                    val numEnd = lexNumber(buffer, i, end)
                    tokens.add(Token(FerroPhaseTokenTypes.NUMBER, s, numEnd))
                    i = numEnd
                    continue
                }

                // Identifiers / keywords, including raw identifiers r#name
                if (c == 'r' && i + 1 < end && buffer[i + 1] == '#' &&
                    i + 2 < end && isIdentStart(buffer[i + 2])
                ) {
                    val s = i
                    var j = i + 2
                    while (j < end && isIdentContinue(buffer[j])) j++
                    tokens.add(Token(FerroPhaseTokenTypes.IDENTIFIER, s, j))
                    i = j
                    continue
                }
                if (isIdentStart(c)) {
                    val s = i
                    var j = i + 1
                    while (j < end && isIdentContinue(buffer[j])) j++
                    val text = buffer.subSequence(s, j).toString()

                    // Macro call: identifier immediately followed by `!` (but not `!=`).
                    if (j < end && buffer[j] == '!' && !(j + 1 < end && buffer[j + 1] == '=')) {
                        tokens.add(Token(FerroPhaseTokenTypes.IDENTIFIER, s, j))
                        tokens.add(Token(FerroPhaseTokenTypes.MACRO_BANG, j, j + 1))
                        i = j + 1
                        continue
                    }

                    val type = if (text in KEYWORDS) FerroPhaseTokenTypes.KEYWORD else FerroPhaseTokenTypes.IDENTIFIER
                    tokens.add(Token(type, s, j))
                    i = j
                    continue
                }

                // Multi-char operators (longest match first)
                val multi = MULTI_PUNCT.firstOrNull { op ->
                    i + op.length <= end && regionMatches(buffer, i, op)
                }
                if (multi != null) {
                    tokens.add(Token(FerroPhaseTokenTypes.OPERATOR, i, i + multi.length))
                    i += multi.length
                    continue
                }

                // Single-char punctuation/operators
                if (SINGLE_PUNCT.indexOf(c) >= 0) {
                    tokens.add(Token(FerroPhaseTokenTypes.PUNCTUATION, i, i + 1))
                    i++
                    continue
                }

                // Anything unrecognized (stray bytes, unsupported unicode, etc.)
                tokens.add(Token(TokenType.BAD_CHARACTER, i, i + 1))
                i++
            }

            return tokens
        }

        private fun regionMatches(buffer: CharSequence, start: Int, text: String): Boolean {
            for (k in text.indices) {
                if (buffer[start + k] != text[k]) return false
            }
            return true
        }

        private fun indexOfOrEnd(buffer: CharSequence, from: Int, end: Int, ch: Char): Int {
            var j = from
            while (j < end && buffer[j] != ch) j++
            return if (j < end) j + 1 else end
        }

        /** Lexes a `"`-delimited (or other quote char) literal starting at [start], handling `\` escapes. */
        private fun lexQuoted(buffer: CharSequence, start: Int, end: Int, quote: Char): Int {
            var j = start + 1
            while (j < end) {
                val c = buffer[j]
                if (c == '\\' && j + 1 < end) {
                    j += 2
                    continue
                }
                if (c == quote) {
                    return j + 1
                }
                if (c == '\n') {
                    // Unterminated on this line; stop here (permissive).
                    return j
                }
                j++
            }
            return end
        }

        /** Lexes `'a'`, `'\n'`, or a lifetime/label like `'static`. Never throws. */
        private fun lexCharOrLifetime(buffer: CharSequence, start: Int, end: Int): Int {
            var j = start + 1
            if (j < end && buffer[j] == '\\' && j + 1 < end) {
                j += 2
                if (j < end && buffer[j] == '\'') return j + 1
                return j
            }
            if (j < end && isIdentStart(buffer[j])) {
                // Could be a lifetime/label ('a) or a single-char literal ('a').
                val identStart = j
                var k = j
                while (k < end && isIdentContinue(buffer[k])) k++
                if (k < end && buffer[k] == '\'' && k == identStart + 1) {
                    // Single alnum char literal like 'a'
                    return k + 1
                }
                return k
            }
            if (j < end) {
                j++
                if (j < end && buffer[j] == '\'') return j + 1
                return j
            }
            return end
        }

        private fun lexNumber(buffer: CharSequence, start: Int, end: Int): Int {
            var j = start
            // Hex/oct/bin prefix
            if (buffer[j] == '0' && j + 1 < end && (buffer[j + 1] == 'x' || buffer[j + 1] == 'X' ||
                        buffer[j + 1] == 'b' || buffer[j + 1] == 'B' || buffer[j + 1] == 'o' || buffer[j + 1] == 'O')
            ) {
                j += 2
                while (j < end && (buffer[j].isLetterOrDigit() || buffer[j] == '_')) j++
                return j
            }
            while (j < end && (buffer[j].isDigit() || buffer[j] == '_')) j++
            // Fractional part, but don't swallow a range operator `..`
            if (j < end && buffer[j] == '.' && !(j + 1 < end && buffer[j + 1] == '.')) {
                val next = if (j + 1 < end) buffer[j + 1] else null
                if (next == null || next.isDigit() || !isIdentStart(next)) {
                    j++
                    while (j < end && (buffer[j].isDigit() || buffer[j] == '_')) j++
                }
            }
            // Exponent
            if (j < end && (buffer[j] == 'e' || buffer[j] == 'E')) {
                var k = j + 1
                if (k < end && (buffer[k] == '+' || buffer[k] == '-')) k++
                if (k < end && buffer[k].isDigit()) {
                    j = k
                    while (j < end && (buffer[j].isDigit() || buffer[j] == '_')) j++
                }
            }
            // Optional type suffix, e.g. i64, u8, f32
            if (j < end && buffer[j].isLetter()) {
                while (j < end && (buffer[j].isLetterOrDigit() || buffer[j] == '_')) j++
            }
            return j
        }

        /**
         * Attempts to lex a raw string `r"..."` or `r#"..."#` / `r##"..."##`.
         * Returns null (falls back to identifier lexing) if this isn't actually a raw string.
         */
        private fun tryLexRawString(buffer: CharSequence, start: Int, end: Int): Int? {
            var j = start + 1
            var hashes = 0
            while (j < end && buffer[j] == '#') {
                hashes++
                j++
            }
            if (j >= end || buffer[j] != '"') return null
            j++
            val closing = "\"" + "#".repeat(hashes)
            while (j < end) {
                if (buffer[j] == '"' && j + hashes <= end && regionMatches(buffer, j, closing)) {
                    return j + closing.length
                }
                j++
            }
            return end
        }
    }
}
