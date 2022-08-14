antlr4: src/main/antlr4/SHLL.g4
	antlr4 src/main/antlr4/SHLL.g4  -no-listener -no-visitor -package antlr4 -o target/antlr4/
	cp target/antlr4/src/main/antlr4/* src/main/java/antlr4/
