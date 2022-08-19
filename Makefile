antlr4: src/main/antlr4/SHLL.g4
	antlr src/main/antlr4/SHLL.g4  -no-listener -no-visitor -package shll -o target/antlr4/
	mkdir -p src/main/java/shll/
	cp target/antlr4/src/main/antlr4/* src/main/java/shll/
