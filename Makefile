antlr: src/main/antlr4/SHLL.g4
	antlr src/main/antlr4/SHLL.g4 -package shll.frontends -o target/antlr4/
	mkdir -p src/main/java/shll/frontends
	cp target/antlr4/src/main/antlr4/* src/main/java/shll/frontends/

antlr4: src/main/antlr4/SHLL.g4
	antlr4 src/main/antlr4/SHLL.g4 -package shll.frontends -o target/antlr4/
	mkdir -p src/main/java/shll/frontends
	cp target/antlr4/src/main/antlr4/* src/main/java/shll/frontends/
