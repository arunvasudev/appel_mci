all: TigerLexerTest.exe

TigerLexer.fs: TigerLexer.fsl
	fslex.exe --unicode TigerLexer.fsl

TigerParser%fsi TigerParser%fs: TigerParser%fsp
	fsyacc.exe --module TigerParser TigerParser$*fsp

TigerLexerTest.exe: TigerParser.fsi TigerParser.fs TigerLexer.fs TigerLexerTest.fs
	fsharpc -r $(FSLEXYACCDIR)/FsLexYacc.Runtime.dll TigerParser.fsi TigerParser.fs TigerLexer.fs TigerLexerTest.fs -o TigerLexerTest.exe
