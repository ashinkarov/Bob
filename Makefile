
all: parser

CFLAGS= -Wall -g -pedantic -std=c99

lexer: lex.c
	$(CC) $(CFLAGS) -DLEXER_BINARY -o $@ $^

parser: lex.o parser.o tree.o global.o print.o
	$(CC) $(CFLAGS) -o $@ $^
clean:
	$(RM) *.o lexer parser


lex.o: expand.h token_kind.def keywords.def token_class.def
parser.o: expand.h
tree.o: expand.h tree.h tree.def
global.o: global.h
print.o: print.h expand.h tree.h
