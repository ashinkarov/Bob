/* Copyright (c) 2011 Artem Shinkarov <artyom.shinkaroff@gmail.com>
  
   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.
  
   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <err.h>

#include "expand.h"

#define TOKEN_KIND(a, b) b,
#define KEYWORD(a, b) b,
const char *token_kind_name[] = 
{
#include "token_kind.def"
#include "keywords.def"
};
#undef TOKEN_KIND
#undef KEYWORD

#define TOKEN_CLASS(a, b) b,
const char *token_class_name[] =
{
#include "token_class.def"
};
#undef TOKEN_CLASS

/* This is a pointer to the first token from keywords.def  */
const char **  keywords = &token_kind_name[(int) tv_args];
size_t keywords_length = tok_kind_length  - tv_args;

/* Binary search function to search string in a char** table.  */
static inline size_t
kw_bsearch (const char *key, const char *table[], size_t len)
{
  size_t l = 0, r = len;

  while (l < r)
    {
      size_t hit = (l + r) / 2;
      int i = strcmp (key, table[hit]);
      /*printf ("%s ? %s, [%i, %i, %i]\n", key, table[hit], l, r, i);*/

      if (i == 0)
	return hit;
      else if (i < 0)
	r = hit;
      else
	l = hit + 1;
    }
  return len;
}

/* Initialize lexer LEX with a file name FNAME and
   set initial parameters of the lexer.  */
bool
lexer_init (struct lexer *lex, const char *fname)
{
  assert (fname != NULL, "lexer initialized with empty filename");
  assert (lex != NULL, "lexer memory is not allocated");
  
  lex->is_eof = false;
  lex->loc = (struct location) {1, 0};
  lex->fname = fname;
  lex->file = fopen (fname, "r");

  if (!lex->file)
    {
      warn ("error opening file `%s'", fname);
      return false;
    }
  
  /* tval_intit (&(lex->cur_token), tok_eof, tv_eof); */
  return true;
}

/* Actions before deallocating lexer.  */
bool
lexer_finalize (struct lexer *lex)
{
  fclose (lex->file);
  return true;
}


/* Gets one character from the file, is end of file is
   reached, it will return EOF in all the consequent calls.  */
static inline char
lexer_getch (struct lexer *lex)
{
  int ch;
  
  if (lex->is_eof)
    return EOF;

  ch = fgetc (lex->file);
  if (ch == EOF)
    {
      lex->is_eof = true;
      return EOF;
    }

  if (ch == '\n')
    {
      lex->loc.line++;
      lex->loc.col = 0;
    }
  else
    lex->loc.col++;

  return (char)ch;
}

/* Put character back on the stream of the lexer.
   Consequent lexer_getch should return exactly this character.  */
static inline void
lexer_ungetch (struct lexer *lex, char ch)
{
  if (ch == '\n')
    lex->loc.line--;
  /* FIXME position should show the last symbol
           of previous line, not -1.  */
  lex->loc.col--;
  ungetc (ch, lex->file);
}

/* Adds the character C to the string *BUFFER that has length *SIZE
   at the position *INDEX. *INDEX is a pointer in the *BUFFER.
   If the *BUFFER is NULL then it is being allocated, if the *INDEX
   points at the end of the *BUFFER the *BUFFER will be reallocated. */
static inline void
buffer_add_char (char **buffer, char **index, size_t *size, char c)
{
  const size_t initial_size = 16;

  if (*buffer == NULL)
    {
      *buffer = (char *) malloc (initial_size *sizeof(char));
      *index = *buffer;
      *(*index)++ = c;
      *size = initial_size;
      return;
    }

  assert (*index <= *buffer + *size, 
          "index is greater than allocated buffer");
  
  if (*index == *buffer + *size)
    {
      *buffer = (char *) realloc (*buffer, *size * 2 * sizeof (char));
      *index = *buffer +*size;
      *size *= 2;
    }
    
  *(*index)++ = c;
}

/* Internal function to read until the end of comment.  */
static inline enum token_class
lexer_read_comments (struct lexer *lex, char **buf, size_t *size)
{
  char *index = *buf;

  buffer_add_char (buf, &index, size, '/');
  buffer_add_char (buf, &index, size, '*');

  while (true)
    {
      char c = lexer_getch (lex);
      if (c == EOF)
        {
          error_loc (lex->loc, "unexpected end of file in the middle of comment");
          buffer_add_char (buf, &index, size, 0);
          return tok_unknown;
        }
      
      buffer_add_char (buf, &index, size, c);
      if (c == '*')
        {
          char cc = lexer_getch (lex);
          if (cc == EOF)
            {
              error_loc (lex->loc, "unexpected end of file in the middle of comment");
              buffer_add_char (buf, &index, size, 0);
              return tok_unknown;
            }
          else if (cc == '/')
            {
              buffer_add_char (buf, &index, size, cc);
              break;
            }
        }
    }

   buffer_add_char (buf, &index, size, 0);
   return tok_comments;
}

/* Internal function to read until the end of string/char ignoring 
   escape sequences.  */
static inline enum token_class
lexer_read_string (struct lexer *lex, char **buf, size_t *size, char c)
{
  char *index = *buf;
  enum token_kind tok_kind;
  const char stop = c;

  assert (stop == '"' || stop == '\'', 
          "inapproriate starting symbol for string or char");
  tok_kind = stop == '"' ? tok_string : tok_char;


  buffer_add_char (buf, &index, size, stop);

  while (true)
    {
      c = lexer_getch (lex);
      if (c == EOF)
        {
          error_loc (lex->loc, 
                     "unexpected end of file in the middle of string");
          buffer_add_char (buf, &index, size, 0);
          return tok_unknown;
        }
      
      buffer_add_char (buf, &index, size, c);
      if (c == '\\')
        {
          char cc = lexer_getch (lex);
          if (cc == EOF)
            {
              error_loc (lex->loc, 
                         "unexpected end of file in the middle of string");
              buffer_add_char (buf, &index, size, 0);
              return tok_unknown;
            }
          buffer_add_char (buf, &index, size, cc);
        }
      else if (c == stop)
        break;
    }

   buffer_add_char (buf, &index, size, 0);
   return tok_kind;
}

/* Internal function to read until the end of identifier, checking
   if it is a keyword.  */
static inline void
lexer_read_id (struct lexer *lex, struct token *tok,
               char **buf, size_t *size, char c)
{
  char *index = *buf;
  size_t search;

  do 
    {
      buffer_add_char (buf, &index, size, c);
      c = lexer_getch (lex);
    }
  while (isalnum (c) || c == '_');
  lexer_ungetch (lex, c);
  buffer_add_char (buf, &index, size, 0);

  search = kw_bsearch (*buf, keywords, keywords_length);
  if (search != keywords_length)
    {
      if (*buf)
        free (*buf);
      *size = 0;
      *buf = NULL;
      tval_tok_init (tok, tok_keyword, (enum token_kind)(search + tv_args));
      return;
    }

  tok->tok_class = tok_id;

  return;
}

/* FIXME real numbers currently are not recognized.  */
/* Internal function to read until the end of number.  */
static inline enum token_class
lexer_read_number (struct lexer *lex, char **buf, size_t *size, char c)
{
  char *index = *buf;
  bool isoctal=false;
  bool ishex=false;

  /* first digit  */
  buffer_add_char (buf, &index, size, c);

  if (c == '0')
    {
      c = lexer_getch (lex);

      if  (c == 'x' || c == 'X')
	ishex = true;
      else if (isdigit (c))
        {
	  isoctal = true;
	  if (!(c >= '0' && c <= '7'))
	    error_loc (lex->loc, "%c found in the octal number", c);
	}
      else
	{
	  lexer_ungetch (lex, c);
  	  buffer_add_char (buf, &index, size, 0);
	  return tok_number;
	}
      buffer_add_char (buf, &index, size, c);
    }

  /* middle of the number  */
  while ((c = lexer_getch (lex))
	 && (isdigit (c)
	     || (ishex && ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')))))
    {
      if (isoctal && c >= '8')
	error_loc (lex->loc, "%c found in octal number", c);

      buffer_add_char (buf, &index, size, c);
    }

  if (c == 'l' || c == 'L')
    {
      buffer_add_char (buf, &index, size, c);
      c = lexer_getch (lex);
    }

  lexer_ungetch (lex, c);
  buffer_add_char (buf, &index, size, 0);

  return tok_number;
}

/* Reads the stream from lexer and returns dynamically allocated token
   of the appropriate type.  */
struct token *
lexer_get_token (struct lexer *lex)
{
  char c;
  struct location loc = lex->loc;
  struct token *tok = (struct token *) malloc (sizeof (struct token));
  size_t buf_size=16;
  char *buf = NULL;


  c = lexer_getch (lex);
  if (isspace (c))
    {
      while (EOF != (c = lexer_getch (lex)) && isspace (c))
        ;
      loc = lex->loc;
    }

  if (c == EOF)
    {
      tval_tok_init (tok, tok_eof, tv_eof);
      goto return_token;
    }

  if (c == '/')
    {
      char c1 = lexer_getch (lex);

      tval_tok_init (tok, tok_operator, tv_div);
      if (c1 == '*')
        tok->tok_class = lexer_read_comments (lex, &buf, &buf_size);
      else if (c1 == '=')
        tval_tok_init (tok, tok_operator, tv_div_eq);
      else
        lexer_ungetch (lex, c1);
      goto return_token;
    }

  if (c == '"' || c == '\'')
    {
      tok->tok_class = lexer_read_string (lex, &buf, &buf_size, c);
      goto return_token;
    }

  if (isalpha (c) || c == '_')
    {
      lexer_read_id (lex, tok, &buf, &buf_size, c);
      goto return_token;
    }

  if (isdigit (c))
    {
      tok->tok_class = lexer_read_number (lex, &buf, &buf_size, c);
      goto return_token;
    }

  if (c == '&')
    {
      char c1 = lexer_getch (lex);
      tval_tok_init (tok, tok_operator, tv_and);
      if (c1 == '&')
	tval_tok_init (tok, tok_operator, tv_and_and);
      else if (c1 == '=')
	tval_tok_init (tok, tok_operator, tv_and_eq);
      else
	lexer_ungetch (lex, c1);
      goto return_token;
    }
  
  if (c == '|')
    {
      char c1 = lexer_getch (lex);
      tval_tok_init (tok, tok_operator, tv_or);
      if (c1 == '|')
	tval_tok_init (tok, tok_operator, tv_or_or);
      else if (c1 == '=')
	tval_tok_init (tok, tok_operator, tv_or_eq);
      else
	lexer_ungetch (lex, c1);
      goto return_token;
    }

  if (c == '+')
    {
      char c1 = lexer_getch (lex);
      tval_tok_init (tok, tok_operator, tv_plus);
      if (c1 == '+')
	tval_tok_init (tok, tok_operator, tv_plus_plus);
      else if (c1 == '=')
	tval_tok_init (tok, tok_operator, tv_plus_eq);
      else
	lexer_ungetch (lex, c1);
      goto return_token;
    }
   
  if (c == '-')
    {
      char c1 = lexer_getch (lex);
      tval_tok_init (tok, tok_operator, tv_minus);
      if (c1 == '-')
	tval_tok_init (tok, tok_operator, tv_minus_minus);
      else if (c1 == '=')
	tval_tok_init (tok, tok_operator, tv_minus_eq);
      else if (c1 == '>')
	tval_tok_init (tok, tok_operator, tv_ref);
      else
	lexer_ungetch (lex, c1);
      goto return_token;
    } 
  
  if (c == '*')
    {
      char c1 = lexer_getch (lex);
      tval_tok_init (tok, tok_operator, tv_mult);
      if (c1 == '=')
	tval_tok_init (tok, tok_operator, tv_mult_eq);
      else
	lexer_ungetch (lex, c1);
      goto return_token;
    }

  if (c == '=')
    {
      char c1 = lexer_getch (lex);
      tval_tok_init (tok, tok_operator, tv_assign);
      if (c1 == '=')
	tval_tok_init (tok, tok_operator, tv_eq);
      else
	lexer_ungetch (lex, c1);
      goto return_token;
    }

  if (c == '!')
    {
      char c1 = lexer_getch (lex);
      tval_tok_init (tok, tok_operator, tv_not);
      if (c1 == '=')
	tval_tok_init (tok, tok_operator, tv_not_eq);
      else
	lexer_ungetch (lex, c1);
      goto return_token;
    }
  
  if (c == '^')
    {
      char c1 = lexer_getch (lex);
      tval_tok_init (tok, tok_operator, tv_xor);
      if (c1 == '=')
	tval_tok_init (tok, tok_operator, tv_xor_eq);
      else
	lexer_ungetch (lex, c1);
      goto return_token;
    }

  if (c == '>')
    {
      char c1 = lexer_getch (lex);
      tval_tok_init (tok, tok_operator, tv_gt);
      if (c1 == '=')
	tval_tok_init (tok, tok_operator, tv_gt_eq);
      else
	lexer_ungetch (lex, c1);
      goto return_token;
    }
  
  if (c == '<')
    {
      char c1 = lexer_getch (lex);
      tval_tok_init (tok, tok_operator, tv_lt);
      if (c1 == '=')
	tval_tok_init (tok, tok_operator, tv_lt_eq);
      else
	lexer_ungetch (lex, c1);
      goto return_token;
    }

  switch (c)
    {
    case '?': 
      tval_tok_init (tok, tok_operator, tv_question); goto return_token;
    case '%': 
      tval_tok_init (tok, tok_operator, tv_mod); goto return_token;
    case '~': 
      tval_tok_init (tok, tok_operator, tv_compl); goto return_token;
    case ',': 
      tval_tok_init (tok, tok_operator, tv_comma); goto return_token;
    case '(': 
      tval_tok_init (tok, tok_operator, tv_lparen); goto return_token;
    case ')': 
      tval_tok_init (tok, tok_operator, tv_rparen); goto return_token;
    case '[': 
      tval_tok_init (tok, tok_operator, tv_lsquare); goto return_token;
    case ']': 
      tval_tok_init (tok, tok_operator, tv_rsquare); goto return_token;
    case '{': 
      tval_tok_init (tok, tok_operator, tv_lbrace); goto return_token;
    case '}': 
      tval_tok_init (tok, tok_operator, tv_rbrace); goto return_token;
    case ';': 
      tval_tok_init (tok, tok_operator, tv_semicolon); goto return_token;
    case ':': 
      tval_tok_init (tok, tok_operator, tv_colon); goto return_token;
    default:
      ;
    }


  /* if nothing was found, we construct an unknown token  */
  assert (buf == NULL, "buf was used, but token_class is missing");
  buf = (char *) malloc (2 * sizeof (char));
  buf[0] = c; buf[1] = 0;
  tok->tok_class = tok_unknown;

return_token:
  assert (tok->tok_class >= tok_keyword && tok->tok_class <= tok_unknown,
          "token type was not provided");
  
  if (buf != NULL)
    tok->value.cval = buf;

  tok->loc = loc;

  return tok;
}


/* If the value of the token needs a character buffer or it is
   stored as an enum token_kind variable.  */
inline bool
token_uses_buf (enum token_class tclass)
{
  switch (tclass)
    {
    case tok_id:
    case tok_number:
    case tok_comments:
    case tok_string:
    case tok_char:
    case tok_unknown:
      return true;
    default:
      return false;
    }
}

/* String representation of the token TOK.  */
const char *
token_as_string (struct token * tok)
{
  
  if (token_uses_buf (token_class (tok)))
    return tok->value.cval;
  else
    return token_kind_name [(int) tok->value.tval];
}


/* Prints the token.  */
void
token_print (struct token *tok)
{
  const char *tokval = token_as_string (tok);

  (void) fprintf (stdout, "%d:%d %s ", (int)tok->loc.line, 
                  (int)tok->loc.col, token_class_name[(int) tok->tok_class]);

  if (tok->tok_class != tok_unknown)
    (void) fprintf (stdout, "['%s']\n", tokval);
  else
    (void) fprintf (stdout, "['%s'] !unknown\n", tokval);
    
  fflush (stdout);
}


/* Deallocates the memory that token occupies.  */
void
token_free (struct token *tok)
{
  assert (tok, "attempt to free NULL token");

  if (token_uses_buf (token_class (tok)) && tok->value.cval)
    free (tok->value.cval);
  free (tok);
}


/* Main function if you want to test lexer part only.  */
#ifdef LEXER_BINARY
int
main (int argc, char *argv[])
{
  struct lexer *lex = (struct lexer *) malloc (sizeof (struct lexer));
  struct token *tok;
  
  if (argc <= 1)
    {
      fprintf (stderr, "No input file\n");
      goto cleanup;
    }
  
  if (!lexer_init (lex, argv[1]))
    goto cleanup;

  while ((tok = lexer_get_token (lex))->tok_class != tok_eof)
    {
      token_print (tok);
      token_free (tok);     
    }

  token_free (tok);
  lexer_finalize (lex);

cleanup:
  if (lex)
    free (lex);

  return 0;
}
#endif
