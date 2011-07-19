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

#include "expand.h"
#include "tree.h"
#include "global.h"
#include "print.h"
#include "typecheck.h"
#include "optimise.h"


struct parser 
{
  struct lexer *lex;
  
  /* Buffer and lengths associated with buffer. 
     Buffer holds up-to BUF_SIZE tokens, which means
     that it is possible to look BUF_SIZE tokens 
     forward.  */
  struct token **token_buffer;
  size_t buf_size ;
  size_t buf_start, buf_end, unget_idx;
  bool buf_empty;
  
  /* Count of opened parens, square brackets and 
     figure brackets. Used when we skip the tokens
     skip is finished when all the three counters
     are zeroes.  */
  int paren_count;
  int square_count;
  int brace_count;
};

/* Check if parser is not in any parenthesis/bracket expression.  */
static inline bool
parser_parens_zero (struct parser *parser)
{
  return parser->paren_count == 0 
         && parser->square_count == 0
         && parser->brace_count == 0;
}


struct token *  parser_get_token (struct parser *);
void parser_unget (struct parser *);
struct token *  parser_get_until_tval (struct parser *, enum token_kind);
struct token *  parser_get_until_tclass (struct parser *, enum token_class);
bool parser_expect_tval (struct parser *, enum token_kind);
bool parser_expect_tclass (struct parser *, enum token_class);
bool parser_init (struct parser *, struct lexer *);
bool parser_finalize (struct parser *);

tree handle_expr_list (struct parser *);
tree handle_primary_expr (struct parser *);
tree handle_postfix_expr (struct parser *);
tree handle_conditional_expr (struct parser *);
/* Handle statement type <id>  */
tree handle_type_definition (struct parser *);
/* Handle the case when we need to parse a type
   in the function/expand args or in case of constant.  */
tree handle_type (struct parser *);
tree handle_function_args (struct parser *);
tree handle_fun (struct parser *);
tree handle_args (struct parser *);
tree handle_binary_expr (struct parser *);
tree handle_expression (struct parser *);
tree handle_statement_list (struct parser *);
tree handle_expand (struct parser *);
tree handle_statement (struct parser *);
tree handle_stmt_or_stmt_list (struct parser *);
tree handle_strlist (struct parser *);

/* Get one token from the lexer or from the token buffer.
   Token is taken from the buffer if parser_unget was 
   called earlier. */
struct token *
parser_get_token (struct parser *  parser)
{
  struct token *  tok;
  
  if (parser->unget_idx == 0)
    {
      /* Skip comments for the time being. We do not skip
         the comments at the level of lexer, because we 
         can put them in the output program.  */
      while (true)
        {
          tok = lexer_get_token (parser->lex);
          if (token_class (tok) != tok_comments)
            break;
          else
            token_free (tok);
        }
            
      /* Keep track of brackets.  */
      if (token_class (tok) == tok_operator)
        switch (token_value (tok))
          {
          case tv_lparen:
            parser->paren_count ++; break;
          case tv_rparen:
            parser->paren_count --; break;
          case tv_lsquare:
            parser->square_count ++; break;
          case tv_rsquare:
            parser->square_count --; break;
          case tv_lbrace:
            parser->brace_count ++; break;
          case tv_rbrace:
            parser->brace_count --; break;
          default:
            ;
          }
      
      /* If TOKEN_BUFFER is full, we free the token pointed by BUF_START
         and put the new token on its place, changing BUF_START and
         BUF_END accordingly.  */
      if ((parser->buf_end + 1) % parser->buf_size == parser->buf_start)
        {
          token_free (parser->token_buffer[parser->buf_start]);
          parser->buf_start = (parser->buf_start + 1) % parser->buf_size;
          parser->token_buffer[parser->buf_end] = tok;
          parser->buf_end = (parser->buf_end + 1) % parser->buf_size;
        }
      else
        {
          parser->token_buffer[parser->buf_end] = tok;
          parser->buf_end = (parser->buf_end + 1) % parser->buf_size;
        }
    }
  else
    {
      ssize_t s;
      
      /* Return a token from the buffer.  */
      assert (parser->unget_idx < parser->buf_size, 
              "parser buffer holds only up to %i values.",
              parser->buf_size);
      
      s = parser->buf_end - parser->unget_idx;
      s = s < 0 ? parser->buf_size + s : s;
      parser->unget_idx --;
      
      tok = parser->token_buffer[s];
    }

  return tok;
}


/* Move the parser one token back. It means that the consequent
   call of parser_get_token would return the token from buffer,
   not from lexer.  */
void
parser_unget (struct parser *  parser)
{
  parser->unget_idx++;
  assert (parser->unget_idx < parser->buf_size, 
          "parser buffer holds only up to %i values.",
          parser->buf_size);
}


/* Skip tokens until token with value TKIND would be found.  */
struct token *
parser_get_until_tval (struct parser * parser, enum token_kind tkind)
{
  struct token * tok;
  
  do 
    {
      tok = parser_get_token (parser);
      if (!token_uses_buf (token_class (tok))
          /* FIXME the following condition makes it impossible
             to skip until some symbol if you are inside the 
             block or brackets. */
          /* && parser_parens_zero (parser) */
          && token_value (tok) == tkind)
        {
          return tok;
        }
    }
  while (token_class (tok) != tok_eof);

  return tok;
}

/* Skip tokens until token of class TCLASS would be found.  */
struct token *
parser_get_until_tclass (struct parser * parser, enum token_class tclass)
{
  struct token * tok;
  
  do 
    {
      tok = parser_get_token (parser);
      /* FIXME the following condition makes it impossible
         to skip until some symbol if you are inside the 
         block or brackets. */
      if (/* parser_parens_zero (parser) && */ token_class (tok) == tclass)
        {
          return tok;
        }
    }
  while (token_class (tok) != tok_eof);

  return tok;
}


/* Check if the next token returned by parser_get_token would be
   token with the value TKIND, in case the value is different,
   the error_loc would be called.
   NOTE: function ungets the token after checking it.  */
bool
parser_expect_tval (struct parser *parser, enum token_kind tkind)
{
  struct token *  tok = parser_get_token (parser);
  if (!token_uses_buf (token_class (tok)) && token_value (tok) == tkind)
    {
      parser_unget (parser);
      return true;
    }
  else
    {
      error_loc (token_location (tok), 
                 "token `%s' expected, `%s' token found",
                 token_kind_as_string (tkind), token_as_string (tok));
      parser_unget (parser);
      return false;
    }
}


/* Check if the next token returned by parser_get_token would be
   token of class TCLASS, in case the class is different,
   the error_loc would be called.
   NOTE: function ungets the token after checking it.  */
bool
parser_expect_tclass (struct parser *parser, enum token_class tclass)
{
  struct token *  tok = parser_get_token (parser);
  if (token_class (tok) == tclass)
    {
      parser_unget (parser);
      return true;
    }
  else
    {
      error_loc (token_location (tok), 
                 "token of class `%s' expected, `%s' token found",
                 token_class_as_string (tclass), token_as_string (tok));
      parser_unget (parser);
      return false;
    }
}


/* Initialize the parser, allocate memory for token_buffer.  */
bool
parser_init (struct parser * parser, struct lexer *lex)
{
  parser->lex = lex;
  parser->buf_size = 16;
  parser->buf_start = 0;
  parser->buf_end = 0;
  parser->buf_empty = true;
  parser->token_buffer 
    = (struct token **) malloc (parser->buf_size * sizeof (struct token *));
  parser->unget_idx = 0;
  return true;
}


/* Clear the memory allocated for internal structure.
   NOTE: PARSER is not freed.  */
bool
parser_finalize (struct parser *parser)
{
  assert (parser, "attempt to free empty parser");

  while (parser->buf_start % parser->buf_size 
         != parser->buf_end % parser->buf_size)

    {
      token_free (parser->token_buffer[parser->buf_start]);
      parser->buf_start = (parser->buf_start + 1) % parser->buf_size;
    }

  if (parser->token_buffer) 
    free (parser->token_buffer);
  
  lexer_finalize (parser->lex);
  return true;
}


/* Check if token TOK can start an expression.  */
bool
token_starts_expr (struct token *  tok)
{
  switch (token_class (tok))
    {
    case tok_id:
    case tok_string:
    case tok_char:
    case tok_number:
      return true;
    case tok_operator:
      switch (token_value (tok))
        {
        case tv_lparen:
        case tv_lsquare:
        case tv_minus:
        /* FIXME case tv_not:  should be supported.  */
          return true;
        default:
          return false;
        }
    case tok_keyword:
      switch (token_value (tok))
        {
        case tv_int:
        case tv_str:
          return true;
        default:
          return false;
        }
    default:
      return false;
    }
}


/* Read a list of expressions: <exp>, <exp>, ... 
   and return a tree of type LIST which contains the elements.  */
tree
handle_expr_list (struct parser *  parser)
{
  tree  lst = make_tree_list ();
  struct token *  tok = parser_get_token (parser);
  bool error_occured = false;

  if (token_starts_expr (tok))
    parser_unget (parser);
  else
    {
      parser_unget (parser);
      return lst;
    }

  while (true)
    {
      tree exp = handle_expression (parser);
      tok = parser_get_token (parser);

      /* Check for error.  */
      if (!error_occured && exp != error_mark_node)
        tree_list_append (lst, exp);
      else
        {
          error_occured = true;
          free_tree (exp);
          free_tree (lst);
          lst = error_mark_node;
        }
        
      if (!(token_class (tok) == tok_operator && token_value (tok) == tv_comma))
        {
          parser_unget (parser);
          break;
        }
    }

  return lst;
}


/* Primary expression is:
   constant,
   identifier,
   function call,
   ( <expr> ),
   [ <list elements> ],
   - primary_expr,
   ! primary_expr  */
tree
handle_primary_expr (struct parser *  parser)
{
  tree res = error_mark_node;
  struct token *  tok;

  tok = parser_get_token (parser);

  /* token_print (tok); */
  switch (token_class (tok))
    {
      struct token *  tok1;

    case tok_number:
      res = make_tree (INTEGER_CST);
      TREE_INTEGER_CST (res) = atoi (token_as_string (tok));
      TREE_LOCATION (res) = token_location (tok);
      break;

    case tok_string:
    case tok_char:
      res = make_string_cst_tok (tok);
      TREE_LOCATION (res) = token_location (tok);
      break;

    case tok_id:
    case tok_keyword:
      tok1 = parser_get_token (parser);
      

      if (token_class (tok1) == tok_operator 
          && token_value (tok1) == tv_lparen)
        {
          /* We have a function call.  */
          tree args = handle_expr_list (parser);
          
         
          if (parser_expect_tval (parser, tv_rparen))
            {
              /* Check for possible conversion functions like 
                 int and str, all the other keyword functions
                 should be rejected for the time being.  */
              if (token_class (tok) == tok_keyword
                  && !token_starts_expr (tok))
                {
                  error_loc (token_location (tok), 
                             "invalid usage of keyword %s.", 
                             token_as_string (tok));
                  parser_get_token (parser);
                  free_tree (args);
                  return error_mark_node;
                }

              parser_get_token (parser);
              res = make_tree (CALL_EXPR);
              TREE_OPERAND_SET (res, 0, make_string_cst_tok (tok));
              TREE_OPERAND_SET (res, 1, args);
              TREE_LOCATION (res) = token_location (tok);
            }
          else
            res = error_mark_node;
        }
      else
        {
          parser_unget (parser);
          res = make_identifier_tok (tok);
          TREE_LOCATION (res) = token_location (tok);
        }
      break;

    case tok_operator:
      switch (token_value (tok))
        {
        case tv_lparen:
          res = handle_expression (parser);
          parser_expect_tval (parser, tv_rparen);
          parser_get_token (parser);
          break;
        
        case tv_lsquare:
          res = make_tree (LIST_CST);
          TREE_LOCATION (res) = token_location (tok);
          TREE_LIST_CST (res) = handle_expr_list (parser);
          parser_expect_tval (parser, tv_rsquare);
          parser_get_token (parser);
          break;

        case tv_minus:
          res = make_tree (UMINUS_EXPR);
          TREE_LOCATION (res) = token_location (tok);
          TREE_OPERAND_SET (res, 0, handle_primary_expr (parser));
          break;

        case tv_not:
          res = make_tree (TRUTH_NOT_EXPR);
          TREE_LOCATION (res) = token_location (tok);
          TREE_OPERAND_SET (res, 0, handle_primary_expr (parser));
          break;

        default:
          error_loc (token_location (tok), 
                     "unexpected token %s", token_as_string (tok));
          parser_unget (parser);
          break;
        }
      break;
      
    default:
      error_loc (token_location (tok), 
                 "token %s cannot start an expression.", token_as_string (tok));
      break;
    }

    
    //printf ("--- returning from primary %s\n", TREE_CODE_NAME (TREE_CODE (res)));
    //if (TREE_CODE (res) == INTEGER_CST)
      //printf ("--- int_cst value = %i\n", TREE_INTEGER_CST (res));
    return res;
}


/* Currently we have only one type of postfix expressions
   which are indexing:
     <primary_expr> [<expr>][<expr>] ...
   each indexing is converted to the index function call.  */
tree
handle_postfix_expr (struct parser *  parser)
{
  struct token *  tok;
  tree exp;
  struct location loc;
  
  tok = parser_get_token (parser);
  parser_unget (parser);

  if (!token_starts_expr (tok))
    {
      error_loc (token_location (tok), 
                 "token %s cannot start an expression.", 
                 token_as_string (tok));
      return error_mark_node;
    }

  exp = handle_primary_expr (parser);
  loc = TREE_LOCATION (exp);

  while (true)
    {
      tok = parser_get_token (parser);
      if (token_class (tok) == tok_operator 
          && token_value (tok) == tv_lsquare)
        {
          tree post;
          tree list;
          tree name;
          tree idx = handle_expression (parser);
          bool rsq = parser_expect_tval (parser, tv_rsquare);

          if (idx == error_mark_node || !rsq)
            {
              free_tree (exp);
              free_tree (idx);
              return error_mark_node;
            }
          
          parser_get_token (parser);
          post = make_tree (CALL_EXPR);
          name = make_string_cst_str ("index");
          TREE_OPERAND_SET (post, 0, name);
          list = make_tree_list ();
          tree_list_append (list, exp);
          tree_list_append (list, idx);
          TREE_OPERAND_SET (post, 1, list);
          TREE_LOCATION (post) = loc;
          exp = post;
        }
      else
        {
          parser_unget (parser);
          return exp;
        }
    }
}

/* User defined type: type "<name>"  { (<convert function> -> "<name>")*};  */
tree
handle_type_definition (struct parser *parser)
{
  tree t;
  struct token *  tok;

  tok = parser_get_token (parser);
  if (token_class (tok) != tok_id)
    {
      error_loc (token_location (tok), "type identifier expected");
      parser_get_until_tval (parser, tv_semicolon);
      t = error_mark_node;
    }
  else
    {
      tree tt = make_string_cst_tok (tok);
      t = add_user_type (tt);
    }

  /* FIXME Parse the part of accessors { (string -> "")* } 
     for the time being we just skip it.  */
  parser_get_until_tval (parser, tv_semicolon);

  return t;
}


tree
handle_type (struct parser *  parser)
{
  struct token *  tok = parser_get_token (parser);
  tree type= error_mark_node;

  if (token_class (tok) != tok_id && token_class (tok) != tok_keyword)
    {
      error_loc (token_location (tok), "type expected");
      parser_unget (parser);
      return error_mark_node;
    }

  type = type_defined (token_as_string (tok));
  if (!type)
    {
      error_loc (token_location (tok), 
                 "unknown type `%s' used", token_as_string (tok));
      parser_unget (parser);
      return error_mark_node;
    }

  return type;
}

/* handle the list (<type> <name> ,) function checks
   that type is present and is appropriate and returns
   a list of variables typed with the types.  */
tree
handle_function_args (struct parser *parser)
{
  struct token *  tok;
  tree list = make_tree_list ();
  bool error_occured=false; 

  while (true)
    {
      tree type, id;

      tok = parser_get_token (parser);
      if (token_class (tok) == tok_operator && token_value (tok) != tv_comma)
        {
          /* it could be ']' or ')' in that case 
             it is not an empty list.  */
          parser_unget (parser);
          goto comma_or_rsquare;
        }

      parser_unget (parser);
      if ((type = handle_type (parser)) == error_mark_node)
        {
          parser_get_token (parser); /* eat type  */
          parser_get_token (parser); /* eat variable  */
          error_occured = true;
          goto comma_or_rsquare;
        }

      //tok = parser_get_token (parser);
      //token_print (tok);
      /* variable */
      if (parser_expect_tclass (parser, tok_id))
        {
          tok = parser_get_token (parser);
          id = make_identifier_tok (tok);
          TREE_TYPE (id) = type;
          tree_list_append (list, id);
        }
      else
        {
          error_loc (token_location (tok), "variable expected here");
          error_occured = true;
        }

comma_or_rsquare:
      /* comma or rsquare or eof  */
      tok = parser_get_token (parser);
      if (token_class (tok) == tok_operator && token_value (tok) == tv_comma)
        continue;
      else
        {
          parser_unget (parser);
          break;
        }
    }

  if (error_occured)
    {
      free_tree (list);
      return error_mark_node;
    }
  else
    return list;
}


/* Handle the part of expand 'name = "<name>"'  */
tree
handle_fun (struct parser *  parser)
{
  struct token *  tok = parser_get_token (parser);

  /* we assume that it is called when we know
     that the current token is fun.  */
  assert (token_class (tok) == tok_keyword && token_value (tok) == tv_name,
          "token `name' expected here");
  if (!parser_expect_tval (parser, tv_assign))
    {
      error_loc (token_location (tok), "assignment expected");
      return error_mark_node;
    }

  parser_get_token (parser); /* eat-up '='  */
  
  if (!parser_expect_tclass (parser, tok_string))
    return error_mark_node;

  tok = parser_get_token (parser);
  return make_string_cst_tok (tok);
}

/* Handle the part of expsnd 'args = [ (<type> <var>)* ]'  */
tree
handle_args (struct parser *  parser)
{
  tree args;
  struct token *  tok = parser_get_token (parser);
  
  /* we assume that it is called when we know
     that the current token is fun.  */
  assert (token_class (tok) == tok_keyword && token_value (tok) == tv_args,
          "token 'args' expected here");
  if (!parser_expect_tval (parser, tv_assign))
    {
      error_loc (token_location (tok), "assignment expected");
      return error_mark_node;
    }

  parser_get_token (parser); /* eat-up '='  */
  
  if (!parser_expect_tval (parser, tv_lsquare))
    return error_mark_node;
  
  tok = parser_get_token (parser);

  args = handle_function_args (parser);

  if (parser_expect_tval (parser, tv_rsquare))
    parser_get_token (parser);
  else
    {
      /* FIXME free args.  */
      args = error_mark_node;
    }

  return args;
}


/* Binary expression of primary expressions. PE + PE * ...  */
tree
handle_binary_expr (struct parser *  parser)
{
  struct token *  tok;
  enum prec {
    prec_none,
    prec_logor,
    prec_logand,
    /* bit operations here if later
       we will want them.  */
    prec_eq,
    prec_rel,
    prec_shift,
    prec_add,
    prec_mult,
    num_precs
  };

  struct {
    tree expr;
    enum prec prec;
    enum tree_code op;
  } stack[num_precs];

  int sp = 0;

  tree t = handle_postfix_expr (parser);

  if (t == error_mark_node)
    return error_mark_node;

  stack[0].expr = t;
  stack[0].prec = prec_none;

  while (true)
    {
      enum prec oprec;
      enum tree_code ocode;

      tok = parser_get_token (parser);
      if (token_class (tok) == tok_operator)
        {
          switch (token_value (tok))
            {
            case tv_mult:
              oprec = prec_mult;
              ocode = MULT_EXPR;
              break;
            case tv_div:
              oprec = prec_mult;
              ocode = DIV_EXPR;
              break;
            case tv_mod:
              oprec = prec_mult;
              ocode = MOD_EXPR;
              break;
            case tv_plus:
              oprec = prec_add;
              ocode = PLUS_EXPR;
              break;
            case tv_minus:
              oprec = prec_add;
              ocode = MINUS_EXPR;
              break;
            /* case tv_shl:
              oprec = prec_shift;
              ocode = SHL_EXPR;
              break;
            case tv_shr:
              oprec = prec_shift;
              ocode = SHR_EXPR;
              break; */
            case tv_lt:
              oprec = prec_rel;
              ocode = GT_EXPR;
              break;
            case tv_gt:
              oprec = prec_rel;
              ocode = LT_EXPR;
              break;
            case tv_eq:
              oprec = prec_rel;
              ocode = EQ_EXPR;
              break;
            case tv_gt_eq:
              oprec = prec_rel;
              ocode = GE_EXPR;
              break;
            case tv_lt_eq:
              oprec = prec_rel;
              ocode = LE_EXPR;
              break;
            case tv_not_eq:
              oprec = prec_rel;
              ocode = NE_EXPR;
              break;
            /* here we could have bitwise operators.  */
            case tv_and_and:
              oprec = prec_logand;
              ocode = TRUTH_AND_EXPR;
              break;
            case tv_or_or:
              oprec = prec_logor;
              ocode = TRUTH_OR_EXPR;
              break;
            default:
              parser_unget (parser);
              goto out;
            }
          
          while (oprec <= stack[sp].prec)
            {
              stack[sp-1].expr 
                = make_binary_op (stack[sp].op, 
                                  stack[sp-1].expr, stack[sp].expr);
              sp--;
            }
          
          t = handle_postfix_expr (parser);

          if (t == error_mark_node)
            {
              /* Free the memory allocated.  */
              while (sp >= 0)
                free_tree (stack[sp--].expr);
              return error_mark_node;
            }

          sp++;
          stack[sp].expr = t;
          stack[sp].prec = oprec;
          stack[sp].op = ocode;
        }
      else
        {
          parser_unget (parser);
          break;
        }
    }
out:
  while (sp > 0)
    {
      stack[sp-1].expr 
        = make_binary_op (stack[sp].op, stack[sp-1].expr, stack[sp].expr);
      sp--;
    }
  
  //printf ("-- %s exit, tok = ", __func__); token_print (tok);
  return stack[0].expr;
}


/* <expr> ? <expr> : <expr>  */
tree
handle_conditional_expr (struct parser *  parser)
{
  struct token *  tok;
  tree cond=error_mark_node, 
       if_expr = error_mark_node, 
       else_expr = error_mark_node;

  cond = handle_binary_expr (parser);

  if (cond == error_mark_node)
    return error_mark_node;

  tok = parser_get_token (parser);
  if (token_class (tok) == tok_operator 
      && token_value (tok) == tv_question)
    {
      if_expr = handle_expression (parser);
      if (if_expr == error_mark_node
          || !parser_expect_tval (parser, tv_colon))
        goto out;

      parser_get_token (parser); /* eat ':'  */

      else_expr = handle_conditional_expr (parser);
      if (else_expr == error_mark_node)
        goto out;
    }
  else
    {
      parser_unget (parser);
      return cond;
    }

out:
  if (cond == error_mark_node 
      || if_expr == error_mark_node
      || else_expr == error_mark_node)
    {
      free_tree (cond);
      free_tree (if_expr);
      free_tree (else_expr);
      return error_mark_node;
    }
  else
    {
      tree ret = make_tree (COND_EXPR);
      TREE_OPERAND_SET (ret, 0, cond);
      TREE_OPERAND_SET (ret, 1, if_expr);
      TREE_OPERAND_SET (ret, 2, else_expr);
      TREE_LOCATION (ret) = TREE_LOCATION (cond);
      return ret;
    }
}

/* Handle assigment expression or binary expression.
   X = BE or BE.  */
tree
handle_expression (struct parser *  parser)
{
  tree expr;
  struct token *  tok = parser_get_token (parser);

  //printf ("-- %s begin, tok = ", __func__); token_print (tok);
  if (token_class (tok) == tok_id)
    {
      struct token *  tok1 = parser_get_token (parser);
      if (token_class (tok1) == tok_operator
          && is_assignment_operator (token_value (tok1)))
        {
          /* We need to save the assignment operator, otherwise
             consequent get_token would free it.  */
          enum token_kind tk = token_value (tok1);
          tree lhs = make_identifier_tok (tok);
          tree rhs;
          
          rhs = handle_conditional_expr (parser);
          if (rhs != error_mark_node)
            expr = make_assign (tk, lhs, rhs);
          else
            expr = error_mark_node;
        }
      else
        {
          parser_unget (parser);
          parser_unget (parser);
          expr = handle_conditional_expr (parser);
        }
    }
  else
    {
      parser_unget (parser);
      expr = handle_conditional_expr (parser);
    }

  return expr;
}


/* Read statement or statement list. Used when parsing 
   for and if statements, as they can have list of expression
   or single expression in the body.
   NOTE: function always returns STMT_LIST even if there
   was only one expression.  */
tree
handle_stmt_or_stmt_list (struct parser *  parser)
{
  tree ret;
  struct token *  tok = parser_get_token (parser);
  
  parser_unget (parser);
  if (token_class (tok) == tok_operator 
      && token_value (tok) == tv_lbrace)
    ret = handle_statement_list (parser);
  else
    {
      tree stmt = handle_statement (parser);

      if (stmt != error_mark_node)
        {
          ret = make_tree (STMT_LIST);
          TREE_STMT_LIST_STMTS (ret) = make_tree_list ();
          tree_list_append (TREE_STMT_LIST_STMTS (ret), stmt);
          TREE_LOCATION (ret) = TREE_LOCATION (stmt);
        }
      else
        ret = error_mark_node;
    }

  return ret;
}


/* Handle statement that can be found in function or expand
   body.  */
tree
handle_statement (struct parser *  parser)
{
  tree stmt;
  struct token *  tok = parser_get_token (parser);
  parser_unget (parser);

  if (token_class (tok) == tok_keyword)
    {
      switch (token_value (tok))
        {
        case tv_assert:
        case tv_generate:
          {
            tree name = make_string_cst_tok (tok);

            parser_get_token (parser);
            stmt = handle_expression (parser);
          
            if (stmt != error_mark_node)
              {
                tree ce = make_tree (CALL_EXPR);
                tree args = make_tree_list ();

                tree_list_append (args, stmt);
                TREE_OPERAND_SET (ce, 0, name);
                TREE_OPERAND_SET (ce, 1, args);
                TREE_LOCATION (ce) = TREE_LOCATION (name);
                
                if (parser_expect_tval (parser, tv_semicolon))
                  parser_get_token (parser);

                return ce;
              }
            return error_mark_node;
          }
        case tv_if:
          {
            tree cond;
            tree if_branch = NULL;
            tree else_branch = NULL;
            struct location loc = token_location (tok);
            
            parser_get_token (parser);
            cond = handle_expression (parser);
            
            if_branch = handle_stmt_or_stmt_list (parser);

            tok = parser_get_token (parser);
            if (token_class (tok) == tok_keyword 
                && token_value (tok) == tv_else)
              else_branch = handle_stmt_or_stmt_list (parser);
            else
              parser_unget (parser);

            if (cond != error_mark_node 
                && if_branch != NULL && if_branch != error_mark_node
                && (else_branch == NULL || else_branch != error_mark_node))
              {
                tree ret = make_tree (IF_STMT);
                TREE_OPERAND_SET (ret, 0, cond);
                TREE_OPERAND_SET (ret, 1, if_branch);
                TREE_OPERAND_SET (ret, 2, else_branch);
                TREE_LOCATION (ret) = loc;
                return ret;
              }
            else
              {
                /* Free everything that has been allocated here.  */
                free_tree (cond);
                free_tree (if_branch);
                free_tree (else_branch);
                return error_mark_node;
              }
          }
        case tv_for:
          {
            tree vars;
            tree iter;
            tree stmts;
            bool error_occured = false;
            struct location loc = token_location (tok);

            parser_get_token (parser); 
            vars = handle_expression (parser);

            if (parser_expect_tval (parser, tv_in))
              parser_get_token (parser);
            else
              error_occured = true;

            iter = handle_expression (parser);
            stmts = handle_stmt_or_stmt_list (parser);

            if (!error_occured 
                && vars != error_mark_node
                && iter != error_mark_node
                && stmts != error_mark_node)
              {
                tree ret = make_tree (FOR_STMT);
                TREE_OPERAND_SET (ret, 0, vars);
                TREE_OPERAND_SET (ret, 1, iter);
                TREE_OPERAND_SET (ret, 2, stmts);
                TREE_LOCATION (ret) = loc;
                return ret;
              }
            else
              {
                /* Free everything that has been allocated here.  */
                free_tree (vars);
                free_tree (iter);
                free_tree (stmts);
                return error_mark_node;
              }
          }
        default:
          return error_mark_node;
        }
    }
  else if (token_starts_expr (tok))
    {
      tree ret;
      

      ret = handle_expression (parser);
      
      if (parser_expect_tval (parser, tv_semicolon))
        parser_get_token (parser);
      return ret;
    }
  else if (token_class (tok) == tok_operator 
           && token_value (tok) == tv_semicolon)
    {
      /* This is a hack to eat empty semicolon, or maybe not a hack.  */
      parser_get_token (parser);
      return NULL;
    }
  else
    return error_mark_node;
}


/* Handle statement list. Statements list are bodies of
   functions, expands and for/if structures.  */
tree
handle_statement_list (struct parser *  parser)
{
  struct token *  tok;
  bool error_occured = false;
  tree stmt_list = make_tree_list ();
  // tree var_list = make_tree_list ();


  if (!parser_expect_tval (parser, tv_lbrace))
    {
      free_tree (stmt_list);
      return error_mark_node;
    }
  tok = parser_get_token (parser);

  while (true)
    {
      struct location tl;

      tok = parser_get_token (parser);
      parser_unget (parser);
      tl = token_location (tok);

      tree stmt = handle_statement (parser);
      
      if (stmt != error_mark_node && stmt != NULL)
        {
          tree_list_append (stmt_list, stmt);
          
          //print_statement (stdout, stmt);
          //printf ("\n");
        }
      else if (stmt != NULL)
        {     
          error_occured = true;
          //printf ("-- start to skip form token %s ", saved);
          tok = parser_get_until_tval (parser, tv_semicolon);
          //printf ("skipped until %s\n", token_as_string (tok));
          error_loc (tl, "invalid expression found, skipped until %i:%i:`%s'",
                     (int) token_location (tok).line, 
                     (int) token_location (tok).col,
                     token_as_string (tok));
          if (token_class (tok) == tok_eof)
            goto out;
        }

      tok = parser_get_token (parser);
      
      if (token_class (tok) == tok_operator && token_value (tok) == tv_rbrace)
        break;
      else
        parser_unget (parser);
    }

out:
  if (error_occured)
    {
      free_tree (stmt_list);
      return error_mark_node;
    }
  else
    {
      tree ret = make_tree (STMT_LIST);
      TREE_STMT_LIST_STMTS (ret) = stmt_list;
      return ret;
    }
}

/* Parse expand construction.
   expand fun="<name>" args=[(<type> <name>)*] {<stmt-list>}  */
tree
handle_expand (struct parser *parser)
{
  tree name=error_mark_node, 
       args=error_mark_node, 
       stmts;
  struct token *  tok = parser_get_token (parser);
  
  parser_unget (parser);

  if (token_class (tok) == tok_keyword && token_value (tok) == tv_name)
    {
      name = handle_fun (parser);
      if (!parser_expect_tval (parser, tv_args))
        goto skip;
      args = handle_args (parser);
    }
  else if (token_class (tok) == tok_keyword && token_value (tok) == tv_args)
    {
      args = handle_args (parser);
      if (!parser_expect_tval (parser, tv_name))
        goto skip;
      name = handle_fun (parser);
    }
  else
    {
       error_loc (token_location (tok), "fun or args definition expected");
       goto skip;
    }

  tok = parser_get_token (parser);
  parser_unget (parser);

skip:
  parser_get_until_tval (parser, tv_lbrace);
  parser_unget (parser);

  stmts = handle_statement_list (parser);

  /* For testing purposes.  */
  /* if (stmts != error_mark_node)
    {
      printf ("-- statements of expansion %s parsed successfully\n", 
              TREE_STRING_CST (name));
      print_stmt_list (stdout, stmts);
      printf ("\n");
    }  */
  
  if (args == error_mark_node 
      || name == error_mark_node
      || stmts == error_mark_node)
    {  
      free_tree (name);
      free_tree (args);
      free_tree (stmts);
      return error_mark_node;
    }
  else
    {
      tree t = make_tree (EXPAND_STMT);
      TREE_OPERAND_SET (t, 0, name);
      TREE_OPERAND_SET (t, 1, args);
      TREE_OPERAND_SET (t, 2, stmts);
      return t;
    }
}

tree 
handle_strlist (struct parser *  parser)
{
  struct location loc = token_location (parser_get_token (parser));
  tree expr;
  
  parser_unget (parser);
  expr = handle_expression (parser);

  if (expr == error_mark_node || TREE_CODE (expr) != ASSIGN_EXPR)
    {
      error_loc (loc, "invalid expression for strlist found");
      return error_mark_node;
    }
  
  return expr;
}

tree 
handle_const (struct parser *  parser)
{
  struct location loc = token_location (parser_get_token (parser));
  tree type, expr;
  
  parser_unget (parser);
  if ((type = handle_type (parser)) == error_mark_node)
    goto error;
  
  expr = handle_expression (parser);

  if (TREE_CODE (expr) == ASSIGN_EXPR)
    TREE_TYPE (TREE_OPERAND (expr, 0)) = type;
  else if (TREE_CODE (expr) == IDENTIFIER)
    TREE_TYPE (expr) = type;
  else
    {
      error_loc (loc, "invalid expression for constant found");
      goto error;
    }
  
  return expr;
error:
  parser_get_until_tval (parser, tv_semicolon);
  parser_get_token (parser);
  return error_mark_node;
}


tree
handle_proto (struct parser *  parser)
{
  struct token *  tok = parser_get_token (parser);
  struct location loc = token_location (tok);
  tree name = error_mark_node, 
       ret = error_mark_node, 
       args = error_mark_node,
       ccall = error_mark_node;

  parser_unget (parser);

  while (token_class (tok) == tok_keyword)
    {
      switch (token_value (tok))
        {
          tree t;
        case tv_name:
          t = handle_fun (parser);
          if (t != error_mark_node && name == error_mark_node)
            {
              /* Get rid od both first and last quotation marks  */
              char * fname = TREE_STRING_CST (t);
              char * fnew;
              const int sz = strlen (fname) -1;
              
              fnew = (char *) malloc (sizeof (char) * sz);
              fnew = strncpy (fnew, &fname[1], sz - 1);
              fnew[sz-1] = '\0';
              free (fname);
              TREE_STRING_CST (t) = fnew;
              name = t;
            }
          else
            {
              error_loc (loc, "failed to read name of proto or name "
                              "was specified more than once");
              goto out;
            }
          break;
        case tv_ccall:
          parser_get_token (parser);
          if (!parser_expect_tval (parser, tv_assign))
            goto out;

          parser_get_token (parser); /* eat '='  */

          if (!parser_expect_tclass (parser, tok_string))
            goto out;

          tok = parser_get_token (parser);
          t = make_string_cst_tok (tok);

          if (t != error_mark_node && ccall == error_mark_node)
            ccall = t;
          else
            {
              error_loc (loc, "failed to read ccall of proto or ccall "
                              "was specified more than once");
              goto out;
            }
          break;
        case tv_args:
          t = handle_args (parser);
          if (t != error_mark_node && args == error_mark_node)
            {
              /* FIXME actually we need only types in the
                 definition.  */
              struct tree_list_element *  tel;
              TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (t), entries)
                {
                  tree type;
                  assert (TREE_TYPE (tel->element) != NULL, 0);
                  type = TREE_TYPE (tel->element);
                  
                  free_tree (TREE_ID_NAME (tel->element));
                  free (tel->element);
                  tel->element = type;
                }
              args = t;
            }
          else
            {
              error_loc (loc, "failed to read args of proto or args "
                              "were specified more than once");
              goto out;
            }
          break;
        case tv_ret:
          parser_get_token (parser);
          if (!parser_expect_tval (parser, tv_assign))
            goto out;

          parser_get_token (parser); /* eat '=' */
           
          if ((t = handle_type (parser)) == error_mark_node)
            goto out;
           
          if (t != error_mark_node && ret == error_mark_node)
            ret = t;
          else
            {
              error_loc (loc, "failed to read ret of proto or ret "
                              "was specified more than once");
              goto out;
            }
          break;
        default:
          goto out;
        }

      tok = parser_get_token (parser);
      parser_unget (parser);
    }

out:
  if (name == error_mark_node || ret == error_mark_node
      || args == error_mark_node || ccall == error_mark_node)
    {
      error_loc (loc, "error parsing proto definition");
      parser_get_until_tval (parser, tv_semicolon);
      return error_mark_node;
    }

  if (!parser_expect_tval (parser, tv_semicolon))
    {
      parser_get_until_tval (parser, tv_semicolon);
      return error_mark_node;
    }
  else
    {
      tree proto = make_tree (FUNCTION_PROTO);
      TREE_FUNCTION_PROTO_CCALL (proto) = ccall;
      TREE_OPERAND_SET (proto, 0, name);
      TREE_OPERAND_SET (proto, 1, ret);
      TREE_OPERAND_SET (proto, 2, args);
      TREE_LOCATION (proto) = loc;
      return proto;
    }
}

/* Top level function to parse the file.  */
int
parse (struct parser *parser)
{
  struct token *  tok;
  
  error_count = warning_count = 0;

  while (token_class (tok = parser_get_token (parser)) != tok_eof)
    {
      switch (token_class (tok))
        {
        case tok_keyword:
          switch (token_value (tok))
            {
              tree res;

            case tv_type:
              handle_type_definition (parser);
              break;

            case tv_expand:
              if ((res = handle_expand (parser)) != error_mark_node)
                {
                  if (expand_exists (TREE_STRING_CST (TREE_OPERAND (res, 0)))
                      != NULL)
                    error ("duplicate expand `%s' definition found",
                           TREE_STRING_CST (TREE_OPERAND (res, 0)));
                  else
                    tree_list_append (function_list, res);
                }
              break;

            case tv_strlist:
              if ((res = handle_strlist (parser)) != error_mark_node)
                {
                  assert (TREE_CODE (res) == ASSIGN_EXPR, 
                          "Assign expression expected");
                  tree t = TREE_ID_NAME (TREE_OPERAND (res, 0));
                  if (constant_exists (TREE_STRING_CST (t)) != NULL)
                    error ("duplicate constant definition for "
                           "variable `%s' found", TREE_STRING_CST (t));
                  else
                    tree_list_append (constant_list, res);
                }
              break;

            case tv_const:
              if ((res = handle_const (parser)) != error_mark_node)
                {
                  assert (TREE_CODE (res) == ASSIGN_EXPR
                          || TREE_CODE (res) == IDENTIFIER,
                          "Assign or id expression expected");

                  tree t = TREE_CODE (res) == ASSIGN_EXPR
                           ? TREE_ID_NAME (TREE_OPERAND (res, 0))
                           : TREE_ID_NAME (res);
                  if (constant_exists (TREE_STRING_CST (t)) != NULL)
                    error ("duplicate constant definition for "
                           "variable `%s' found", TREE_STRING_CST (t));
                  else
                    tree_list_append (constant_list, res);
                }
              break;
            
            case tv_proto:
              if ((res = handle_proto (parser)) != error_mark_node)
                {
                  tree proto;
                  tree name, ret, args;

                  assert (TREE_CODE (res) == FUNCTION_PROTO,
                          "Function prototype expected");

                  name = TREE_OPERAND (res, 0);
                  ret =  TREE_OPERAND (res, 1);
                  args = TREE_OPERAND (res, 2);
                  
                  if ((proto = function_proto_exists (name, ret, args))
                      != NULL)
                    {
                      /* FIXME if proto exists with the same ccall
                         then most likely it is an error.  */
                      warning_loc (TREE_LOCATION (res),
                                   "redefining ccall of the prototype");
                      TREE_FUNCTION_PROTO_CCALL (proto) 
                        = TREE_FUNCTION_PROTO_CCALL (res);
                    }       
                  else
                    tree_list_append (function_proto_list, res);
                }
              break;
            default:
              unreachable (0);
              break;
            }
          break;

        case tok_id:
        case tok_number:
        case tok_string:
        case tok_char:
          error_loc (token_location (tok), "unexpected token %s found",
                     token_as_string (tok));
          return -1;

        case tok_unknown:
          error_loc (token_location (tok), "unknown token found `%s'!",
                     token_as_string (tok));
          return -2;

        case tok_comments:
        default:
          continue;
        }
    }
  //printf ("note: finished parsing.\n");
  if (error_count != 0)
    {
      printf ("note: %i error(s) found.\n", error_count);
      return -3;
    }

  return 0;
}


int 
main (int argc, char *argv[])
{
  int ret = 0;
  struct lexer *  lex = (struct lexer *) malloc (sizeof (struct lexer));
  struct parser *  parser = (struct parser *) malloc (sizeof (struct parser));

  init_global ();
  init_global_tree ();
  init_function_protos ();

  if (argc <= 1)
    {
      fprintf (stderr, "filename argument required\n");
      ret = -1;
      goto cleanup;
    }

  if (!lexer_init (lex, argv[1]))
    {
      fprintf (stderr, "cannot create a lexer for file %s\n", argv[1]);
      ret = -2;
      goto cleanup;
    }

  parser_init (parser, lex);
  parse (parser);

  if (error_count != 0)
    goto cleanup;

  typecheck ();

  if (error_count != 0)
    goto cleanup;

  //optimise ();

  print_all (stdout);

cleanup:
  parser_finalize (parser);
  finalize_global_tree ();
  finalize_global ();
  
  /* That should be called at the very end.  */
  free_atomic_trees ();

  if (parser)
    free (parser);
  if (lex)
    free (lex);
  
  return ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
