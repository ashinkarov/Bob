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

#include "expand.h"
#include "tree.h"
#include "print.h"

static int level = 0;

#define indent(f, x)   \
    do { \
        int __i; \
        for (__i = 0; __i < (x); __i++) \
          fprintf (f, "  "); \
    } while (0)

int
print_expression (FILE *f, tree exp)
{
  /*if (exp != NULL)
    printf ("-- enter function %s with exp %s\n", __func__, 
            TREE_CODE_NAME (TREE_CODE (exp)));*/

  assert (exp != NULL 
          && (TREE_CODE_CLASS (TREE_CODE (exp)) == tcl_expression
              || TREE_CODE_CLASS (TREE_CODE (exp)) == tcl_constant
              || TREE_CODE (exp) == IDENTIFIER
              || exp == error_mark_node),
          "attempt to print non-expression tree %s",
          TREE_CODE_NAME (TREE_CODE (exp)));
  
  switch (TREE_CODE (exp))
    {
    case ERROR_MARK:
      return fprintf (f, "<<ERROR>>");
    case STRING_CST:
      return fprintf (f, "%s", TREE_STRING_CST (exp));
    case INTEGER_CST:
      return fprintf (f, "%i", TREE_INTEGER_CST (exp));
    case LIST_CST:
      {
        struct tree_list_element *  tle;
        tree lst = TREE_LIST_CST (exp);

        assert (TREE_CODE (lst) == LIST, 0);
        fprintf (f, "[");
        TAILQ_FOREACH (tle, &TREE_LIST_QUEUE (lst), entries)
          {
            print_expression (f, tle->element);
            if (TAILQ_NEXT (tle, entries))
              fprintf (f, ", ");
          }
        return fprintf (f, "]");
      }
    case IDENTIFIER:
      return print_expression (f, TREE_ID_NAME (exp));
    case CALL_EXPR:
      {
        tree name = TREE_OPERAND (exp, 0);
        tree lst = TREE_OPERAND (exp, 1);
        struct tree_list_element *  tle;

        assert (TREE_CODE (lst) == LIST, 0);
        print_expression (f, name);
        fprintf (f, " (");
        TAILQ_FOREACH (tle, &TREE_LIST_QUEUE (lst), entries)
          {
            print_expression (f, tle->element);
            if (TAILQ_NEXT (tle, entries))
              fprintf (f, ", ");
          }
        return fprintf (f, ")");
      }
    case COND_EXPR:
      {
        tree cond = TREE_OPERAND (exp, 0);
        tree if_expr = TREE_OPERAND (exp, 1);
        tree else_expr = TREE_OPERAND (exp, 2);

        print_expression (f, cond);
        fprintf (f, " ? ");
        print_expression (f, if_expr);
        fprintf (f, " : ");
        return print_expression (f, else_expr);
      }
    case UMINUS_EXPR:
      fprintf (f, " -");
      return print_expression (f, TREE_OPERAND (exp, 1));
    case TRUTH_NOT_EXPR:
      fprintf (f, " !");
      return print_expression (f, TREE_OPERAND (exp, 1));

    default:
      {
        const char *opcode;
        switch (TREE_CODE (exp))
          {
          case PLUS_EXPR: opcode = "+"; break;
          case MINUS_EXPR: opcode = "-"; break;
          case MULT_EXPR: opcode = "*"; break;
          case DIV_EXPR: opcode = "/"; break;
          case MOD_EXPR: opcode = "%"; break;
          case EQ_EXPR: opcode = "=="; break;
          case GT_EXPR: opcode = ">"; break;
          case GE_EXPR: opcode = ">="; break;
          case LT_EXPR: opcode = "<"; break;
          case LE_EXPR: opcode = "<="; break;
          case NE_EXPR: opcode = "!="; break;
          case TRUTH_OR_EXPR: opcode = "||"; break;
          case TRUTH_AND_EXPR: opcode = "&&"; break;
          case SHR_EXPR: opcode = ">>"; break;
          case SHL_EXPR: opcode = "<<"; break;
          case ASSIGN_EXPR: opcode = "="; break;
          default:
            unreachable (0);
          }
        /* for the time being in parens.  */
        fprintf (f, "(");
        print_expression (f, TREE_OPERAND (exp, 0));
        fprintf (f, " %s ", opcode);
        print_expression (f, TREE_OPERAND (exp, 1));
        return fprintf (f, ")");
      }
    }
}

int
print_statement (FILE *f, tree stmt)
{
  assert (stmt != NULL
          && (TREE_CODE_CLASS (TREE_CODE (stmt)) == tcl_statement
              || TREE_CODE_CLASS (TREE_CODE (stmt)) == tcl_expression
              || TREE_CODE (stmt) == IDENTIFIER), 0);

  if (TREE_CODE_CLASS (TREE_CODE (stmt)) == tcl_expression
      || TREE_CODE (stmt) == IDENTIFIER)
    return print_expression (f, stmt);
  else if (TREE_CODE_CLASS (TREE_CODE (stmt)) == tcl_statement)
    switch (TREE_CODE (stmt))
      {
      case FOR_STMT:
        fprintf (f, "for ");
        print_expression (f, TREE_OPERAND (stmt, 0));
        fprintf (f, " in ");
        print_expression (f, TREE_OPERAND (stmt, 1));
        fprintf (f, "\n");
        return print_stmt_list (f, TREE_OPERAND (stmt, 2));

      case IF_STMT:
        fprintf (f, "if ");
        print_expression (f, TREE_OPERAND (stmt, 0));
        fprintf (f, "\n");
        print_stmt_list (f, TREE_OPERAND (stmt, 1));
        if (TREE_OPERAND (stmt, 2) != NULL)
          {
            fprintf (f, " else \n");
            print_stmt_list (f, TREE_OPERAND (stmt, 2));
          }
        return 0;
      default:
        unreachable (0);
      }
  else
    unreachable (0);

  return -1;
}

int
print_stmt_list (FILE *f, tree stmt)
{
  struct tree_list_element *  tle;
  tree lst;

  assert (stmt != NULL
          && TREE_CODE (stmt) == STMT_LIST, 0);

  lst = TREE_STMT_LIST_STMTS (stmt);
  assert (TREE_CODE (lst) == LIST, 0);
  
  indent (f, level);
  fprintf (f, "{\n");
  level++;
  TAILQ_FOREACH (tle, &TREE_LIST_QUEUE (lst), entries)
    {
      indent (f, level);
      print_statement (f, tle->element);
      fprintf (f, ";\n");
    }
  level--;
  indent (f, level);
  fprintf (f, "}");

  return 0;
}

