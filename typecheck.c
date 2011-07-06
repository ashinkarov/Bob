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
#include "global.h"
#include "typecheck.h"

int
typecheck ()
{
  struct tree_list_element *  tl;
  int const_check = 0, expand_check = 0, function_check = 0;

  TAILQ_FOREACH (tl, &TREE_LIST_QUEUE (constant_list), entries)
    {
      const_check += typecheck_constant (tl->element);
    }

  TAILQ_FOREACH (tl, &TREE_LIST_QUEUE (function_list), entries)
    {
      if (TREE_CODE (tl->element) != EXPAND_STMT)
        continue;

      expand_check += typecheck_expand (tl->element);
    }

  /* FIXME We do NOT check typecheck for functions at the moment
     because we do not support them.  */
 
  return const_check + expand_check + function_check;
}

int
typecheck_constant (tree cst)
{
  tree lhs, rhs;
  int ret = 0;

  assert (TREE_CODE (cst) == ASSIGN_EXPR, 0);

  /* XXX At the time being we assume that the constants could
     be only a list of string expressions. Seriously:
        varname = ["str", "str", ...]  */
  lhs = TREE_OPERAND (cst, 0);
  rhs = TREE_OPERAND (cst, 1);

  if (TREE_TYPE (rhs) == NULL)
    ret = typecheck_expression (rhs);

  if (TREE_TYPE (rhs) != list_type_node || !TREE_CONSTANT (rhs))
    {
      error_loc (TREE_LOCATION (rhs), 
                 "constant list expected in the assignment");
      return 1;
    }

  if (TREE_TYPE (lhs) == NULL)
    {
      TREE_TYPE (rhs) = TREE_TYPE (lhs);
      TREE_CONSTANT (rhs) = TREE_CONSTANT (lhs);
    }
  else if (TREE_TYPE (rhs) != TREE_TYPE (lhs) 
           || TREE_CONSTANT (rhs) != TREE_CONSTANT (lhs))
    {
      error_loc (TREE_LOCATION (lhs), "types in the assignment do not match");
      return 1;
    }

  return ret;
}

int
typecheck_expand (tree exp)
{
  assert (TREE_CODE (exp) == EXPAND_STMT, 0);


  return 0;
}

int
typecheck_function (tree func)
{
  return 0;
}

int
typecheck_expression (tree expr)
{
  return 0;
}


int
typecheck_statement (tree stmt)
{
  return 0;
}


