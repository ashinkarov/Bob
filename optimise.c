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

#include <ctype.h>
#include "expand.h"
#include "tree.h"
#include "global.h"
#include "optimise.h"
#include "print.h"


/* Add a quotatio marks to the string.
   The function reallocates the memory,
   frees old string and replaces the *STR
   pointer with the newly allocated string.  */
void
quote_string (char ** str)
{
  unsigned int size = strlen (*str);

  char *  str2 = (char *) malloc ((size + 3) * sizeof (char));
  strncpy (&str2[1], *str, size);
  str2[0] = '"';
  str2[size+1]= '"';
  str2[size+2]= '\0';
  free (*str);
  *str = str2;
}


tree
make_generate (tree arg)
{
  tree gen, tname, arglist;
  assert (TREE_TYPE (arg) == string_type_node, 0);

  gen = make_tree (CALL_EXPR);
  tname = make_string_cst_str ("generate");
  TREE_CONSTANT (tname) = 1;
  TREE_OPERAND_SET (gen, 0, tname);
  TREE_TYPE (tname) = string_type_node;
  TREE_TYPE (gen) = void_type_node;
  
  arglist = make_tree_list ();
  tree_list_append (arglist, arg);
  TREE_OPERAND_SET (gen, 1, arglist);
  return gen;
}

/* construct a list of generate calls from the generate argument
   of type const_string % [a, b, c]. For example:

   "foo (%1, %2);" % [str (a), str (b)]  -->
        generate "foo (";
        generate str (a);
        generate ", ";
        generate str (b);  */
tree
optimise_generate (tree modexp)
{
  tree lst;
  const char *  fmt;
  const char *  start;
  int len, i = 0;
  int fmt_length;

  assert (modexp != NULL && TREE_CODE (modexp) == MOD_EXPR
          && TREE_TYPE (modexp) == string_type_node 
          && TREE_CONSTANT (modexp), 
          "cannot optimise the genarray expression");

  lst = make_tree_list ();
  fmt = TREE_STRING_CST (TREE_OPERAND (modexp, 0));
  //fprintf (stderr, "-- looking at string: %s\n", fmt);

  /* Address the quotes at the beginning
     and at the end.  */
  len = -1;
  start = &fmt[1];
  fmt_length = strlen (fmt) - 1;
  while (i < fmt_length)
    {
      if (fmt[i] == '%') {
        if (i+1 == fmt_length)
          {
            error_loc (TREE_LOCATION (modexp), 
                       "%% found at the end of string");
            return NULL;
          }
        else if (isdigit (fmt[i+1]))
          {
            const int sz = 16;
            int j = i + 1;
            char buf[sz];
            char *str;
            int val;
            tree gen1, gen2;
            tree tmp, listelement;

            while (j < fmt_length && isdigit (fmt[j]))
              j++;

            if (j - i > sz)
              {
                error_loc (TREE_LOCATION (modexp),
                           "index number out of range");
                free_tree (lst);
                return NULL;
              }

            strncpy (buf, &fmt[i+1], j - i - 1);
            buf[j - i -1] = '\0';
            val = atoi (buf);
            //fprintf (stderr, "-- found index of %% %i\n", val);
            listelement 
                = list_at_position (TREE_LIST_CST (TREE_OPERAND (modexp, 1)), 
                                    val-1);

            assert (TREE_TYPE (listelement) == string_type_node, 0);

            if (listelement == NULL)
              {
                error_loc (TREE_LOCATION (modexp),
                           "list index out of range");
                free_tree (lst);
                return NULL;
              }
            
            /* Constant part of the string until '%'  */
            str = (char *) malloc ((len+1) *sizeof (char));
            str = strncpy (str, start, len);
            str[len] = '\0';
            quote_string (&str);
            tmp = make_string_cst_str (str);
            TREE_CONSTANT (tmp) = 1;
            TREE_TYPE (tmp) = string_type_node;
            gen1 = make_generate (tmp);

            /* %<elemnt> expression  */
            gen2 = make_generate (copy_tree (listelement));
            free (str);

            /* Append both generates to the return list.  */
            tree_list_append (lst, gen1);
            tree_list_append (lst, gen2);

            i = j;
            start = &fmt[i];
            len = 0;
            continue;
          }
        else if (fmt[i+1] == '%')
          {
            i += 2;
            len += 2;
            continue;
          }
        else
          {
            error_loc (TREE_LOCATION (modexp),
                       "single '%%' found in the format string");
            free_tree (lst);
            return NULL; 
          }
        }
      i += 1;
      len += 1;
    }

  if (len > 0)
    {
      char * str;
      tree tmp, gen;
      str = (char *) malloc ((len+1) * sizeof (char));
      str = strncpy (str, start, len);
      str[len] = '\0';
      quote_string (&str);

      tmp = make_string_cst_str (str);
      TREE_TYPE (tmp) = string_type_node;
      TREE_CONSTANT (tmp) = 1;
      gen = make_generate (tmp);
      tree_list_append (lst, gen);
      free (str);
    }

  return lst;
}

void
optimise_generates (tree expand)
{
  struct tree_list_element *  tel;
  tree stmts;

  assert (TREE_CODE (expand) == EXPAND_STMT, "expand statement expected");
  stmts = TREE_STMT_LIST_STMTS (TREE_OPERAND (expand, 2));

  TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (stmts), entries)
    {
      tree stmt = tel->element;

      if (TREE_CODE (stmt) == CALL_EXPR
          && strcmp (TREE_STRING_CST (TREE_OPERAND (stmt, 0)), "generate") 
             == 0)
        {
          struct tree_list_element *  ptr;
          tree arg;

          ptr = TAILQ_FIRST (&TREE_LIST_QUEUE (TREE_OPERAND (stmt, 1)));
          arg = ptr->element;

          assert (TREE_TYPE (arg) == string_type_node, 0);
          if (TREE_CODE (arg) == MOD_EXPR && TREE_CONSTANT (arg))
            {
              tree x;
              //warning_loc (TREE_LOCATION (stmt), "optimising generate");
              x = optimise_generate (arg);
              if (x != NULL && !TAILQ_EMPTY (&TREE_LIST_QUEUE (x)))
                {
                  struct tree_list_element *  first;
                  struct tree_list_element *  last;

                  /* Insert the elements of list X a the place
                     where TEL points.  */
                  first = TAILQ_FIRST (&TREE_LIST_QUEUE (x));
                  last = TAILQ_LAST (&TREE_LIST_QUEUE (x), tree_list);
                  TAILQ_PREV (first, tree_list, entries) = tel;
                  if (TAILQ_NEXT (tel, entries) != NULL)
                    TAILQ_PREV (TAILQ_NEXT (tel, entries), 
                                tree_list, entries) = last;
                  TAILQ_NEXT (last, entries) = TAILQ_NEXT (tel, entries);
                  TAILQ_NEXT (tel, entries) = first;
                 
                  /* Remove current TEL and move the TEL to the last
                     element of X.  */
                  TAILQ_REMOVE (&TREE_LIST_QUEUE (stmts), tel, entries);
                  free_tree (tel->element);
                  free (tel);
                  free (x);
                  tel = last;
                }
            }
        }
    }
}


void
optimise ()
{
  struct tree_list_element *  tel;

  TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (function_list), entries)
    if (TREE_CODE (tel->element) == EXPAND_STMT)
      {
        optimise_generates (tel->element);
      }
}
