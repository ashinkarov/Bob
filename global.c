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

/* Variable that is going to be increased every 
   time when an error is happening.  */
int error_count = 0;

/* Variable that is going to be increased every 
   time when an error is happening.  */
int warning_count = 0;


/* Table that stores user-defined types.  */
tree user_types = NULL;


/* Allocat all the global structures that are going to be used
   during the compilation.  */
void 
init_global ()
{
  assert (user_types == NULL, "user types are already allocated");
  
  user_types = make_tree (LIST);
  TAILQ_INIT (&TREE_LIST_QUEUE (user_types));
  
  error_count = 0;
  warning_count = 0;
}

void
finalize_global ()
{
  free_tree (user_types);
}

/* Function returns tree if the used-defined type exists in the 
   table and NULL otherwise.  */
tree 
user_type_defined (const char *  name)
{
  struct tree_list_element *  tel;
  
  assert (user_types != NULL, "user types are not allocated");
  /* assert (TREE_CODE (name) == STRING_CST, "type name must be string"); */

  TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (user_types), entries)
    {
      /*printf ("-- %s, %s\n", TREE_STRING_CST (TREE_USER_TYPE_NAME (tel->element)), name);*/
      if (strcmp (TREE_STRING_CST (TREE_USER_TYPE_NAME (tel->element)), name) == 0)
        return tel->element;
    }
  return NULL;
}


/* Adds type named NAME to the user-defined type table USER_TYPES.  */
tree 
add_user_type (tree name)
{
  struct tree_list_element *tel;
  tree t;

  assert (TREE_CODE (name) == STRING_CST, "user-type name must be a string");
  t = user_type_defined (TREE_STRING_CST (name));
  if (t != NULL)
    {
      warning ("type redefined");
      return error_mark_node;
    }

  tel = (struct tree_list_element *) malloc (sizeof (struct tree_list_element));
  t = make_tree (USER_TYPE);
  TREE_USER_TYPE_NAME (t) = name;
  tel->element = t;

  TAILQ_INSERT_TAIL (&TREE_LIST_QUEUE (user_types), tel, entries);
  return t;
}

