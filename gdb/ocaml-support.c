/* Interface to the OCaml support library for GDB, the GNU debugger.

   Contributed by Jane Street Europe.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include <stdio.h>
#include <dlfcn.h>

#include "ocaml-support.h"

#define LIB_SUPPORT "libgdb_ocaml_support.so"

#define SET_STUB(stubs,handle,name) \
  stubs->name = dlsym(handle, "gdb_ocaml_support_" #name)

static void *
ocaml_support_init (struct gdb_ocaml_support *stubs)
{
  void *handle = NULL;
  int (*support_init) (void) = NULL;

  handle = dlopen (LIB_SUPPORT, RTLD_LAZY | RTLD_GLOBAL);
  if (handle == NULL)
    {
      fprintf (stderr, "ocaml support: cannot load library %s (%s)\n",
               LIB_SUPPORT, dlerror());
      return NULL;
    }

  support_init = dlsym (handle, "gdb_ocaml_support_init");
  if (support_init == NULL)
    {
      fprintf (stderr, 
               "ocaml support: missing symbol "
               "gdb_ocaml_support_init in %s (%s)\n", 
               LIB_SUPPORT, dlerror());
      dlclose (handle);
      return NULL;
    }

  if (support_init () == 0)
    {
      fprintf (stderr, "ocaml support: initialization failed\n");
      dlclose (handle);
      return NULL;
    }

  SET_STUB (stubs, handle, val_print);
  SET_STUB (stubs, handle, demangle);
  SET_STUB (stubs, handle, partially_mangle);
  SET_STUB (stubs, handle, print_type);
  SET_STUB (stubs, handle, compile_and_run_expression);

  return handle;
}

struct gdb_ocaml_support *
ocaml_support_library (void)
{
  static int initialized = 0;
  static void *handle = NULL;
  static struct gdb_ocaml_support stubs;

  if (initialized == 0)
    {
      handle = ocaml_support_init (&stubs);
      initialized = 1;
    }

  if (handle == NULL)
    return NULL;
  else
    return &stubs;
}

int
ocaml_support_val_print (struct type *type, struct symbol *symbol,
                         const gdb_byte *valaddr,
                         int embedded_offset,
                         CORE_ADDR address, struct ui_file *stream,
                         int recurse, const struct value *val,
                         const struct value_print_options *options,
                         int depth)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();
  if (stubs && stubs->val_print)
    {
      stubs->val_print (type, symbol, valaddr, embedded_offset, address, stream,
                        recurse, val, options, depth);
      return 1;
    }
  return 0;
}

char*
ocaml_support_partially_mangle(const char* name)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();
  if (stubs && stubs->val_print)
    {
      return (stubs->partially_mangle (name));
    }
  return NULL;
}

char*
ocaml_support_demangle(const char* mangled, int options)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();
  if (stubs && stubs->val_print)
    {
      return (stubs->demangle (mangled, options));
    }
  return NULL;
}

void
ocaml_support_print_type (struct type *type, struct ui_file *stream)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();
  if (stubs && stubs->print_type)
    {
      return (stubs->print_type (type, stream));
    }
}

void
ocaml_support_compile_and_run_expression (const char *expr_text,
                                          const char **vars_in_scope_names,
                                          CORE_ADDR *vars_in_scope_values,
                                          int num_vars_in_scope,
                                          struct ui_file *stream)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();
  if (stubs && stubs->compile_and_run_expression)
    {
      stubs->compile_and_run_expression (expr_text, vars_in_scope_names,
                                         vars_in_scope_values, num_vars_in_scope,
                                         stream);
    }
}
