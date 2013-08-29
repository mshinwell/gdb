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
      fprintf (stderr, "ocaml support: cannot load library %s\n", LIB_SUPPORT);
      return NULL;
    }

  support_init = dlsym (handle, "gdb_ocaml_support_init");
  if (support_init == NULL)
    {
      fprintf (stderr, 
               "ocaml support: missing symbol gdb_ocaml_support_init in %s\n", 
               LIB_SUPPORT);
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
ocaml_support_val_print (struct type *type, const gdb_byte *valaddr,
                         int embedded_offset,
                         CORE_ADDR address, struct ui_file *stream,
                         int recurse, const struct value *val,
                         const struct value_print_options *options,
                         int depth)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();
  if (stubs && stubs->val_print)
    {
      stubs->val_print (type, valaddr, embedded_offset, address, stream,
                        recurse, val, options, depth);
      return 1;
    }
  return 0;
}
