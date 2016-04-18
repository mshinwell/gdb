CC=gcc
OCAMLC=ocamlc
OCAMLOPT=ocamlopt

OCAML_LIB=`ocamlc -where`

CFLAGS='-fPIC -I .. -I ../common -I ../../include -ggdb -I../../../mshinwell-ocaml/stdlib'
OCAMLFLAGS=-I $(OCAML_LIB)/compiler-libs -I /home/mark/dev/mshinwell-ocaml/typing/ -g -fPIC
SRC=gdb_ocaml_support.c ml_gdb.c std.ml gdb.ml debug.ml \
    cmt_cache.mli cmt_cache.ml \
    cmt_file.mli cmt_file.ml dwarf_type.ml \
    abstraction_breaker.mli abstraction_breaker.ml \
    list_oracle.mli list_oracle.ml \
    type_oracle.mli type_oracle.ml \
    print_closure.mli print_closure.ml \
    demangle.ml printer.ml compile_and_run.ml gdb_ocaml.ml
TARGET_LIB=libgdb_ocaml_support.so

.PHONY: all
all: native 

.PHONY: clean
clean: 
	@rm -f *.cm? *.cm??
	@rm -f *.exe *.so *.o

## Native support need PIC-enabled toolchain, e.g. configured with:
## ./configure -prefix %{prefix}% -cc "gcc -fPIC" -aspp "gcc -c -fPIC"]
.PHONY: native
native:
	$(OCAMLOPT) -fPIC -output-obj -o $(TARGET_LIB) $(OCAMLFLAGS) -ccopt $(CFLAGS) \
	$(OCAML_LIB)/compiler-libs/ocamlcommon.cmxa \
	$(OCAML_LIB)/compiler-libs/ocamloptcomp.cmxa \
	dynlink.cmxa bigarray.cmxa \
	$(SRC)

## If we later want to use Dynlink
#.PHONY: plugin
#plugin:
#	@$(OCAMLC) -c plugin.ml
#	#@$(OCAMLOPT) -o plugin.cmxs -shared plugin.ml

