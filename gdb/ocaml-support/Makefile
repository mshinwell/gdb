CC=gcc
OCAMLC=ocamlc
OCAMLOPT=ocamlopt

OCAML_LIB=`ocamlc -where`

CFLAGS='-I .. -I ../common -I ../../include -ggdb -I../../../mshinwell-ocaml/stdlib/'
OCAMLFLAGS=-I $(OCAML_LIB)/compiler-libs
SRC=gdb_ocaml_support.c ml_gdb.c std.ml gdb.ml \
    cmt_file.mli cmt_file.ml demangle.ml printer.ml gdb_ocaml_support.ml
TARGET_LIB=libgdb_ocaml_support.so

.PHONY: all
all: bytecode 
	@#native 

.PHONY: clean
clean: 
	@rm -f *.cm? *.cm??
	@rm -f *.exe *.so *.o

.PHONY: bytecode
bytecode:
	$(OCAMLC) -output-obj -o $(TARGET_LIB) $(OCAMLFLAGS) -ccopt $(CFLAGS) \
          $(OCAML_LIB)/compiler-libs/ocamlcommon.cma \
          $(OCAML_LIB)/compiler-libs/ocamloptcomp.cma \
	  dynlink.cma bigarray.cma -runtime-variant _shared \
	  $(SRC)

## Native support need PIC-enabled toolchain, e.g. configured with:
## ./configure -prefix %{prefix}% -cc "gcc -fPIC" -aspp "gcc -c -fPIC"]
#.PHONY: native
#native:
#	$(OCAMLOPT) -output-obj -o $(TARGET_LIB) \
#	  dynlink.cmxa bigarray.cmxa -runtime-variant _shared \
#	  $(SRC)

## If we later want to use Dynlink
#.PHONY: plugin
#plugin:
#	@$(OCAMLC) -c plugin.ml
#	#@$(OCAMLOPT) -o plugin.cmxs -shared plugin.ml

