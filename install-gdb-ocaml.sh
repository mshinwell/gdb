#!/bin/sh
set -eu
target_bindir=$1
gdb_install_root=$2
libmonda_install_root=$3
mkdir -p $target_bindir
cat > $target_bindir/gdb-ocaml << EOF
#!/bin/sh
export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:$libmonda_install_root
export OCAMLRUNPARAM=b
$gdb_install_root/bin/gdb "\$@"
EOF
chmod +x $target_bindir/gdb-ocaml
