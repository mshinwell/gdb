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

for header in $(find . -name "*.h" -or -name "*.def"); do
  dir=$(dirname "$header")
  install_dirname=$gdb_install_root/src/$dir
  mkdir -p $install_dirname
  cp $header $install_dirname
done
