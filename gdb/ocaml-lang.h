/* OCaml language support for GDB, the GNU debugger.
   Copyright (C) 2013--2015, Jane Street Holding

   Contributed by Mark Shinwell <mshinwell@janestreet.com>

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

#if !defined (OCAML_LANG_H)
#define OCAML_LANG_H 1

extern const char* OCAML_MAIN;

extern const char *ocaml_main_name (void);
extern char* ocaml_demangle (const char* mangled, int options);

#endif
