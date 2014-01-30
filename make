#!/bin/bash

#	Name: make.sh
#	Author: Johan Hidding
#	Licence: do whatever you like with this, there's no waranty.
# -----------------
#	Description: this simple script should be able to compile and
#	link a simple but well structured c/c++ project.
# -----------------
#	Details: compiles every .cc file found in all subdirectories. 
#	the script checks the modification times of the source files
#	and their header dependencies against the target files.
#	finally all .o files are linked to $target.
#	if you want to exclude files from compiling, put them in a
#	hidden directory (or somewhere outside the directory
#	structure, which is even better)

target="scam"
objdir="obj"

CAIRO_LDFLAGS=$(pkg-config --libs cairomm-1.0)
CAIRO_CFLAGS=$(pkg-config --cflags cairomm-1.0)

LDFLAGS="-lm -lrt -lfftw3 -lgsl -lgslcblas -lCGAL -lgmp -lboost_thread -lmpfr -fopenmp ${CAIRO_LDFLAGS}"
CFLAGS="-g -std=c++0x -O2 -frounding-math -fopenmp -I/mnt/Prei/local/include ${CAIRO_CFLAGS}"

CC="g++"
ext=".cc"
ECHO="echo -e"

#/>==--------                                  --------==>\#
#>=----- you shouldn't need to edit below this line -----=<#
#\>==--------                                  --------==>/#

DIRS=`find src -maxdepth 1 -type d -name '[^\.]*'`
CCFILES=`find src -maxdepth 2 -wholename "[^.]*$ext"`

case "$TERM" in
	dumb)
		prettyprint() {
			$ECHO " * $3 "
			if $1; then
				# $ECHO "\t[done]"
				return 1
			else
				$ECHO "\t[failed]"
				return 0
			fi
		} ;;
	*)
		prettyprint() {
			$ECHO "\033[$2m*\033[m $3"
			if $1; then
				$ECHO "\r\033[A\033[63C[\033[32mdone\033[m]"
				return 1
			else
				$ECHO "\033[62G[\033[31mfailed\033[m]"
				return 0
			fi
		} ;;
esac

checknewer() {
	local k
	if [ ! -e $1 ]; then
		return 0
	fi

	for k in $2; do
		if [ $k -nt $1 ]; then
			return 0
		fi
	done
	return 1
}

compile() {
	dirn=$objdir/$(dirname $1)
	if [ ! -e $dirn ]; then
		mkdir -p $dirn
	fi

	objf=$dirn/$(basename $1 $ext).o
	deps=`$CC -MM $1 $CFLAGS | sed -e '{ s/^.*: //; s/\\\//; s/^ *// }'`
	if checknewer $objf "$deps"; then
		if prettyprint "$CC -c $CFLAGS $1 -o $objf" 34 "Compiling $1 ... "; then
			exit 1
		fi
	fi
}

compile_unittest() {
	dirn=${objdir}.test/$(dirname $1)
	if [ ! -e $dirn ]; then
		mkdir -p $dirn
	fi

	objf=$dirn/$(basename $1 $ext).o
	deps=`$CC -MM $1 -DUNITTEST $CFLAGS | sed -e '{ s/^.*: //; s/\\\//; s/^ *// }'`
	if checknewer $objf "$deps"; then
		if prettyprint "$CC -c $CFLAGS $1 -DUNITTEST -o $objf" 34 "Compiling unit test $1 ... "; then
			exit 1
		fi
	fi
}

case "$1" in
	single)
		compile $2 ;;
		
	all)
		for f in $CCFILES; do
			compile $f
		done

		objfiles=$(find $objdir -name '*.o')
		if checknewer $target "$objfiles"; then
			if prettyprint "$CC $objfiles -o $target $LDFLAGS" 36 "Linking ..."; then
				exit 1
			fi
		else
			echo "$target allready is up to date."
		fi ;;

	build-test)
		for f in $CCFILES; do
			compile_unittest $f
		done

		objfiles=$(find ${objdir}.test -name '*.o')
		if checknewer ${target}.test "$objfiles"; then
			if prettyprint "$CC $objfiles -o ${target}.test $LDFLAGS" 36 "Linking test ..."; then
				exit 1
			fi
		else
			echo "$target.test allready is up to date."
		fi ;;

	run-test)
		if $0 build-test; then
			echo
			echo "[ running test ]-------------------------------------------"
			echo
			exec ./$target.test
		else
			echo
			echo "compilation failed, not running test"
		fi ;;

	run)
		if $0 all; then
			echo
			echo "[ running target ]-----------------------------------------"
			echo
			exec ./$target
		else
			echo
			echo "compilation failed, not running target."
		fi;;

	clean)
		rm -rf $target $objdir ${target}.test ${objdir}.test
		find . -name '*~' -exec rm {} \; ;;

	*)
		echo "This is a make script, written in bash. It is only good"
		echo "for compiling well-structured C++ programs. Read the"
		echo "source for help on configuring the script."
		echo
		echo "run> ./make [command]"
		echo "where [command] ∈ {single, all, build-test, run-test, run, clean}."
		echo
		echo "'make single' expects one more argument giving a .cc file"
		echo "that you want to compile."
esac

