#!/usr/bin/env bash

# A simple test runner. Synopsis:
#
# ./run.sh foo.mo [bar.mo ..]
#
# Options:
#
#    -a: Update the files in ok/
#    -d: Run on in drun (or, if not possible, in ic-ref-run)
#    -t: Only typecheck
#    -s: Be silent in sunny-day execution
#    -i: Only check mo to idl generation
#    -p: Produce perf statistics
#        only compiles and runs drun, writes stats to $PERF_OUT
#    -v: Translate to Viper
#

function realpath() {
    [[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
}


ACCEPT=no
DTESTS=no
IDL=no
PERF=no
VIPER=no
WASMTIME_OPTIONS="-C cache=n -W nan-canonicalization=y -W multi-memory -W bulk-memory"
WRAP_drun=$(realpath $(dirname $0)/drun-wrapper.sh)
WRAP_ic_ref_run=$(realpath $(dirname $0)/ic-ref-run-wrapper.sh)
SKIP_RUNNING=${SKIP_RUNNING:-no}
SKIP_VALIDATE=${SKIP_VALIDATE:-no}
ONLY_TYPECHECK=no
ECHO=echo

export WASMTIME_NEW_CLI=1

while getopts "adpstirv" o; do
    case "${o}" in
        a)
            ACCEPT=yes
            ;;
        d)
            DTESTS=yes
            ;;
        p)
            PERF=yes
            ;;
        s)
            ECHO=true
            ;;
        t)
            ONLY_TYPECHECK=true
            ;;
        i)
            IDL=yes
            ;;
        v)
            VIPER=yes
            ;;
    esac
done

shift $((OPTIND-1))

failures=()

function normalize () {
  if [ -e "$1" ]
  then
    grep -a -E -v '^Raised by|^Raised at|^Re-raised at|^Re-Raised at|^Called from|^ +at ' $1 |
    grep -a -E -v 'note: using the' |
    sed -e 's/\x00//g' \
        -e 's/\x1b\[[0-9;]*[a-zA-Z]//g' \
        -e 's/^.*[IW], hypervisor:/hypervisor:/g' \
        -e 's/wasm:0x[a-f0-9]*:/wasm:0x___:/g' \
        -e 's/prelude:[^:]*:/prelude:___:/g' \
        -e 's/prim:[^:]*:/prim:___:/g' \
        -e 's/ calling func\$[0-9]*/ calling func$NNN/g' \
        -e 's/rip_addr: [0-9]*/rip_addr: XXX/g' \
        -e 's,/private/tmp/,/tmp/,g' \
        -e 's,/tmp/.*ic.[^/]*,/tmp/ic.XXX,g' \
        -e 's,/build/.*ic.[^/]*,/tmp/ic.XXX,g' \
        -e 's,^.*/idl/_out/,..../idl/_out/,g' | # node puts full paths in error messages
    sed -e 's,\([a-zA-Z0-9.-]*\).mo.mangled,\1.mo,g' \
        -e 's/trap at 0x[a-f0-9]*/trap at 0x___:/g' \
        -e 's/^\(         [0-9]\+:\).*!/\1 /g' | # wasmtime backtrace locations
    sed -e 's/^  \(         [0-9]\+:\).*!/\1 /g' | # wasmtime backtrace locations (later version)
    sed -e 's/wasm `unreachable` instruction executed/unreachable/g' | # cross-version normalisation
    sed -e 's/Ignore Diff:.*/Ignore Diff: (ignored)/ig' \
        -e 's/Motoko compiler (source .*)/Motoko compiler (source XXX)/ig' \
        -e 's/Motoko compiler [^ ]* (source .*)/Motoko compiler (source XXX)/ig' |

    # Normalize canister id prefixes and timestamps in debug prints
    sed -e 's/\[Canister [0-9a-z\-]*\]/debug.print:/g' \
        -e 's/^20.*UTC: debug.print:/debug.print:/g' |
    # Normalize instruction locations on traps, added by ic-ref ad6ea9e
    sed -e 's/region:0x[0-9a-fA-F]\+-0x[0-9a-fA-F]\+/region:0xXXX-0xXXX/g' |
    # Delete everything after Oom
    sed -e '/RTS error: Cannot grow memory/q' \
        -e '/RTS error: Cannot allocate memory/q' |
    # Delete Viper meta-output
    sed -e '/^Silicon /d' \
        > $1.norm
    mv $1.norm $1
  fi
}

function run () {
  # first argument: extension of the output file
  # remaining argument: command line
  # uses from scope: $out, $file, $base, $diff_files

  local ext="$1"
  shift

  if grep -q "^//SKIP $ext$" $(basename $file); then return 1; fi

  if test -e $out/$base.$ext
  then
    echo "Output $ext already exists."
    exit 1
  fi

  $ECHO -n " [$ext]"
  $ECHO "$@" >& $out/$base.$ext
  "$@" >& $out/$base.$ext
  local ret=$?

  if [ $ret != 0 ]
  then echo "Return code $ret" >> $out/$base.$ext.ret
  else rm -f $out/$base.$ext.ret
  fi
  diff_files="$diff_files $base.$ext.ret"

  normalize $out/$base.$ext
  diff_files="$diff_files $base.$ext"

  return $ret
}

# 'run_stderr' is a variant of 'run' that redirects stdout and stderr into
# separate files instead of merging them. It was created by copy&paste and
# applying minor tweaks. If the 'run' function is changed, it is quite likely
# that 'run_stderr' requires similar changes.
#
# Separation of stdout/stderr is necessary when e.g. producing .vpr files so
# that diagnostic messages don't get interspersed with the generated Viper code.
function run_stderr () {
  # first argument: extension of the output file
  # remaining argument: command line
  # uses from scope: $out, $file, $base, $diff_files

  local ext="$1"
  shift

  if grep -q "^//SKIP $ext$" $(basename $file); then return 1; fi

  if test -e $out/$base.$ext
  then
    echo "Output $ext already exists."
    exit 1
  fi

  $ECHO -n " [$ext]"
  "$@" > $out/$base.$ext 2>$out/$base.$ext.stderr
  local ret=$?

  if [ $ret != 0 ]
  then echo "Return code $ret" >> $out/$base.$ext.ret
  else rm -f $out/$base.$ext.ret
  fi
  diff_files="$diff_files $base.$ext.ret"

  normalize $out/$base.$ext.stderr
  diff_files="$diff_files $base.$ext.stderr"

  normalize $out/$base.$ext
  diff_files="$diff_files $base.$ext"

  return $ret
}

function run_if () {
  # first argument: a file extension
  # remaining argument: passed to run

  local ext="$1"
  shift

  if test -e $out/$base.$ext
  then
    run "$@"
  else
    return 1
  fi
}

if [ "$PERF" = "yes" ]
then
  if [ -z "$PERF_OUT" ]
  then
    echo "Warning: \$PERF_OUT not set" >&2
  fi
fi

HAVE_drun=no
HAVE_ic_ref_run=no
HAVE_ic_wasm=no

FLAGS_drun=
FLAGS_ic_ref_run=-ref-system-api

if [ $DTESTS = yes -o $PERF = yes ]
then
  if drun --help >& /dev/null
  then
    HAVE_drun=yes
  else
    if [ $ACCEPT = yes ]
    then
      echo "ERROR: Could not run drun, cannot update expected test output"
      exit 1
    else
      echo "WARNING: Could not run drun, will skip running some tests"
      HAVE_drun=no
    fi
  fi
  if ic-wasm --help >& /dev/null
  then
    HAVE_ic_wasm=yes
  fi
fi

if [ $DTESTS = yes ]
then
  if ic-ref-run --help >& /dev/null
  then
    HAVE_ic_ref_run=yes
  else
    if [ $ACCEPT = yes ]
    then
      echo "WARNING: Could not run ic-ref-run, cannot update expected test output"
    else
      echo "WARNING: Could not run ic-ref-run, will skip running some tests"
      HAVE_ic_ref_run=no
    fi
  fi
fi


for file in "$@";
do
  if ! [ -r $file ]
  then
    echo "File $file does not exist."
    failures+=("$file")
    continue
  fi

  if [ ${file: -3} == ".mo" ]
  then base=$(basename $file .mo); ext=mo
  elif [ ${file: -3} == ".sh" ]
  then base=$(basename $file .sh); ext=sh
  elif [ ${file: -4} == ".wat" ]
  then base=$(basename $file .wat); ext=wat
  elif [ ${file: -4} == ".did" ]
  then base=$(basename $file .did); ext=did
  elif [ ${file: -4} == ".cmp" ]
  then base=$(basename $file .cmp); ext=cmp
  elif [ ${file: -5} == ".drun" ]
  then base=$(basename $file .drun); ext=drun
  else
    echo "Unknown file extension in $file"
    echo "Supported extensions: .mo .sh .wat .did .drun"
    failures+=("$file")
    continue
  fi

  # We run all commands in the directory of the .mo file,
  # so that no paths leak into the output
  pushd $(dirname $file) >/dev/null

  out=_out
  ok=ok

  $ECHO -n "$base:"
  [ -d $out ] || mkdir $out
  [ -d $ok ] || mkdir $ok

  rm -rf $out/$base $out/$base.*

  # First run all the steps, and remember what to diff
  diff_files=

  case $ext in
  "mo")
    if grep -q "//INCREMENTAL-GC-ONLY" $base.mo
    then
      if [[ $EXTRA_MOC_ARGS != *"--incremental-gc"* ]]
      then
        $ECHO " Skipped (non-incremental GC)"
        continue
      fi
    fi

    # extra flags (allow shell variables there)
    moc_extra_flags="$(eval echo $(grep '//MOC-FLAG' $base.mo | cut -c11- | paste -sd' '))"
    moc_extra_env="$(eval echo $(grep '//MOC-ENV' $base.mo | cut -c10- | paste -sd' '))"
    if ! grep -q "//MOC-NO-FORCE-GC" $base.mo
    then
      TEST_MOC_ARGS="--force-gc $EXTRA_MOC_ARGS"
    else
      TEST_MOC_ARGS=$EXTRA_MOC_ARGS
    fi
    if [ $VIPER = 'yes' ]
    then
      TEST_MOC_ARGS="$TEST_MOC_ARGS --package base pkg/base"
    fi
    moc_with_flags="env $moc_extra_env moc $moc_extra_flags $TEST_MOC_ARGS"

    # Typecheck
    run tc $moc_with_flags --check $base.mo
    tc_succeeded=$?

    if [ "$tc_succeeded" -eq 0 -a "$ONLY_TYPECHECK" = "no" ]
    then
      if [ $IDL = 'yes' ]
      then
        run idl $moc_with_flags --idl $base.mo -o $out/$base.did
        idl_succeeded=$?

        normalize $out/$base.did
        diff_files="$diff_files $base.did"

        if [ "$idl_succeeded" -eq 0 ]
        then
          run didc didc --check $out/$base.did
        fi
      elif [ $VIPER = 'yes' ]
      then
        run_stderr vpr $moc_with_flags --viper $base.mo -o $out/$base.vpr
        vpr_succeeded=$?

        normalize $out/$base.vpr
        diff_files="$diff_files $base.vpr"

        if [ "$vpr_succeeded" -eq 0 ]
        then
	  run silicon java -Xmx2048m -Xss16m -cp $VIPER_SERVER \
	    viper.silicon.SiliconRunner --logLevel OFF --z3Exe $(which z3) \
	    $out/$base.vpr
        fi
      else
        if [ "$SKIP_RUNNING" != yes -a "$PERF" != yes ]
        then
          # Interpret
          run run $moc_with_flags --hide-warnings -r $base.mo

          # Interpret IR without lowering
          run run-ir $moc_with_flags --hide-warnings -r -iR -no-async -no-await $base.mo

          # Diff interpretations without/with lowering
          if [ -e $out/$base.run -a -e $out/$base.run-ir ]
          then
            diff -u -N --label "$base.run" $out/$base.run --label "$base.run-ir" $out/$base.run-ir > $out/$base.diff-ir
            diff_files="$diff_files $base.diff-ir"
          fi

          # Interpret IR with lowering
          run run-low $moc_with_flags --hide-warnings -r -iR $base.mo

          # Diff interpretations without/with lowering
          if [ -e $out/$base.run -a -e $out/$base.run-low ]
          then
            diff -u -N --label "$base.run" $out/$base.run --label "$base.run-low" $out/$base.run-low > $out/$base.diff-low
            diff_files="$diff_files $base.diff-low"
          fi

        fi

        # Mangle for compilation:
        # The compilation targets do not support self-calls during canister
        # installation, so this replaces
        #
        #     actor a { … }
        #     a.go(); //OR-CALL …
        #
        # with
        #
        #     actor a { … }
        #     //CALL …
        #
        # which actually works on the IC platform

        # needs to be in the same directory to preserve relative paths :-(
        mangled=$base.mo.mangled
        sed 's,^.*//OR-CALL,//CALL,g' $base.mo > $mangled


        # Compile
        if [ $DTESTS = yes ]
        then
          run comp $moc_with_flags $FLAGS_drun --hide-warnings --map -c $mangled -o $out/$base.wasm
          run comp-ref $moc_with_flags $FLAGS_ic_ref_run --hide-warnings --map -c $mangled -o $out/$base.ref.wasm
        elif [ $PERF = yes ]
        then
          run comp $moc_with_flags --hide-warnings --map -c $mangled -o $out/$base.wasm
          if [ $HAVE_ic_wasm = yes ]; then
            run opt ic-wasm -o $out/$base.opt.wasm $out/$base.wasm optimize O3 --keep-name-section
          fi
        else
          run comp $moc_with_flags -g -wasi-system-api --hide-warnings --map -c $mangled -o $out/$base.wasm
        fi

        if [ "$SKIP_VALIDATE" != yes ]
        then
          run_if wasm valid wasm-validate --enable-multi-memory $out/$base.wasm
          run_if ref.wasm valid-ref wasm-validate --enable-multi-memory $out/$base.ref.wasm
        fi

        if [ -e $out/$base.wasm ]
        then
          # Check filecheck
          if [ "$SKIP_RUNNING" != yes ]
          then
            if grep -F -q CHECK $mangled
            then
              $ECHO -n " [FileCheck]"
              wasm2wat --enable-multi-memory --no-check $out/$base.wasm > $out/$base.wat
              cat $out/$base.wat | FileCheck $mangled > $out/$base.filecheck 2>&1
              diff_files="$diff_files $base.filecheck"
            fi
          fi
        fi

        # Run compiled program
        if [ "$SKIP_RUNNING" != yes ]
        then
          if [ $DTESTS = yes ]
          then
            if [ $HAVE_drun = yes ]; then
              run_if wasm drun-run $WRAP_drun $out/$base.wasm $mangled
            fi
            if [ $HAVE_ic_ref_run = yes ]; then
              run_if ref.wasm ic-ref-run $WRAP_ic_ref_run $out/$base.ref.wasm $mangled
            fi
          elif [ $PERF = yes ]
          then
            if [ $HAVE_drun = yes ]; then
              run_if wasm drun-run $WRAP_drun $out/$base.wasm $mangled 222> $out/$base.metrics
              if [ -e $out/$base.metrics -a -n "$PERF_OUT" ]
              then
                LANG=C perl -ne "print \"gas/$base;\$1\n\" if /^scheduler_(?:cycles|instructions)_consumed_per_round_sum (\\d+)\$/" $out/$base.metrics >> $PERF_OUT;
              fi
              run_if opt.wasm drun-run-opt $WRAP_drun $out/$base.opt.wasm $mangled
            fi
          else
            run_if wasm wasm-run wasmtime run $WASMTIME_OPTIONS $out/$base.wasm
          fi
        fi

        # collect size stats
        if [ "$PERF" = yes -a -e "$out/$base.wasm" ]
        then
           if [ -n "$PERF_OUT" ]
           then
             wasm-strip $out/$base.wasm -o $out/$base.wasm.strip
             echo "size/$base;$(stat --format=%s $out/$base.wasm.strip)" >> $PERF_OUT
           fi
        fi

        rm -f $mangled
      fi
    fi
  ;;
  "drun")
    if [ $DTESTS != yes ]
    then
      $ECHO ""
      echo "Running .drun files only make sense with $0 -d";
      continue
    fi

    # The file is a drun script, so a multi-canister project
    mkdir -p $out/$base

    for runner in ic-ref-run drun
    do
      if grep -q "# *SKIP $runner" $(basename $file)
      then
        continue
      fi

      if grep -q "# *INCREMENTAL-GC-ONLY" $(basename $file)
      then
        if [[ $EXTRA_MOC_ARGS != *"--incremental-gc"* ]]
        then 
          continue
        fi
      fi

      have_var_name="HAVE_${runner//-/_}"
      if [ ${!have_var_name} != yes ]
      then
        $ECHO "skipped (no $runner)";
        continue
      fi

      # collect all .mo files referenced from the file
      mo_files="$(grep -o '[^[:space:]]\+\.mo' $base.drun |sort -u)"

      for mo_file in $mo_files
      do
        mo_base=$(basename $mo_file .mo)
        if [ "$(dirname $mo_file)" != "$base" ];
        then
          $ECHO ""
          echo "$base.drun references $mo_file which is not in directory $base"
          exit 1
        fi
        moc_extra_flags="$(eval echo $(grep '//MOC-FLAG' $mo_file | cut -c11- | paste -sd' '))"
        flags_var_name="FLAGS_${runner//-/_}"
        run $mo_base.$runner.comp moc $EXTRA_MOC_ARGS ${!flags_var_name} $moc_extra_flags --hide-warnings -c $mo_file -o $out/$base/$mo_base.$runner.wasm
      done

      # mangle drun script
      LANG=C perl -npe "s,$base/([^\s]+)\.mo,$out/$base/\$1.$runner.wasm," < $base.drun > $out/$base/$base.$runner.drun

      # run wrapper
      wrap_var_name="WRAP_${runner//-/_}"
      run $runner ${!wrap_var_name} $out/$base/$base.$runner.drun
    done

  ;;
  "sh")
    # The file is a shell script, just run it
    $ECHO -n " [out]"
    ./$(basename $base.sh) > $out/$base.stdout 2> $out/$base.stderr
    normalize $out/$base.stdout
    normalize $out/$base.stderr
    diff_files="$diff_files $base.stdout $base.stderr"
  ;;
  "wat")
    # The file is a .wat file, so we are expected to test linking
    $ECHO -n " [mo-ld]"
    rm -f $out/$base.{base,lib,linked}.{wasm,wat,o}
    make --quiet $out/$base.{base,lib}.wasm
    mo-ld -b $out/$base.base.wasm -l $out/$base.lib.wasm -o $out/$base.linked.wasm > $out/$base.mo-ld 2>&1
    diff_files="$diff_files $base.mo-ld"

    if [ -e $out/$base.linked.wasm ]
    then
        run wasm2wat wasm2wat $out/$base.linked.wasm -o $out/$base.linked.wat
        diff_files="$diff_files $base.linked.wat"
    fi
  ;;
  "did")
    # The file is a .did file, so we are expected to test the idl
    # Typecheck
    $ECHO -n " [tc]"
    didc --check $base.did > $out/$base.tc 2>&1
    tc_succeeded=$?
    normalize $out/$base.tc
    diff_files="$diff_files $base.tc"

    if [ "$tc_succeeded" -eq 0 ];
    then
      $ECHO -n " [pp]"
      didc --pp $base.did > $out/$base.pp.did
      sed -i 's/import "/import "..\//g' $out/$base.pp.did
      didc --check $out/$base.pp.did > $out/$base.pp.tc 2>&1
      diff_files="$diff_files $base.pp.tc"

      run didc-js didc --js $base.did -o $out/$base.js
      normalize $out/$base.js
      diff_files="$diff_files $base.js"

      if [ -e $out/$base.js ]
      then
        export NODE_PATH=$NODE_PATH:$ESM
        run node node -r esm $out/$base.js
      fi
    fi
    ;;
  "cmp")
    # The file is a .cmp file, so we are expected to test compatiblity of the two
    # files in cmp
    # Compatibility check
    $ECHO -n " [cmp]"
    moc --stable-compatible $(<$base.cmp) > $out/$base.cmp 2>&1
    succeeded=$?
    if [ "$succeeded" -eq 0 ]
    then
     echo "TRUE" >> $out/$base.cmp
    else
     echo "FALSE" >> $out/$base.cmp
    fi
    diff_files="$diff_files $base.cmp"
  ;;
  *)
    echo "Unknown extentions $ext";
    exit 1
  esac
  $ECHO ""

  if [ $ACCEPT = yes ]
  then
    rm -f $ok/$base.*

    for outfile in $diff_files
    do
      if [ -s $out/$outfile ]
      then
        cp $out/$outfile $ok/$outfile.ok
      else
        rm -f $ok/$outfile.ok
      fi
    done
  else
    for diff_file in $diff_files
    do
      if [ -e $ok/$diff_file.ok -o -e $out/$diff_file ]
      then
        diff -a -u -N --label "$diff_file (expected)" $ok/$diff_file.ok --label "$diff_file (actual)" $out/$diff_file
        if [ $? != 0 ]; then failures+=("$file");fi
      fi
    done
  fi
  popd >/dev/null
done

if [ ${#failures[@]} -gt 0  ]
then
  echo "Some tests failed:"
  tr ' ' '\n' <<< "${failures[@]}" | uniq | xargs echo
  exit 1
else
  $ECHO "All tests passed."
fi
