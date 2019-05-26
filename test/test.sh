#!/bin/bash -ue
#
# Copyright 2012-2013 OCamlPro
#
# All rights reserved.This file is distributed under the terms of the
# GNU Lesser General Public License version 2.1 with linking
# exception.
#
# TypeRex is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# Lesser GNU General Public License for more details.
#

shopt -s nullglob

ROOT=$(git rev-parse --show-toplevel)
OCAMLFORMAT="$ROOT"/_build/dev/src/ocamlformat.exe
cd $ROOT/test

ACCEPT=
UPDATE=
GIT=
SHOW=
SHOWCMD=
HTML=

usegit() {
    printf "%-12s\t\e[34mgit %s\e[m\n" "" "$*";
    git "$@";
}

is_file_on_git() {
    [ $# -eq 1 ]; f=$1
    git ls-files $f --error-unmatch >/dev/null 2>&1
}

while [ $# -gt 0 ]; do
    case "$1" in
        --update|-u)
            UPDATE=1
            ;;
        --git-update)
            if ! git diff --exit-code -- . >/dev/null; then
                echo -e "\e[1mWarning:\e[m unstaged changes in test/"
                echo "You may want to do 'git checkout -- test/' or"\
                     "'git add -u -- test/' first."
                exit 1
            fi
            UPDATE=1
            GIT="usegit "
            HTML=1
            ;;
        --ocamlformat)
            if [ $# -le 1 ]; then echo "Error: $1 needs an argument"; exit 1; fi
            shift;
            OCAMLFORMAT=$1
            ;;
        --show)
            SHOW=1
            ;;
        --meld)
            SHOW=1
            SHOWCMD="meld"
            ;;
        --html)
            HTML=1
            ;;
	--accept)
	    ACCEPT=1
	    ;;
        *)
            cat <<EOF >/dev/stderr
Usage:
  -u --update          update the files according to the current results
  --git-update         update the files and state the changes in git
  --ocamlformat <prg>  use this ocamlformat exe
  --show               show a diff of changed results
  --meld               show progressions/regressions using meld
  --html               generate an html page showing the diff of failing tests
  --accept             accept the current formatting
EOF
            exit 1
    esac
    shift
done

TMP=$(mktemp -d /tmp/ocamlformat-test.XXXXX)
trap "rm -rf /tmp/ocamlformat-${TMP#/tmp/ocamlformat-}" EXIT
CWD=$(dirname $(pwd))

ocamlformat() {
    [ $# -eq 1 ]
    opts=$(cat $1.opts 2>/dev/null || true)
    tmpfile=$TMP/$(basename $1)
    OCAMLFORMAT=max-iters=2 bash -c "(\"$OCAMLFORMAT\" $opts \"$1\" || true) 2>&1" | sed "s#${CWD}#{CWD}#" > $tmpfile
}

reffile() {
    [ $# -eq 1 ]
    if [ -e "$1.ref" ]
    then echo "$1.ref"
    else echo "$1"
    fi
}

PASSING=("")
FAILING=("")
if [ -n "$GIT" ]; then
    PASSING+=($(git ls-files 'passing/*.ml' 'passing/*.ml[iylt]'))
    FAILING+=($(git ls-files 'failing/*.ml' 'failing/*.ml[iylt]'))
else
    PASSING+=(passing/*.ml passing/*.ml[iylt])
    FAILING+=(failing/*.ml failing/*.ml[iylt])
fi
CHANGES=()


for f in ${PASSING[@]}; do
    base=$(basename $f)
    name=${base%.*}
    printf "[RUNNING]              %s\n" $name
    ocamlformat $f
    printf "\033[1A\033[2K"
    if ! diff -q "$(reffile "$f")" $TMP/$base >/dev/null; then
        printf "\e[31m[FAILED]\e[m \e[41m\e[30m[REGRESSION]\e[m  %s\n" $name
        if [ -n "$ACCEPT" ]; then
	    cp $TMP/$base "$(reffile "$f")"
        elif [ -n "$UPDATE" ]; then
            mkdir -p failing
            $GIT mv -f $f* failing/
            f=failing/${f#passing/}
            mkdir -p failing-output
            cp $TMP/$base failing-output/
            if [ -n "$GIT" ]; then $GIT add failing-output/$base; fi
        fi
        CHANGES+=($f)
    fi
done

for f in ${FAILING[@]}; do
    base=$(basename $f)
    name=${base%.*}
    printf "[RUNNING]              %s\n" $name
    ocamlformat $f
    printf "\033[1A\033[2K"
    if diff -q $(reffile $f) $TMP/$base >/dev/null; then
        printf "\e[32m[PASSED]\e[m \e[42m\e[30m[PROGRESSION]\e[m %s\n" $name
        if [ -n "$UPDATE" ]; then
            $GIT mv -f $f* passing/
            $GIT rm -f failing-output/$base
        fi
    elif [ ! -e failing-output/$base ]; then
        printf "\e[33m[FAILED]\e[m \e[43m\e[30m[NEW]\e[m         %s\n" $name
        cp $TMP/$base failing-output/
        if [ -n "$GIT" ]; then $GIT add failing-output/$base; fi
    elif diff -q $TMP/$base failing-output/$base >/dev/null; then
        printf "\e[33m[FAILED] [BASELINE]\e[m    %s\n" $name
        if [ -n "$GIT" ] && ! is_file_on_git failing-output/$base; then
            $GIT add failing-output/$base; fi
    else
        refcount=$(diff -y --suppress-common-lines \
            $(reffile $f) failing-output/$base \
            |wc -l)
        curcount=$(diff -y --suppress-common-lines \
            $(reffile $f) $TMP/$base \
            |wc -l)
        progress=$((refcount - curcount))
        printf "\e[33m[FAILED]\e[m \e[%dm\e[30m[CHANGE: %+d]\e[m %s\n" \
            $(if [ $progress -gt 0 ]; then echo 42; \
              elif [ $progress -eq 0 ]; then echo 43; \
              else echo 41; fi) \
            $progress \
            $name
        if [ -n "$ACCEPT" ]; then
	    cp $TMP/$base failing-output/$base
        elif [ -n "$UPDATE" ]; then
            mkdir -p failing-output
            cp $TMP/$base failing-output/
            if [ -n "$GIT" ]; then $GIT add failing-output/$base; fi
        fi
        CHANGES+=($f)
    fi
done

if [ -n "$SHOW" ] && [ ${#CHANGES[@]} -gt 0 ]; then
    if [ -z "$SHOWCMD" ]; then
        for f in ${CHANGES[@]}; do
            echo
            printf "\e[1m=== Showing differences in %s ===\e[m\n" $f
            # Custom less buggy version of colordiff -y
            diff -W 130 -ty  $(reffile $f) $TMP/$(basename $f) \
                | awk '/^.{64}[^ ].*/ { printf "[31m%s[m\n",$0; next } 1' \
                || true
        done
    else
        echo
        echo "Meld view:"
        echo "[reference] [new result] [registered]"
        echo "You can update reference and registered status from meld"
        cmd=(meld)
        for f in ${CHANGES[@]}; do
            cur=failing-output/$(basename $f)
            if ! [ -e $cur ]; then cur=; fi
            cmd+=(--diff $(reffile $f) $TMP/$(basename $f) $cur)
        done
        ${cmd[*]}
    fi
elif [ -n "$SHOW" ]; then
    echo
    echo "No changes to show. To check the current failures use for example:"
    echo "  meld test/failing test/failing-output"
fi

diff2html() {
    f1=$1; shift
    f2=$1; shift
    [ $# -eq 0 ]

    echo "<div>"
    echo "<h2>Differences in $(basename $f1)</h2>"
    echo "<table>"
    echo "<tr><th><th>Expected<th>ocamlformat output</tr>"

    {
        line=0
        XIFS="$IFS"
        IFS=
        while read -r l1; do
            read -r l2 <&3 || true
            class="correct"
            if [ "$l1" != "$l2" ]; then
                class="different"
                l1=$(sed 's/ /·/g' <<<"$l1")
                l2=$(sed 's/ /·/g' <<<"$l2")
            fi
            echo -n '<tr>'
            echo -n '<td class="linenum">'$line'</td>'
            echo -n '<td class="'$class'"><pre>'"$l1"'</pre></td>'
            echo -n '<td class="'$class'"><pre>'"$l2"'</pre></td>'
            echo    '</tr>'
            : $((line++))
        done
        while read -r l2 <&3; do
            l2=$(sed 's/ /·/g' <<<"$l2")
            echo -n '<tr>'
            echo -n '<td class="linenum">'$line'</td>'
            echo -n '<td class="different"><pre></pre></td>'
            echo -n '<td class="different"><pre>'"$l2"'</pre></td>'
            echo    '</tr>'
            : $((line++))
        done
        IFS="$XIFS"
    } <$f1 3<$f2

    echo "</table>"
    echo "</div>"
}

if [ -n "$HTML" ]; then
    VERSION=$($OCAMLFORMAT --version | awk '{ print $NF; exit }')
    if COMMITS_SINCE=$(git log --oneline $VERSION.. 2>/dev/null); then
        VERSION="$VERSION+$((1+$(wc -l <<<"$COMMITS_SINCE")))"
    fi
    VERSION_STRING="$VERSION ($(date +%F))"
    echo
    echo -n "Generating summary of failures test/failing.html..."
    cat <<EOF > failing.html
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
 "http://www.w3.org/TR/REC-html40/loose.dtd">
<html>
<head>
    <title>Failing tests, ocamlformat version $VERSION_STRING</title>
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <style>
      TABLE { border-collapse: collapse; border-spacing: 0px; margin: auto; }
      H2 { margin-top: 5ex; text-align: center; padding: 1ex;
           background-color: orange; }
      TR,TD,PRE { padding: 0; margin: 0; }
      PRE { font-family: mono; }
      TD.linenum { vertical-align: top; font-family: mono;
                   padding-right:2px; text-align: right }
      TD.correct { background-color: #EEE; border: 1px solid white; }
      TD.different { background-color: orange; border: 1px solid white; }
    </style>
</head>
<body>
<h1>Failing tests, ocamlformat version $VERSION_STRING</h1>
EOF
    complete_success="1"
    for f in $(git ls-files 'failing/*.ml'); do
        complete_success=
        diff2html "$(reffile $f)" "failing-output/${f#failing/}" \
            >>failing.html
        echo -n "."
    done
    if [ -n "$complete_success" ]; then
        echo "<p>All tests pass: no currently known bugs.</p>" >>failing.html
    fi
    cat <<EOF >>failing.html
</body>
</html>
EOF

    echo " done"
    if [ -n "$GIT" ]; then $GIT add failing.html; fi
fi

exit ${#CHANGES[@]}
