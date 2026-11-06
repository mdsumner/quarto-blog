#!/usr/bin/env bash
## quarto-serve.sh: preview a Quarto project in the RStudio Viewer without the flashing.
##
## `quarto preview` injects a live-reload script that holds a websocket open; the RStudio
## proxy drops it, and the script reloads the page on every close, so it loops (flashing).
## Instead: render, serve the output folder as plain static files, and re-render when
## sources change. No live reload: press refresh in the Viewer after a change.
##
## usage:  ~/Git/quarto-blog/quarto-serve.sh [project-dir] [port]     (defaults: . 4321)
##   then in R:  rstudioapi::viewer("http://localhost:4321/")
## Ctrl-C (or kill) stops the watcher and the server.
## RENDER=0 to skip the initial render (serve what's already in the output folder).
## After that, only changed .qmd/.md/.ipynb files are re-rendered; FULL=1 re-renders everything.

dir=${1:-.}
port=${2:-4321}
cd "$dir" || exit 1
[ -f _quarto.yml ] || [ -f _quarto.yaml ] || { echo "no _quarto.yml in $(pwd)"; exit 1; }
if (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null; then
  echo "port $port is in use (an old 'quarto preview'?): pick another port or stop it"; exit 1
fi

## output folder from the project config (_site for websites, _book for books)
out=$(quarto inspect 2>/dev/null | python3 -c \
  'import json,sys; print(json.load(sys.stdin).get("config",{}).get("project",{}).get("output-dir") or "_site")' \
  2>/dev/null)
out=${out:-_site}

## source files modified since the marker file (ignores output, freeze and hidden dirs)
changed_since() {
  find . \( -path "./$out" -o -path ./_freeze -o -path './.*' -o -name site_libs -o -name renv -o -name node_modules \) -prune -o \
    -type f -newer "$1" \( -name '*.qmd' -o -name '*.md' -o -name '*.ipynb' -o -name '*.yml' -o -name '*.yaml' \
    -o -name '*.R' -o -name '*.py' -o -name '*.js' -o -name '*.ts' -o -name '*.css' -o -name '*.scss' \
    -o -name '*.bib' -o -name '*.csl' -o -name '*.lua' \) -print
}

if [ "${RENDER:-1}" != 0 ]; then quarto render || echo "RENDER FAILED (serving what there is)"; fi
mkdir -p "$out"
python3 -m http.server "$port" --bind 127.0.0.1 --directory "$out" > /dev/null 2>&1 &
server=$!
trap 'kill $server 2>/dev/null' EXIT
trap 'exit 0' INT TERM
sleep 1
kill -0 $server 2>/dev/null || { echo "server failed to start"; exit 1; }
echo "serving $(pwd)/$out at http://localhost:$port/"
echo "  in R: rstudioapi::viewer(\"http://localhost:$port/\")"

## Only changed documents (.qmd/.md/.ipynb) are re-rendered, one by one. Any other changed
## file (yml, css, R, ...) can affect the whole site, so that triggers a full render.
## Listing pages are not refreshed by single-file renders; FULL=1 always renders everything.
marker=$(mktemp); next=$(mktemp)
trap 'kill $server 2>/dev/null; rm -f "$marker" "$next"' EXIT
while true; do
  sleep 2
  touch "$next"
  files=$(changed_since "$marker")
  mv "$next" "$marker"; next=$(mktemp)
  [ -z "$files" ] && continue
  if [ "${FULL:-0}" = 1 ] || echo "$files" | grep -qvE '\.(qmd|md|ipynb)$'; then
    echo "$(date +%T) change detected, full render"
    if quarto render; then echo "$(date +%T) done, refresh the Viewer"; else echo "$(date +%T) RENDER FAILED"; fi
  else
    for f in $files; do
      echo "$(date +%T) $f changed, rendering it"
      if quarto render "$f"; then echo "$(date +%T) done, refresh the Viewer"; else echo "$(date +%T) RENDER FAILED: $f"; fi
    done
  fi
done
