#! /bin/sh
#(test -f force/render) &&
(rm -f force/render; OCAMLRUNPARAM=b=1 ./trurl_render_log ../public_html/)
