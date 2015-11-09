Run as such::-

ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax  -syntax camlp4o   -linkpkg -o barchart.byte barchart.ml

js_of_ocaml barchart.byte  