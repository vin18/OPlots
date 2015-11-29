
Create ocaml bytecode:-
```
ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax  -syntax camlp4o -linkpkg -o barchart.byte barchart.ml
```
Create Js from the byte file using Js_of_ocaml
```
js_of_ocaml barchart.byte  
```
