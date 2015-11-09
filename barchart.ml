(*
	author: Vinaya Mandke
	creating barchart.byte: 
		ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax  -syntax camlp4o   -linkpkg -o barchart.byte barchart.ml  
	creating barchart.js
		 js_of_ocaml barchart.byte 		
*)
module Html = Dom_html
let (>>=) = Lwt.bind

let getData () =
	let arrayData = [1;33;23;78;79;67;87;35;28;90;207;76;45;36;88;87;45;2;89;77;98;56;46;89]
		in
	Lwt.return (List.map float_of_int arrayData)

let create_canvas w h =
	let d = Html.window##document in
    	let c = Html.createCanvas d in
    		c##width <- w;
			c##height <- h;
			c

let getAvg = fun data -> 
	List.fold_left ( +.) 0. data /. float(List.length data)

let square = fun x -> x *. x

let min = fun data ->
	List.fold_left (fun a b ->  if a < b then a else b) (List.hd data) data

let max = fun data ->
	List.fold_left (fun a b ->  if a > b then a else b) (List.hd data) data

let getSD = fun data ->
	let avg = getAvg data in
		let sqrList = List.map (fun x -> square (x -. avg)) data in
			sqrt (getAvg sqrList)

let away_sd = fun sd mean value ->
	if mean > value then
		let rec subSD = fun i -> 
			if (mean -. float(i) *. sd) < value then i else subSD (i + 1) in
				subSD 1
	else
		let rec addSD = fun i ->
			if (mean +. float(i) *. sd) > value then i else addSD (i + 1) in
				addSD 1

let barPlot = fun canvas2Dcontext canvasHeight xStart y ->
	Firebug.console##log(Js.string "plot a bar");
	canvas2Dcontext##fillRect (xStart +. 10., canvasHeight -. y, 100., y);
	xStart +. 120.

let plot = fun canvas data gridSlots startFromY ->
	(*Start from denotes where we start from on Y axis*)
	Firebug.console##log(Js.string "plotting");
	let c = canvas##getContext (Html._2d_) in
	let w = float canvas##width in
  	let h = float canvas##height in
		c##clearRect (0., 0., w, h);
		c##fillStyle <- Js.string "red";
  		let rec plotter = fun consumedWidth i ->
  			if consumedWidth < w then 
  				let consumedWidth = barPlot c h consumedWidth ((List.nth data i) -. startFromY) in
  					plotter consumedWidth (i + 1)
  			else 
  				() 
  			in
  				plotter 0. 0;

    ()


let data = getData ()

let start _ =
  Lwt.ignore_result
   (data >>= fun (arrayData) ->
      Firebug.console##log(Js.string "start");
        (* create a canvas *)
        let doc = Html.document in
     		let page = doc##documentElement in
     			page##style##overflow <- Js.string "hidden";
			    page##style##height <- Js.string "100%";
			    doc##body##style##overflow <- Js.string "hidden";
			    doc##body##style##margin <- Js.string "0px";
			    doc##body##style##height <- Js.string "100%";
			    let w = page##clientWidth in
			     	let h = (page##clientHeight / 2) in
			     		let canvas = create_canvas w h in
			    			 Dom.appendChild doc##body canvas;
			    			 let standard_deviation = getSD arrayData in 
							 let avg = getAvg arrayData in
							 let maxVal = max arrayData in
							 let minVal = min arrayData in
								let sdAwayMin = away_sd standard_deviation avg minVal in
								let sdAwayMax = away_sd standard_deviation avg maxVal in
									(*
										We need to plot between sdAwayMin <---> sdAwayMax on y axis
										We can fit our data exactly in sdAwayMin + sdAwayMax gridSlots
									*)
									let gridSlots = sdAwayMin + sdAwayMax in
									let startFromY = (avg -. (standard_deviation *. float(sdAwayMin))) in
										plot canvas arrayData gridSlots startFromY;
      Lwt.return () (* Lwt.return creates a thread which has aldready terminated*)
   );
   Js._false

let startHandler _ =
  try
    ignore (Html.createCanvas (Html.window##document));
    start ()
  with Html.Canvas_not_available ->
    Js._false

let _ =

(*
js_of_ocaml  Dom.handler
Create an event handler that invokes the provided function. 
If the handler returns false, the default action is prevented.
*)
Html.window##onload <- Html.handler startHandler