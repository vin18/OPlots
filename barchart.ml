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
	let arrayData = [1;33;23;78;79;67;87;35;28;90;207;76;45;36;88;87;45;10;89;77;98;56;46;89;79;67;87;35;28;90;207;76;45;36;88;87;45;79;67;87;35;28;90;207;76;45;36;88;87;45;79;67;87;35;28;90;207;76;45;36;88;87;45;79;67;87;35;28;90;207;76;45;36;88;87;45;79;67;87;35;28;90;207;76;45;36;88;87;45;79;67;87;35;28;90;207;76;45;36;88;87;45;79;67;87;35;28;90;207;76;45;36;88;87;45;79;67;87;35;28;90;207;76;45;36;88;87;45]
	(* let arrayData = [145;147;178;190;153;145;166;177;154;153;142;178;179;167;187;165;178;190;207;176;145;166;188;187;178;179;167;187;195;198;190;207;176;195;186] *)
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

let barPlotWidth = 100.
let barPlotMargin = 10.
let dragWidth = ref 0.
let need_redraw = ref false
let redraw_funct = ref (fun () -> ())

let barPlot = fun canvas2Dcontext canvasHeight xStart y dataValue->
	Firebug.console##log(Js.string "plot a bar");
	canvas2Dcontext##fillStyle <- Js.string "red";
	canvas2Dcontext##fillRect (xStart +. barPlotMargin, canvasHeight -. y, barPlotWidth, y);
	canvas2Dcontext##fillStyle <- Js.string "black";
	canvas2Dcontext##fillText ( Js.string (Printf.sprintf "%.2f" (dataValue)), xStart +. barPlotMargin, canvasHeight -. y -. 10.);

	xStart +. barPlotWidth +. barPlotMargin

let getStartIndex = fun dragWidth ->
	int_of_float(dragWidth /. (barPlotWidth +. barPlotMargin))

let getMaxDragWidth = fun len canvasWidth ->
	(float (len) *. (barPlotWidth +. barPlotMargin)) -. canvasWidth


let perform_redraw () =
  need_redraw := false;
  !redraw_funct ()

let schedule_redraw now =
  if not !need_redraw then begin
    need_redraw := true;
    Html._requestAnimationFrame
      (Js.wrap_callback (fun () ->
        if !need_redraw then perform_redraw ()))
  end


let plot = fun canvas data gridSlots sd startFromY dragWidth->
	(*Start from denotes where we start from on Y axis*)
	Firebug.console##log(Js.string "plotting");
	let c = canvas##getContext (Html._2d_) in
	let w = float canvas##width in
  	let h = float canvas##height in
		c##clearRect (0., 0., w, h);
  		let rec plotter = fun consumedWidth i ->
  			if consumedWidth < w then 
  				(* We have gridSlots each of height sd *)
  				(* So a data with value h will occupy (total Height * dataValue) / (gridSlots * sd) *)
  				let dataHeight = (h *. ((List.nth data i) -. startFromY) ) /. (float(gridSlots) *. sd) in
	  				let consumedWidth = barPlot c h consumedWidth dataHeight (List.nth data i) in
	  					plotter consumedWidth (i + 1)
  			else 
  				() 
  			in
  				let startIndex = getStartIndex dragWidth in
	  				plotter (0. -. dragWidth +. (float(startIndex) *. (barPlotWidth +. barPlotMargin))) startIndex;
    ()

let markGridY = fun yGridCanvas gridSlots standard_deviation startFromY->
	Firebug.console##log(Js.string "marking grid");
	let c = yGridCanvas##getContext (Html._2d_) in
	let w = float yGridCanvas##width in
  	let h = float yGridCanvas##height in
  		c##clearRect (0., 0., w, h);
  		c##font <- Js.string "12pt Verdana";
  		let rec markSlots = fun i ->
  			if i <= (gridSlots) then begin
	  			c##fillStyle <- Js.string "darkkhaki";
	  			c##fillRect ( (w *. 0.5), ( h -. (float(gridSlots - i) *. (h /. float(gridSlots))) -. 1.), (w *. 0.8), 1.);
	  			c##fillStyle <- Js.string "black";
	  			c##fillText ( Js.string (Printf.sprintf "%.2f" (startFromY +. float(gridSlots - i) *. standard_deviation)), (w *. 0.1), ( h -. (float(gridSlots - i) *. (h /. float(gridSlots))) -. 1.));
	  			markSlots (i+1)
  			end else 
	  			() 
	  		in
	  			markSlots 0;
	()

let markGridLines = fun gridLines gridSlots standard_deviation ->
	Firebug.console##log(Js.string "marking gridLines");
	let c = gridLines##getContext (Html._2d_) in
	let w = float gridLines##width in
  	let h = float gridLines##height in
  		c##clearRect (0., 0., w, h);
  		let rec markSlotsX = fun i ->
  			if i <= (gridSlots) then begin
	  			c##fillStyle <- Js.string "darkkhaki";
	  			c##fillRect ( (0.), ( h -. (float(gridSlots - i) *. (h /. float(gridSlots))) -. 1.), w, 1.);
	  			markSlotsX (i+1)
  			end else 
	  			() 
	  		in
	  			markSlotsX 0;
  		(* Mark the Y lines*)
  		let rec markSlotsY = fun i ->
  			if float(i) *. 20. <= (w) then begin
	  			c##fillStyle <- Js.string "darkkhaki";
	  			c##fillRect ( float(i) *. 20., 0., 1., h);
	  			markSlotsY (i+1)
  			end else 
	  			() 
	  		in
	  			markSlotsY 0;
	()


let data = getData ()

let handle_drag = fun element move (* stop click *) ->
	let fuzz = 4 in
		element##onmousedown <- Html.handler (fun ev ->
       		let x0 = ev##clientX in
       			let started = ref false in
       			let c1 = Html.addEventListener Html.document Html.Event.mousemove
       				(Html.handler  (fun ev ->
                 		let x = ev##clientX in
                 			if not !started && (abs (x - x0) > fuzz) then begin
                 				started := true;
                 				element##style##cursor <- Js.string "move";
                 			end;
                 			if !started then move x0 x;
	                 		Html.stopPropagation ev;
	                 		Js._true
                 	)) Js._true (* mousemove handler *)
       			in
	       			let c2 = ref Js.null in
	      				c2 := Js.some (Html.addEventListener Html.document Html.Event.mouseup
	      					(Html.handler  (fun ev ->
	      						Html.removeEventListener c1;
	      						if !started then begin
	      							element##style##cursor <- Js.string "";
	      							started := false;
	      							need_redraw := false;
	      							Js._true (* stop ev##clientX; *)
	      						end
	      						else 
	      							(* click ev##clientX; *)
	      							Js._true
	      					))(* mouseup handler *)
	      				Js._true);
		Js._true)

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
			    dragWidth := 0.;
			    redraw_funct := (fun () -> ());
			    let w = page##clientWidth in
			     	let h = (page##clientHeight / 2) in
			     	let holder = Html.createDiv Html.document in
			     		holder##style##display <- Js.string "flex";
			     		let canvas = create_canvas w h in
			     		let gridLines = create_canvas w h in
			     			 canvas##style##display <- Js.string "block";
			    			 Dom.appendChild doc##body holder;
			    			 let yGridcanvas = create_canvas (page##clientWidth / 10) (page##clientHeight / 2) in
								Dom.appendChild holder yGridcanvas;
							 let plotholder = Html.createDiv Html.document in
							 plotholder##style##position <- Js.string "relative";
							 canvas##style##position <- Js.string "absolute";
							 gridLines##style##position <- Js.string "absolute";
			    			 Dom.appendChild holder plotholder;
			    			 Dom.appendChild plotholder gridLines;
			    			 Dom.appendChild plotholder canvas;
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

									(*debug logs*)
									Firebug.console##log(Js.string "standard_deviation");
									Firebug.console##log(Js.string (string_of_float standard_deviation) );
									Firebug.console##log(Js.string "avg");
									Firebug.console##log(Js.string (string_of_float avg) );
									Firebug.console##log(Js.string "maxVal");
									Firebug.console##log(Js.string (string_of_float maxVal) );
									Firebug.console##log(Js.string "minVal");
									Firebug.console##log(Js.string (string_of_float minVal) );
									Firebug.console##log(Js.string "sdAwayMin");
									Firebug.console##log(Js.string (string_of_int sdAwayMin) );
									Firebug.console##log(Js.string "sdAwayMax");
									Firebug.console##log(Js.string (string_of_int sdAwayMax) );



									let gridSlots = sdAwayMin + sdAwayMax in
									let startFromY = (avg -. (standard_deviation *. float(sdAwayMin))) in
										redraw_funct := (fun () ->
											need_redraw := false;
											let w = page##clientWidth in
											let h = page##clientHeight / 2 in
												canvas##width <- w;
												canvas##height <- h;
												plot canvas arrayData gridSlots standard_deviation startFromY !dragWidth;
										);
										perform_redraw ();

										markGridY yGridcanvas gridSlots standard_deviation startFromY;
										markGridLines gridLines gridSlots standard_deviation;

							handle_drag canvas (fun x0 x1 -> 
								dragWidth := !dragWidth +. float(x0 - x1);
								if !dragWidth < 0. then dragWidth := 0.;
								let maxDragWidth = getMaxDragWidth (List.length arrayData) (float w) in
									if !dragWidth > maxDragWidth then dragWidth := maxDragWidth;
								schedule_redraw true
							);

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
