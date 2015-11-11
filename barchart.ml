(*
	author: Vinaya Mandke
	creating barchart.byte: 
		ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax  -syntax camlp4o   -linkpkg -o barchart.byte barchart.ml  
	creating barchart.js
		 js_of_ocaml barchart.byte 		
*)
module Html = Dom_html
let (>>=) = Lwt.bind
type dataset = { id : string; data : float list; }


let rec transpose list = match list with
	| []             -> []
	| []   :: xss    -> transpose xss
	| (x::xs) :: xss ->
	    (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)   


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

let startPlotting = fun dataGetter ->
  Lwt.ignore_result
   (dataGetter >>= fun (arrayData) ->
      Firebug.console##log(Js.string "start");
        (* create a canvas *)
        let doc = Html.document in
     		let page = doc##documentElement in
			    dragWidth := 0.;
			    redraw_funct := (fun () -> ());
			    let w = page##clientWidth in
			     	let h = (page##clientHeight / 2) in
			     	let holder = Html.createDiv Html.document in
			     		holder##style##display <- Js.string "flex";
			     		holder##setAttribute (Js.string "id", Js.string "mainHolder");
			     		let canvas = create_canvas w h in
			     		let gridLines = create_canvas w h in
			     			 canvas##style##display <- Js.string "block";
			     			 gridLines##style##display <- Js.string "block";
			    			 Dom.appendChild doc##body holder;
			    			 let yGridcanvas = create_canvas (page##clientWidth / 10) (page##clientHeight / 2) in
								yGridcanvas##style##display <- Js.string "block";
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

										Html.window##onresize <- Html.handler 
										 	(fun _ ->
										 			let w = page##clientWidth in
										 				let h = page##clientHeight / 2 in

										 					(*reset holder height*)
										 					holder##style##height <- Js.string (string_of_int(h) ^ "px");

															(*redraw scale*)
															yGridcanvas##width <- (w / 10);
															yGridcanvas##height <- h;
															markGridY yGridcanvas gridSlots standard_deviation startFromY;
										 					(*redraw plot*)
										 					schedule_redraw true;

										 					(*redraw gridlines*)
															gridLines##width <- w;
															gridLines##height <- h;
															markGridLines gridLines gridSlots standard_deviation;
										 	Js._true);

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

let displayDatasetHeader dataLoader =
	Lwt.ignore_result
   		(dataLoader  >>= fun (loadedData) ->
        let doc = Html.document in
     		let page = doc##documentElement in
     			page##style##overflow <- Js.string "hidden";
			    page##style##height <- Js.string "100%";
			    doc##body##style##overflow <- Js.string "hidden";
			    doc##body##style##margin <- Js.string "0px";
			    doc##body##style##height <- Js.string "100%";

			    let fileDragArea = Js.Opt.get (doc##getElementById(Js.string "fileDragArea"))
			     	(fun () -> assert false) in 
			     		Dom.removeChild doc##body fileDragArea;

			    let header = Html.createDiv Html.document in
			     	header##style##display <- Js.string "flex";
			     	header##style##marginBottom <- Js.string "20px";
			     	header##style##marginTop <- Js.string "20px";
			     	header##style##overflowY <- Js.string "scroll";
			     	Dom.appendChild doc##body header;
			     	
			     	let addDataSetheader = (fun data -> 
			     		let dataButton = Html.createDiv Html.document in
			     			Dom.appendChild header dataButton;
			     			dataButton##style##backgroundColor <- Js.string "bisque";
			     			dataButton##style##marginLeft <- Js.string "20px";
			     			dataButton##style##marginRight <- Js.string "20px";
			     			dataButton##style##padding <- Js.string "10px";
			     			dataButton##style##borderRadius <- Js.string "10px";
			     			dataButton##style##borderStyle <- Js.string "groove";

			     			let id = data.id in
			     				let txt = doc##createTextNode (Js.string id) in
			     					Dom.appendChild dataButton txt;
			     				dataButton##onclick <-  Html.handler (fun ev ->

			     					dataButton##style##borderStyle <- Js.string "ridge";
			     					(*remove holder*)
			     					let holder = Js.Opt.get (doc##getElementById(Js.string "mainHolder"))
			     						(fun () -> assert false) in 
			     							Dom.removeChild doc##body holder;

			     					let heldData = data.data in 
			     						let dataGetter = (
			     							Lwt.return (heldData)
			     						)
			     						 in
			     							startPlotting dataGetter
			     				);
			     			())
			     		in
			     			List.iter addDataSetheader loadedData;
			     	let firstDataSet = (List.nth loadedData 0) in
			     		let heldData = firstDataSet.data in
			     			let dataGetter = (
			     				Lwt.return (heldData)
			     			) 
				     		in
				     			ignore (startPlotting dataGetter);

   		Lwt.return ()
   	);
   Js._false



let getDataFromFile = fun readFile ->
	Lwt.ignore_result
		(readFile  >>= fun (textData) ->
			
			let ocamlTextString = Js.to_string textData in
				Firebug.console##log(Js.string ocamlTextString);

				let listLines = Regexp.split (Regexp.regexp "\n") ocamlTextString in
					Firebug.console##log(Js.array (Array.of_list listLines) );
					let splitValues = ( fun line -> 
						Firebug.console##log(Js.string line);
						let l = Regexp.split (Regexp.regexp ",")  line in
							Firebug.console##log(Js.array (Array.of_list l) );
							l
					) in
						let valuesLLT = List.map splitValues listLines in
							let valuesLL = transpose valuesLLT in 
								let createDataSet = ( fun l ->
									match l with 
										| [] -> {id=""; data=[];}
										| h::rem -> {id=h; data=(List.map float_of_string rem);}
								) in
									let dataSet = List.map createDataSet valuesLL in
										let dataLoader = (
											Lwt.return(dataSet)
										) in
											ignore (displayDatasetHeader dataLoader);

			Lwt.return ()
		)


let handleFileDrag () = (
	let doc = Html.document in
     	let fileDragArea = Html.createDiv Html.document in
     		Dom.appendChild doc##body fileDragArea;
     		fileDragArea##style##height <- Js.string "100px";
     		fileDragArea##style##width <- Js.string "100px";
     		fileDragArea##style##borderStyle <- Js.string "dotted";
     		fileDragArea##setAttribute (Js.string "id", Js.string "fileDragArea");

     			fileDragArea##ondrop <- Html.handler (fun ev ->
     				Firebug.console##log(Js.string "A file was dropped");
     				let files = ev##dataTransfer##files in 
     					(* only take the first file*)
     					Firebug.console##log(Js.string (string_of_int files##length));
     					Js.Opt.iter (files##item(0)) (fun file -> 
     							let getFileText = (
     								File.readAsText file
     							) in
     								getDataFromFile getFileText;
     							()	
     						);
     				Js._false;
			    );
     			fileDragArea##ondragover <- Html.handler (fun _ ->
     				Firebug.console##log(Js.string "Drag over event");
     				Js._false;
			    );
	()
)


let startHandler _ =
  try
    ignore (Html.createCanvas (Html.window##document));
    (* startPlotting () *)
    (* displayDatasetHeader () *)
    handleFileDrag ();
    Js._false
  with Html.Canvas_not_available ->
    Js._false

let _ =

(*
js_of_ocaml  Dom.handler
Create an event handler that invokes the provided function. 
If the handler returns false, the default action is prevented.
*)
Html.window##onload <- Html.handler startHandler
