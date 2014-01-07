{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
  open Html5.D
  open Lwt
  open Popcorn (* media library and popcorn.js API *)
}}

open Server

module Subtitle_app =
	Eliom_registration.App (
	struct
		let application_name = "subtitle"
	end)

let main_service =
	Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

{client{
(* hack function to convert video into DOM element *)
let of_video (e: 'a Html5_types.video elt) : videoElement Js.t =
	Js.Unsafe.coerce (To_dom.of_element e)

let init_client _ =
	(* DOM elements including constructors *)
	let video_elm = of_video %video_player in
	let canvas_elm = To_dom.of_canvas %canvas_graphics in
	let graphics_ctrl_elm = To_dom.of_div %graphics_controller in
	let table_elm = To_dom.of_table %subtitle_table in
	let start_ph_elm = To_dom.of_input %start_time_ph in
	let end_ph_elm = To_dom.of_input %end_time_ph in
	let subtitle_text_elm = To_dom.of_div %subtitle_text in
	let x_coord_elm = To_dom.of_input %x_coord_ph in
	let y_coord_elm = To_dom.of_input %y_coord_ph in
	let textbox = To_dom.of_textarea %textarea in
	let button_add = To_dom.of_button %button_add in
	let button_clear = To_dom.of_button %button_clear in
	let button_play = To_dom.of_button %button_play in
	let button_pause = To_dom.of_button %button_pause in
	let button_big = To_dom.of_button %button_big in
	let button_medium = To_dom.of_button %button_medium in
	let button_small = To_dom.of_button %button_small in
	let button_step_forward = To_dom.of_button %button_step_forward in
	let button_step_backward = To_dom.of_button %button_step_backward in
	let button_toggle_graph = To_dom.of_button %button_toggle_graph in
	(* create a empty object of popcorn *)
	let pop = popcorn(Js.string "#myvideo") in
	(* make a reference of it - changed to variable *)
	let pop_ref = ref pop in
	let end_of_sub = ref (Js.string "0.00") in

	(* graphics control slider constructor *)
	let build_graphics_slider parent min max text =
		let slider = jsnew Goog.Ui.slider(Js.null) in
		slider##setMinimum(min);
		slider##setMaximum(max);
		slider##setValue(0.0);
		slider##setMoveToPointEnabled(Js._true);
		let doc = Dom_html.document in
		let label = Dom_html.createLabel doc in
		label##innerHTML <- Js.string text;
		Dom.appendChild parent label;
		slider##render(Js.some parent);
		slider
	in

	(* starting and ending time slider constructor *)
	let build_time_slider elm duration =
		let slider = jsnew Goog.Ui.slider(Js.null) in
		let duration_flt = float_of_string (Js.to_string duration) in
		slider##setMinimum(0.);
		slider##setMaximum(duration_flt);
		slider##setValue(0.0);
		slider##setMoveToPointEnabled(Js._true);
		slider##setStep(Js.some 0.1);
		slider##setUnitIncrement(0.05);
		slider##renderBefore(elm);
		slider
	in

	(* initialization *)
	start_ph_elm##value <- Js.string "0.00";
	end_ph_elm##value <- Js.string "0.00";
	textbox##value <- Js.string "";
	subtitle_text_elm##style##width <-
		Js.string((string_of_int 640) ^ "px");
	subtitle_text_elm##style##top <-
		Js.string((string_of_int 240) ^ "px");
	let duration = (!pop_ref)##duration() in
	let start_slider = build_time_slider start_ph_elm duration in
	let end_slider = build_time_slider end_ph_elm duration in
	let graphics_size_slider =
		build_graphics_slider graphics_ctrl_elm 10. 20. "Ball size" in
	let pSmall = jsnew Goog.Ui.hsvPalette
		(Js.null, Js.null, Js.some (Js.string "goog-hsv-palette-sm")) in
	pSmall##render(Js.some graphics_ctrl_elm);

	(* canvas constructor and initialization *)
	let init_canvas () =
		let x = ref 100. in
		let y = ref 50. in
		let dx = ref 5. in
		let dy = ref 5. in
		(* default hidden *)
		canvas_elm##style##display <- Js.string "none";
		canvas_elm##style##position <- Js.string "absolute";
		canvas_elm##style##top <- Js.string "480px";
		canvas_elm##style##left <- Js.string "175px";
		let ctx = canvas_elm##getContext(Dom_html._2d_) in
		let redraw () =
		    ctx##clearRect (0., 0., 200., 100.);
			let size = Js.to_float graphics_size_slider##getValue() in
		    if !x < (0. +. size) || !x > (200. -. size) then dx := -.(!dx);
		    if !y < (0. +. size) || !y > (100. -. size) then dy := -.(!dy);
			x := !x +. !dx;
			y := !y +. !dy;
			ctx##beginPath();
		    ctx##fillStyle <- pSmall##getColor();
		    ctx##arc (!x, !y, size, 0., 3.142 *. 2., Js.bool true);
			ctx##closePath();
			ctx##fill();
		in
		let _ = Dom_html.window##setInterval(Js.wrap_callback redraw, 10.) in
		()
	in
	init_canvas ();

	(* common functions *)
	let js_string_of_js_float js_float =
		Js.string (string_of_float (Js.to_float js_float))
	in

	let fix_float_string_precision js_string precision =
		let in_flt = float_of_string (Js.to_string js_string) in
		let in_num = Js.number_of_float in_flt in
		let out_flt = in_num##toFixed(precision) in
		out_flt
	in

	let comp_float_string_equal str1 str2 =
		let num1 = float_of_string(Js.to_string str1) in
		let num2 = float_of_string(Js.to_string str2) in
		num1 = num2
	in

	let comp_float_string_lessequal str1 str2 =
		let num1 = float_of_string(Js.to_string str1) in
		let num2 = float_of_string(Js.to_string str2) in
		num1 <= num2
	in

	(*video controller functions*)
	let play_video _ =
		video_elm##play()
	in

	let pause_video _ =
		video_elm##pause()
	in

	let set_video_width w =
		video_elm##width <- w
	in

	let step_video s =
		let new_time = (Js.to_float video_elm##currentTime) +. s in
		video_elm##currentTime <- (Js.float new_time)
	in

	let switch_visibility elt =
		if (elt##style##display <> Js.string "none")
		then elt##style##display <- Js.string "none"
		else elt##style##display <- Js.string ""
	in

	(* subtitle edition functions *)
	let is_time_valid start_time end_time prev_end next_start =
		try
			let start_time_flt = float_of_string (Js.to_string start_time) in
			let end_time_flt = float_of_string (Js.to_string end_time) in
			let prev_end_flt = float_of_string (Js.to_string prev_end) in
			let next_start_flt = float_of_string (Js.to_string next_start) in
			let duration_flt = float_of_string
				(Js.to_string ((!pop_ref)##duration())) in
			let curr_time_flt = float_of_string
				(Js.to_string ((!pop_ref)##currentTime_get())) in
			(* all the error cases *)
			Firebug.console##log_2(Js.string "[start time]", start_time);
			Firebug.console##log_2(Js.string "[end time]", end_time);
			Firebug.console##log_2(Js.string "[prev end]", prev_end);
			Firebug.console##log_2(Js.string "[next start]", next_start);
			if start_time_flt < 0.0 || start_time_flt > duration_flt then
			begin
				Dom_html.window##alert
				(Js.string "Start time beyond video limits");
				false
			end
			else if end_time_flt < 0.0 || end_time_flt > duration_flt then
			begin
				Dom_html.window##alert
				(Js.string "End time beyond video limits");
				false
			end
			else if end_time_flt <= start_time_flt then
			begin
				Dom_html.window##alert
				(Js.string "End time must be greater than start time");
				false
			end
			(* prevent users from editing subtitle in another screen *)
			else if start_time_flt > curr_time_flt ||
				end_time_flt < curr_time_flt then
			begin
				Dom_html.window##alert
				(Js.string "Subtitle out of current screen disallowed");
				false
			end
			else if start_time_flt < prev_end_flt then
			begin
				Dom_html.window##alert
				(Js.string "Start time beyond previous subtitle limits");
				false
			end
			else if end_time_flt > next_start_flt then
			begin
				Dom_html.window##alert
				(Js.string "End time beyond next subtitle limits");
				false
			end
			else
				true
		(* exception thrown *)
		with _ ->
			Dom_html.window##alert
			(Js.string "Invalid time format");
			false
	in

	let is_coord_valid x_coord y_coord =
		try
			let x_coord_int = int_of_string (Js.to_string x_coord) in
			let y_coord_int = int_of_string (Js.to_string y_coord) in
			true
		with _ ->
			Dom_html.window##alert
				(Js.string "Invalid coordinates");
			false
	in

	let rec add_subtitle_row row_no =
		let row_count = table_elm##rows##length in
		let start_time = start_ph_elm##value in
		let end_time = end_ph_elm##value in
		let x_coord = x_coord_elm##value in
		let y_coord = y_coord_elm##value in
		(* first new subtitle *)
		if row_count == 1 then
		begin
			Firebug.console##log(Js.string "[first new subtitle]");
			let prev_end = Js.string "0.00" in
			let next_start = (!pop_ref)##duration() in
			if is_time_valid start_time end_time prev_end next_start &&
				is_coord_valid x_coord y_coord
			then begin
				let new_row = table_elm##insertRow(row_count) in
				let start_cell = new_row##insertCell(0) in
				let end_cell = new_row##insertCell(1) in
				let text_cell = new_row##insertCell(2) in
				let x_coord_cell = new_row##insertCell(3) in
				let y_coord_cell = new_row##insertCell(4) in
				end_of_sub := end_ph_elm##value;
				start_cell##innerHTML <- start_ph_elm##value;
				end_cell##innerHTML <- end_ph_elm##value;
				x_coord_cell##innerHTML <- x_coord;
				y_coord_cell##innerHTML <- y_coord;
				text_cell##innerHTML <- textbox##value;
				start_ph_elm##value <- end_ph_elm##value;
				start_slider##setValue
					(float_of_string (Js.to_string end_ph_elm##value));
			end
			else ()
		end
		(* it is a new subtitle not the first one *)
		else if row_no == row_count then
		begin
			Firebug.console##log(Js.string "[new subtitle]");
			let prev_row = Js.Opt.get(table_elm##rows##item(row_no - 1))
				(fun _ -> assert false) in
			let prev_end_cell = Js.Opt.get(prev_row##cells##item(1))
				(fun _ -> assert false) in
			let prev_end = prev_end_cell##innerHTML in
			let next_start = (!pop_ref)##duration() in
			if is_time_valid start_time end_time prev_end next_start
			then begin
				let new_row = table_elm##insertRow(row_count) in
				let start_cell = new_row##insertCell(0) in
				let end_cell = new_row##insertCell(1) in
				let text_cell = new_row##insertCell(2) in
				let x_coord_cell = new_row##insertCell(3) in
				let y_coord_cell = new_row##insertCell(4) in
				end_of_sub := end_ph_elm##value;
				start_cell##innerHTML <- start_ph_elm##value;
				end_cell##innerHTML <- end_ph_elm##value;
				text_cell##innerHTML <- textbox##value;
				x_coord_cell##innerHTML <- x_coord;
				y_coord_cell##innerHTML <- y_coord;
				start_ph_elm##value <- end_ph_elm##value;
				start_slider##setValue
					(float_of_string (Js.to_string end_ph_elm##value));
			end
			else ()
		end
		(* existing subtitle *)
		else
		begin
			let row = Js.Opt.get(table_elm##rows##item(row_no))
				(fun _ -> assert false) in
			let start_cell = Js.Opt.get(row##cells##item(0))
				(fun _ -> assert false) in
			let end_cell = Js.Opt.get(row##cells##item(1))
				(fun _ -> assert false) in
			let text_cell = Js.Opt.get(row##cells##item(2))
				(fun _ -> assert false) in
			let x_coord_cell = Js.Opt.get(row##cells##item(3))
				(fun _ -> assert false) in
			let y_coord_cell = Js.Opt.get(row##cells##item(4))
				(fun _ -> assert false) in
			(* if curr time lies within the range between start & end time *)
			if comp_float_string_equal start_cell##innerHTML start_ph_elm##value ||
				comp_float_string_equal end_ph_elm##value end_cell##innerHTML
			then begin
				if row_no == 1 then (* if editing the first row of table *)
				begin
					let prev_end = Js.string "0.00" in
					if row_count == 2 then (* only one row in table *)
					begin
						Firebug.console##log(Js.string "[first only row]");
						let next_start = (!pop_ref)##duration() in
						if is_time_valid start_time end_time prev_end next_start
						then begin
							end_of_sub := end_time;
							start_cell##innerHTML <- start_time;
							end_cell##innerHTML <- end_time;
							text_cell##innerHTML <- textbox##value;
							x_coord_cell##innerHTML <- x_coord;
							y_coord_cell##innerHTML <- y_coord;
						end
						else ()
					end (* if row_count == 2 *)
					else (* more than 2 rows in table *)
					begin
						Firebug.console##log(Js.string "[first row]");
						let next_row = Js.Opt.get(table_elm##rows##item(row_no + 1))
							(fun _ -> assert false) in
						let next_start_cell = Js.Opt.get(next_row##cells##item(0))
							(fun _ -> assert false) in
						let next_start = next_start_cell##innerHTML in
						if is_time_valid start_time end_time prev_end next_start
						then begin
							start_cell##innerHTML <- start_time;
							end_cell##innerHTML <- end_time;
							text_cell##innerHTML <- textbox##value;
							x_coord_cell##innerHTML <- x_coord;
							y_coord_cell##innerHTML <- y_coord;
						end
						else ()
					end
				end (* if row_no == 1 *)
				(* last row of table *)
				else if row_no == row_count - 1 && row_no <> 1 then
				begin
					Firebug.console##log(Js.string "[last row]");
					let prev_row = Js.Opt.get(table_elm##rows##item(row_no - 1))
						(fun _ -> assert false) in
					let prev_end_cell = Js.Opt.get(prev_row##cells##item(1))
						(fun _ -> assert false) in
					let prev_end = prev_end_cell##innerHTML in
					let next_start = (!pop_ref)##duration() in
					if is_time_valid start_time end_time prev_end next_start
					then begin
						end_of_sub := end_time;
						start_cell##innerHTML <- start_time;
						end_cell##innerHTML <- end_time;
						text_cell##innerHTML <- textbox##value;
						x_coord_cell##innerHTML <- x_coord;
						y_coord_cell##innerHTML <- y_coord;
					end
					else ()
				end (* if row_no == row_count - 1 *)
				else (* rows neither the first nor the last *)
				begin
					Firebug.console##log(Js.string "[row in between]");
					let prev_row = Js.Opt.get(table_elm##rows##item(row_no - 1))
						(fun _ -> assert false) in
					let prev_end_cell = Js.Opt.get(prev_row##cells##item(1))
						(fun _ -> assert false) in
					let next_row = Js.Opt.get(table_elm##rows##item(row_no + 1))
						(fun _ -> assert false) in
					let next_start_cell = Js.Opt.get(next_row##cells##item(0))
						(fun _ -> assert false) in
					let prev_end = prev_end_cell##innerHTML in
					let next_start = next_start_cell##innerHTML in
					if is_time_valid start_time end_time prev_end next_start
					then begin
						start_cell##innerHTML <- start_time;
						end_cell##innerHTML <- end_time;
						text_cell##innerHTML <- textbox##value;
						x_coord_cell##innerHTML <- x_coord;
						y_coord_cell##innerHTML <- y_coord;
					end
					else ()
				end
			end
			(* try the next row *)
			else
				add_subtitle_row (row_no + 1)
		end
		in
	
	let insert_subtitle start_ end_ text =
		let st = empty_subtitle () in
		st##start_ <- start_;
		st##end_ <- end_;
		st##text <- text;
		(!pop_ref)##subtitle(st) in

	let refresh_subtitles () = 
		(!pop_ref)##destroy();
		pop_ref := popcorn(Js.string "#myvideo") in

	let rec build_subtitles table_elm row_no =
		let rows_length = table_elm##rows##length in
		if rows_length == row_no then
			()
		else
		let row = Js.Opt.get(table_elm##rows##item(row_no))
			(fun _ -> assert false) in
		let start_cell = Js.Opt.get(row##cells##item(0))
			(fun _ -> assert false) in
		let end_cell = Js.Opt.get(row##cells##item(1))
			(fun _ -> assert false) in
		let text_cell = Js.Opt.get(row##cells##item(2))
			(fun _ -> assert false) in
		let start_time = start_cell##innerHTML in
		let end_time = end_cell##innerHTML in
		let text = text_cell##innerHTML in
		insert_subtitle start_time end_time text;
		build_subtitles table_elm (row_no + 1)
		in

	let clear_rows rows_length =
		for i=(rows_length-1) downto 1 do
		table_elm##deleteRow(i) done
		in

	let clear_all_subtitles () =
		let rows_length = table_elm##rows##length in
		start_ph_elm##value <- Js.string "0.00";
		end_ph_elm##value <- Js.string "0.00";
		textbox##value <- Js.string "";
		end_of_sub := Js.string "0.00";
		clear_rows rows_length;
		refresh_subtitles ()
		in

	Lwt.async
		(fun () ->
			let open Lwt_js_events in
				Lwt.pick[
					clicks button_play
					(fun _ _ -> play_video (); Lwt.return ());
					clicks button_pause
					(fun _ _ -> pause_video (); Lwt.return ());
					clicks button_big
					(fun _ _ -> set_video_width 800; Lwt.return ());
					clicks button_medium
					(fun _ _ -> set_video_width 640; Lwt.return ());
					clicks button_small
					(fun _ _ -> set_video_width 480; Lwt.return ());
					clicks button_step_forward
					(fun _ _ -> step_video 5.0; Lwt.return ());
					clicks button_step_backward
					(fun _ _ -> step_video (-5.0); Lwt.return ());
					clicks button_toggle_graph
					(fun _ _ -> switch_visibility canvas_elm; Lwt.return ());
					clicks button_add
					(fun _ _ -> add_subtitle_row 1; Lwt.return ());
					clicks button_clear
					(fun _ _ -> clear_all_subtitles (); Lwt.return ());
			    	keypresses video_elm
					(fun ev _ ->
				    	let key = (Js.Optdef.get (ev##charCode) (fun() -> 0)) in
				    	let is_paused = Js.to_bool video_elm##paused in
				    	if key = 106 then
				    		step_video (-5.0)
				    	else if key = 108 then
				    		step_video 5.0
				    	else if key = 107 && is_paused then
				    		play_video ()
				    	else if key = 107 && not is_paused then
				    		pause_video ();
				    	Lwt.return());
		]);


	(* update placeholders: start_time, end_time & textbox *)
	(* according to currentTime of video *)
	let rec update_phs_by_time row_no curr_time =
		if comp_float_string_lessequal !end_of_sub curr_time then
		begin
			start_ph_elm##value <- (!end_of_sub);
			end_ph_elm##value <- curr_time;
			start_slider##setValue(float_of_string (Js.to_string !end_of_sub));
			end_slider##setValue(float_of_string (Js.to_string curr_time));
			textbox##value <- Js.string "";
			subtitle_text_elm##innerHTML <- Js.string " ";
			(*
			Firebug.console##log_2(Js.string "curr:", curr_time);
			Firebug.console##log_2(Js.string "end of sub:", !end_of_sub);
			*)
		end
		else
		let rows_length = table_elm##rows##length in
		if rows_length == row_no then (* no subtitle found *)
		begin
			end_ph_elm##value <- curr_time;
			end_slider##setValue(float_of_string (Js.to_string curr_time));
			textbox##value <- Js.string "";
			subtitle_text_elm##innerHTML <- Js.string " ";
		end
		else
		let row = Js.Opt.get(table_elm##rows##item(row_no))
			(fun _ -> assert false) in
		let start_cell = Js.Opt.get(row##cells##item(0))
			(fun _ -> assert false) in
		let end_cell = Js.Opt.get(row##cells##item(1))
			(fun _ -> assert false) in
		let text_cell = Js.Opt.get(row##cells##item(2))
			(fun _ -> assert false) in
		let x_coord_cell = Js.Opt.get(row##cells##item(3))
			(fun _ -> assert false) in
		let y_coord_cell = Js.Opt.get(row##cells##item(4))
			(fun _ -> assert false) in
		let start_time = start_cell##innerHTML in
		let end_time = end_cell##innerHTML in
		let text = text_cell##innerHTML in
		let x_coord = x_coord_cell##innerHTML in
		let y_coord = y_coord_cell##innerHTML in
		(* for debug purpose
		Firebug.console##log_2(Js.string "[row]:", Js.string (string_of_int row_no));
		Firebug.console##log_2(Js.string "start:", start_time);
		Firebug.console##log_2(Js.string "end:", end_time);
		Firebug.console##log_2(Js.string "text:", text);
		*)
		if comp_float_string_lessequal start_time curr_time && 
			comp_float_string_lessequal curr_time end_time then
		begin
			start_ph_elm##value <- start_time;
			end_ph_elm##value <- end_time;
			start_slider##setValue(float_of_string (Js.to_string start_time));
			end_slider##setValue(float_of_string (Js.to_string end_time));
			textbox##value <- text;
			subtitle_text_elm##innerHTML <- text;
			subtitle_text_elm##style##top <-
				Js.string ((Js.to_string y_coord) ^ "px");
			subtitle_text_elm##style##left <-
				Js.string ((Js.to_string x_coord) ^ "px");
			(*
			Firebug.console##log_2(Js.string "start:", start_time);
			Firebug.console##log_2(Js.string "end:", end_time);
			Firebug.console##log_2(Js.string "text:", text);
			*)
		end
		else
			update_phs_by_time (row_no + 1) curr_time
		in

	(* continuously update the time and subtitle *)
	(* ss = start_slider; es = end_slider *)
	let rec update_phs old_time old_ss_val old_es_val n =
		let origin_time = (!pop_ref)##currentTime_get() in
		let curr_time = fix_float_string_precision origin_time 2 in
		let ss_val = start_slider##getValue() in 
		let es_val = end_slider##getValue() in 
		let is_playing = not (Js.to_bool (!pop_ref)##paused()) in
		let n =
			if is_playing then
			begin
				update_phs_by_time 1 curr_time;
				20
			end
			else if old_ss_val <> ss_val then
			begin
				let ss_val_string =
					js_string_of_js_float start_slider##getValue() in
				let rounded_ss_val_string = 
					fix_float_string_precision ss_val_string 2 in
				(!pop_ref)##currentTime_set(rounded_ss_val_string);
				start_ph_elm##value <- rounded_ss_val_string;
				20
			end
			else if old_es_val <> es_val then
			begin
				let es_val_string =
					js_string_of_js_float end_slider##getValue() in
				let rounded_es_val_string = 
					fix_float_string_precision es_val_string 2 in
				(!pop_ref)##currentTime_set(rounded_es_val_string);
				end_ph_elm##value <- rounded_es_val_string;
				20
			end
			else
			begin
				max 0 (n - 1)
			end
		in
		Lwt_js.sleep (if n = 0 then 0.5 else 0.25) >>=
		fun () -> update_phs curr_time ss_val es_val n in
	ignore (update_phs (Js.string "0.00") (Js.float 0.) (Js.float 0.) 0)
}}

let () =
  Subtitle_app.register
    ~service:main_service
    (fun () () ->
	ignore{unit{ init_client () }};
      Lwt.return
        (Eliom_tools.F.html
           ~title:"Demo | Eliom multimedia"
           ~css:[
		   ["css";"subtitle.css"];
		   ["css";"bootstrap.min.css"];
		   ["css";"common.css"];
		   ["css";"slider.css"];
		   ["css";"hsvpalette.css"];
		   ]
           ~js:[["js";"popcorn-complete.min.js"]; ["subtitle_oclosure.js"]]
           Html5.F.(body [
		   div ~a:[Bootstrap.hero_unit] [
             h2 ~a:[Bootstrap.page_header]
			 	[pcdata "Eliom multimedia demo"];
			 video_controller;
			 video_wrapper;
			 subtitle_editor;
			]])))
