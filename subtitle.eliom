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
	(* DOM elements *)
	let video_elm = of_video %video_player in
	let table_elm = To_dom.of_table %subtitle_table in
	let start_ph_elm = To_dom.of_input %start_time_ph in
	let end_ph_elm = To_dom.of_input %end_time_ph in
	let textbox = To_dom.of_textarea %textarea in
	let button_add = To_dom.of_button %button_add in
	let button_save = To_dom.of_button %button_save in
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
	(* google closure slider constructor *)
	let build_time_slider elm duration =
		let slider = jsnew Goog.Ui.slider(Js.null) in
		let duration_flt = float_of_string (Js.to_string duration) in
		slider##setMinimum(0.);
		slider##setMaximum(duration_flt);
		slider##setValue(0.0);
		slider##setMoveToPointEnabled(Js._true);
		slider##setStep(Js.some 0.1);
		slider##renderBefore(elm);
		slider
	in

	(* initialization *)
	start_ph_elm##value <- Js.string "0.00";
	end_ph_elm##value <- Js.string "0.00";
	textbox##value <- Js.string "";
	let duration = (!pop_ref)##duration() in
	let start_slider = build_time_slider start_ph_elm duration in
	let end_slider = build_time_slider end_ph_elm duration in

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

	let comp_float_string_lessequal str1 str2 =
		let num1 = float_of_string(Js.to_string str1) in
		let num2 = float_of_string(Js.to_string str2) in
		num1 <= num2
	in

	(*video controller functions*)
	let play_video _ =
		(!pop_ref)##play()
	in

	let pause_video _ =
		(!pop_ref)##pause()
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
			(Js.string "Invalid input format");
			false
	in

	let rec add_subtitle_row row_no =
		let row_count = table_elm##rows##length in
		let start_time = start_ph_elm##value in
		let end_time = end_ph_elm##value in
		(* first subtitle *)
		if row_count == 1 then
		begin
			let prev_end = Js.string "0.00" in
			let next_start = (!pop_ref)##duration() in
			if is_time_valid start_time end_time prev_end next_start
			then begin
				let new_row = table_elm##insertRow(row_count) in
				let start_cell = new_row##insertCell(0) in
				let end_cell = new_row##insertCell(1) in
				let text_cell = new_row##insertCell(2) in
				end_of_sub := end_ph_elm##value;
				start_cell##innerHTML <- start_ph_elm##value;
				end_cell##innerHTML <- end_ph_elm##value;
				text_cell##innerHTML <- textbox##value;
				start_ph_elm##value <- end_ph_elm##value;
			end
			else ()
		end
		(* it is a new subtitle not the first one *)
		else if row_no == row_count then
		begin
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
				end_of_sub := end_ph_elm##value;
				start_cell##innerHTML <- start_ph_elm##value;
				end_cell##innerHTML <- end_ph_elm##value;
				text_cell##innerHTML <- textbox##value;
				start_ph_elm##value <- end_ph_elm##value;
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
			let curr_time = (!pop_ref)##currentTime_get() in

			(* if curr time lies within the range between start & end time *)
			if comp_float_string_lessequal start_cell##innerHTML curr_time &&
				comp_float_string_lessequal curr_time end_cell##innerHTML
			then begin
				if row_no == 1 then (* if editing the first row of table *)
				begin
					let prev_end = Js.string "0.00" in
					if row_count == 2 then (* only one row in table *)
					begin
						let next_start = (!pop_ref)##duration() in
						if is_time_valid start_time end_time prev_end next_start
						then begin
							end_of_sub := end_time;
							start_cell##innerHTML <- start_time;
							end_cell##innerHTML <- end_time;
							text_cell##innerHTML <- textbox##value;
						end
						else ()
					end (* if row_count == 2 *)
					else (* more than 2 rows in table *)
					begin
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
						end
						else ()
					end
				end (* if row_no == 1 *)
				else if row_no == row_count - 1 then (* last row of table *)
				begin
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
					end
					else ()
				end (* if row_no == row_count - 1 *)
				else (* rows neither the first nor the last *)
				begin
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

	let save_subtitles () =
		refresh_subtitles ();
		build_subtitles table_elm 1 in

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
					clicks button_add
					(fun _ _ -> add_subtitle_row 1; Lwt.return ());
					clicks button_save
					(fun _ _ -> save_subtitles (); Lwt.return ());
					clicks button_clear
					(fun _ _ -> clear_all_subtitles (); Lwt.return ());
		]);

	(* update placeholders: start_time, end_time & textbox *)
	(* according to currentTime of video *)
	let rec update_phs_by_time row_no curr_time =
		if comp_float_string_lessequal !end_of_sub curr_time then
		begin
			start_ph_elm##value <- (!end_of_sub);
			end_ph_elm##value <- curr_time;
			textbox##value <- Js.string "";
			(*
			Firebug.console##log_2(Js.string "curr:", curr_time);
			Firebug.console##log_2(Js.string "end of sub:", !end_of_sub);
			*)
			()
		end
		else
		let rows_length = table_elm##rows##length in
		if rows_length == row_no then (* no subtitle found *)
		begin
			end_ph_elm##value <- curr_time;
			textbox##value <- Js.string "";
			()
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
		let start_time = start_cell##innerHTML in
		let end_time = end_cell##innerHTML in
		let text = text_cell##innerHTML in
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
			textbox##value <- text;
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
	let rec update_phs old_time old_ss_val n =
		let origin_time = (!pop_ref)##currentTime_get() in
		let curr_time = fix_float_string_precision origin_time 2 in
		let ss_val = start_slider##getValue() in 
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
			else
			begin
				update_phs_by_time 1 curr_time;
				max 0 (n - 1)
			end
		in
		Lwt_js.sleep (if n = 0 then 0.5 else 0.25) >>=
		fun () -> update_phs curr_time ss_val n in
	ignore (update_phs (Js.string "0.00") (Js.float 0.) 0)
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
		   ]
           ~js:[["js";"popcorn-complete.min.js"]; ["subtitle_oclosure.js"]]
           Html5.F.(body [
		   div ~a:[Bootstrap.container] [
             h2 [pcdata "Eliom multimedia demo"];
			 video_controller;
			 video_player;
			 subtitle_editor;
			]])))
