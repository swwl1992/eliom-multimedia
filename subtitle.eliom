{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
  open Html5.D
  open Lwt
  open Popcorn (* media library and popcorn.js API *)
}}

module Subtitle_app =
	Eliom_registration.App (
	struct
		let application_name = "subtitle"
	end)

let main_service =
	Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let button_play =  button ~button_type:`Button [pcdata "Play"]
let button_pause =  button ~button_type:`Button [pcdata "Pause"]
let button_big =  button ~button_type:`Button [pcdata "Big"]
let button_medium =  button ~button_type:`Button [pcdata "Medium"]
let button_small =  button ~button_type:`Button [pcdata "Small"]
let button_step_forward =  button ~button_type:`Button [pcdata "Step forward"]
let button_step_backward =  button ~button_type:`Button [pcdata "Step backward"]
let button_fullscr =  button ~button_type:`Button [pcdata "Fullscreen"]
let button_toggle_graph =  button ~button_type:`Button [pcdata "Graphics"]

let button_add =  button ~button_type:`Button [pcdata "Insert a Subtitle"]
let button_save =  button ~button_type:`Button [pcdata "Save"]
let button_clear =  button ~button_type:`Button [pcdata "Clear All"]

let t_row = tr [
	td [pcdata "Start"];
	td [pcdata "End"];
	td [pcdata "Text"]]
let subtitle_table = tablex [tbody [t_row]]

let textarea = raw_textarea ~a:[a_cols 60] ~name:"subline" ()

let start_time_ph = string_input ~input_type:`Text ~value:"0.00" ()
let end_time_ph = string_input ~input_type:`Text ~value:"0.00" ()

let video_player =
	video
	~srcs:(make_uri (Eliom_service.static_dir ())
		["oceans-clip.webm"],[])
	~a:[a_controls (`Controls); a_id "myvideo"]
	[pcdata "your browser does not support video element"]

let video_controller = div [
	 label [pcdata "Playback"];
	 button_play;
	 button_pause;
	 button_step_forward;
	 button_step_backward;
	 br ();
	 label [pcdata "Frame size"];
	 button_big;
	 button_medium;
	 button_small;
	 br ();
	 label [pcdata "Overlay graphics"];
	 button_toggle_graph;
	]

let subtitle_editor = div [
	start_time_ph;
	br ();
	end_time_ph;
	br ();
	textarea;
	br ();
	button_add;
	subtitle_table;
	button_save;
	button_clear;
	]

{client{
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

	(* initialization values *)
	table_elm##border <- Js.string "1";
	start_ph_elm##value <- Js.string "0.00";
	end_ph_elm##value <- Js.string "0.00";
	textbox##value <- Js.string "";

	(*video controller functions*)
	let play_video _ =
		(!pop_ref)##play()
	in

	let pause_video _ =
		(!pop_ref)##pause()
	in

	(* subtitle edition functions *)

	let fix_float_string_precision js_string precision =
		let in_flt = float_of_string (Js.to_string js_string) in
		let in_num = Js.number_of_float in_flt in
		let out_flt = in_num##toFixed(precision) in
		out_flt
		in

	let comp_float_string_less str1 str2 =
		let num1 = float_of_string(Js.to_string str1) in
		let num2 = float_of_string(Js.to_string str2) in
		num1 < num2
		in

	let comp_float_string_lessequal str1 str2 =
		let num1 = float_of_string(Js.to_string str1) in
		let num2 = float_of_string(Js.to_string str2) in
		num1 <= num2
		in

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
		(*first subtitle *)
		if row_count == 1 then
		begin
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
		(* for debug purpose
		Firebug.console##log_2(Js.string "[row]:", Js.string (string_of_int row_no));
		Firebug.console##log_2(Js.string "start:", start_time);
		Firebug.console##log_2(Js.string "end:", end_time);
		Firebug.console##log_2(Js.string "text:", text);
		*)
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
	(* TODO get rid of using var end_of_sub *)
	let rec update_phs table_elm row_no curr_time =
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
			update_phs table_elm (row_no + 1) curr_time
		in

	(* continuously update the time and subtitle *)
	let rec update_end_ph old_time n =
		let origin_time = (!pop_ref)##currentTime_get() in
		let curr_time = fix_float_string_precision origin_time 2 in
		let n =
			if curr_time <> old_time then begin
				begin try
					update_phs table_elm 1 curr_time;
				with _ -> () end;
				20
			end else
				max 0 (n - 1)
		in
		Lwt_js.sleep (if n = 0 then 0.5 else 0.25) >>=
		fun () -> update_end_ph curr_time n in
	ignore (update_end_ph (Js.string "0.00") 0)
}}

let () =
  Subtitle_app.register
    ~service:main_service
    (fun () () ->
	ignore{unit{ init_client () }};
      Lwt.return
        (Eliom_tools.F.html
           ~title:"Demo | Eliom multimedia"
           ~css:[["css";"subtitle.css"]]
           ~js:[["js";"popcorn-complete.min.js"]]
           Html5.F.(body [
             h2 [pcdata "Eliom multimedia demo"];
			 video_controller;
			 video_player;
			 subtitle_editor;
           ])))
