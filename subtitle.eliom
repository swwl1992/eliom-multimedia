{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
  open Html5.D
  open Lwt

  (* media library and popcorn.js API *)
  open Popcorn
}}

module Subtitle_app =
  Eliom_registration.App (
    struct
      let application_name = "subtitle"
    end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let button_add =  button ~button_type:`Button [pcdata "Add a Subtitle"]
let button_save =  button ~button_type:`Button [pcdata "Save"]
let button_clear =  button ~button_type:`Button [pcdata "Clear All"]

let t_row = tr [
	td [pcdata "Start"];
	td [pcdata "End"];
	td [pcdata "Text"]]
let subtitle_table = tablex [tbody [t_row]]

let start_time_ph = div [pcdata "0.000000"]
let end_time_ph = div [pcdata "0.000000"]

let video_player =
	video	
	~srcs:(make_uri (Eliom_service.static_dir ())
		["oceans-clip.webm"],[])
	~a:[a_controls (`Controls); a_id "myvideo"]
	[pcdata "Your browser does not support audio element"]

let subtitle_editor = div []

{client{
let init_client _ =
	let d = Dom_html.document in
	let table_elm = To_dom.of_table %subtitle_table in
	let div_elm = To_dom.of_div %subtitle_editor in
	let start_ph_elm = To_dom.of_div %start_time_ph in
	let end_ph_elm = To_dom.of_div %end_time_ph in
	let button_add = To_dom.of_button %button_add in
	let button_save = To_dom.of_button %button_save in
	let button_clear = To_dom.of_button %button_clear in
	(* create a empty object of popcorn *)
	let pop = popcorn(Js.string "#myvideo") in
	(* make a reference of it - changed to variable *)
	let pop_ref = ref pop in

	let textbox = Dom_html.createTextarea d in
	table_elm##border <- Js.string "1";
	textbox##cols <- 60;
	Dom.appendChild div_elm start_ph_elm;
	Dom.appendChild div_elm end_ph_elm;
	Dom.appendChild div_elm textbox;
	Dom.appendChild div_elm button_add;
	Dom.appendChild div_elm table_elm;
	Dom.appendChild div_elm button_save;
	Dom.appendChild div_elm button_clear;

	let add_subtitle_row () =
		let row_count = table_elm##rows##length in
		let row = table_elm##insertRow(row_count) in
		let start_cell = row##insertCell(0) in
		let end_cell = row##insertCell(1) in
		let text_cell = row##insertCell(2) in
		start_cell##innerHTML <- start_ph_elm##innerHTML;
		end_cell##innerHTML <- end_ph_elm##innerHTML;
		text_cell##innerHTML <- textbox##value;
		textbox##value <- Js.string "";
		start_ph_elm##innerHTML <- end_ph_elm##innerHTML in
	
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
		Firebug.console##log_2(Js.string "row count:", Js.string (string_of_int rows_length));
		(* cannot find a subtitle -> return empty string *)
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
		start_ph_elm##innerHTML <- Js.string "0.000000";
		end_ph_elm##innerHTML <- Js.string "0.000000";
		clear_rows rows_length;
		refresh_subtitles ()
		in

	Lwt.async
		(fun () ->
			let open Lwt_js_events in
				Lwt.pick[
					clicks (button_add)
					(fun _ _ -> add_subtitle_row (); Lwt.return ());
					clicks (button_save)
					(fun _ _ -> save_subtitles (); Lwt.return ());
					clicks (button_clear)
					(fun _ _ -> clear_all_subtitles (); Lwt.return ());
		]);

	(* continuously update the time and subtitle *)
	let rec update_end_ph old_time n =
		let curr_time = pop##currentTime_get() in
		if start_ph_elm##innerHTML > curr_time then
		start_ph_elm##innerHTML <- curr_time;
		let n =
			if curr_time <> old_time then begin
				begin try
					end_ph_elm##innerHTML <- curr_time;
				with _ -> () end;
				20
			end else
				max 0 (n - 1)
		in
		Lwt_js.sleep (if n = 0 then 0.5 else 0.25) >>=
		fun () -> update_end_ph curr_time n in
	ignore (update_end_ph (Js.string "0.000") 0)
}}

let () =
  Subtitle_app.register
    ~service:main_service
    (fun () () ->
	ignore{unit{ init_client () }};
      Lwt.return
        (Eliom_tools.F.html
           ~title:"Demo | Subtitle Editor"
           ~css:[["css";"subtitle.css"]]
           ~js:[["js";"popcorn-complete.min.js"]]
           Html5.F.(body [
             h2 [pcdata "HTML5 Video Subtitle Editor"];
			 video_player;
			 subtitle_editor;
           ])))
