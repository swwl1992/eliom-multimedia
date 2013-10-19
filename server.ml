open Eliom_content
open Html5
open Html5.D

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

let canvas_graphics =
	canvas ~a:[a_width 400; a_height 200]
		[pcdata "your browser does not support canvas"]

let t_row = tr [
	td [pcdata "Start"];
	td [pcdata "End"];
	td [pcdata "Text"]]
let subtitle_table =
	tablex ~a:[Bootstrap.table; Bootstrap.table_bordered] [tbody [t_row]]

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
	div ~a:[Bootstrap.row_fluid]
	[div ~a:[Bootstrap.span 3] [pcdata "Playback"];
	 div ~a:[Bootstrap.span 9]
	 [
	 	button_play;
	 	button_pause;
	 	button_step_forward;
	 	button_step_backward;
	 ]
	];
	div ~a:[Bootstrap.row_fluid]
	[div ~a:[Bootstrap.span 3] [pcdata "Frame size"];
	 div ~a:[Bootstrap.span 9]
	 [
	 	button_big;
	 	button_medium;
	 	button_small;
	 ]
	];
	div ~a:[Bootstrap.row_fluid]
	[
	 div ~a:[Bootstrap.span 3] [pcdata "Overlay graphics"];
	 div ~a:[Bootstrap.span 9] [button_toggle_graph];
	];
	]

let video_wrapper =
	div
	~a:[a_style "position:relative"]
	[canvas_graphics; video_player]

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
