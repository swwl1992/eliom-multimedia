open Js

class type mediaElement = object
  inherit Dom_html.element

  (* Network state *)
  method src : js_string t prop
  method currentSrc : js_string t readonly_prop
  method networkState : int readonly_prop
  method preload : js_string t prop
  method load : unit meth
  method canPlayType : js_string t -> js_string t meth

  (* Ready state *)
  method readyState : int readonly_prop
  method seeking : bool t readonly_prop

  (* Playback state *)
  method currentTime : float t prop
  method initialTime : float t readonly_prop
  method duration : float t readonly_prop
  method startOffsetTime : date t readonly_prop
  method paused : bool t readonly_prop
  method defaultPlaybackRate : float t prop
  method playbackRate : float t prop
  method ended : bool t readonly_prop
  method autoplay : bool t prop
  method loop : bool t prop

  method play : unit meth
  method pause : unit meth

  (* Media controller *)
  method mediaGroup : js_string t prop

  (* Controls *)
  method controls : bool t prop
  method volume : float t prop
  method muted : bool t prop
  method defaultMuted : bool t prop
end

class type audioElement = object
  inherit mediaElement
end

class type videoElement = object
  inherit mediaElement

  method width : int prop
  method height : int prop
  method videoWidth : int readonly_prop
  method videoHeight : int readonly_prop
end

(* plug-in API *)
class type footnote = object
  method start_ : js_string t prop
  method end_ : js_string t prop
  method target : js_string t prop
  method text : js_string t prop
end

let empty_footnote () : footnote t =
  Js.Unsafe.obj [||]

class type subtitle = object
  method start_ : js_string t prop
  method end_ : js_string t prop
  method target : js_string t prop
  method text : js_string t prop
end

let empty_subtitle () : subtitle t =
  Js.Unsafe.obj [||]

class type popcornElement = object
  (* media methods *)
  method play : unit meth
  method pause : unit meth
  method currentTime_set : js_string t -> unit meth
  method currentTime_get : js_string t meth
  method destroy : unit meth
  (* plug-ins*)
  method footnote : footnote t -> unit meth
  method subtitle : subtitle t -> unit meth
end

let empty_popcorn () : popcornElement t =
  Js.Unsafe.obj [||]

let popcorn 
  (id: js_string t) : popcornElement t =
  Js.Unsafe.fun_call (Js.Unsafe.variable "Popcorn")
  [|Js.Unsafe.inject id|]
