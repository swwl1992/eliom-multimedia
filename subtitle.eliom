{shared{
  open Eliom_lib
  open Eliom_content
}}

module Subtitle_app =
  Eliom_registration.App (
    struct
      let application_name = "subtitle"
    end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  Subtitle_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"subtitle"
           ~css:[["css";"subtitle.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's destillery!"];
           ])))
