open Alcotest

let data_ = Queue.create ()
let data () = Queue.pop data_

let now =
  let n = ref ~-1 in
  fun () -> incr n; Int64.of_int !n

let push_to_queue msg =
  Queue.push msg data_;
  Lwt.return_unit

let src =
  let open Metrics in
  let tags = Tags.[int "foo"; string "bar"] in
  let data i =
    Data.v [string "toto" ("XXX" ^ string_of_int i); int "titi" i]
  in
  Src.v "test" ~tags ~data

let f tags =
  Metrics.add src tags (fun m -> m 42)

let i0 t = t 42 "hi!"
let i1 t = t 12 "toto"

let test_f () =
  f i0;
  check string "i0" "test,foo=42,bar=hi! toto=\"XXX43\",titi=43i\n" (data ());
  f i1;
  check string "i0" "test,foo=12,bar=toto toto=\"XXX43\",titi=43i\n" (data ())

let () =
  Metrics.enable_all ();
  let reporter = Metrics_influx.lwt_reporter push_to_queue now in
  Metrics.set_reporter reporter;
  f i0 ; f i1 ; 
  let push_result1 = data () in
  let push_result2 = data () in

  let reporter, get_data = Metrics_influx.lwt_reporter_pull now in
  Metrics.set_reporter reporter;
  f i0 ;
  let pull_result1 = get_data () in
  f i1 ;
  let pull_result2 = get_data () in
  
  let test_string a b () =
    check string __LOC__ a b in

  Alcotest.run "metrics"
    [ ( "base"
      , [ "data point 1", `Quick, test_string push_result1 pull_result1
        ; "data point 2", `Quick, test_string push_result2 pull_result2
         ] ) ]
