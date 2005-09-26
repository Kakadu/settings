open Options
open Settings

let main () = 
  let oParser = customize 
    (empty ())
     [
       "o", String, Optional , "   \t - optional option";
       "m", String, Mandatory, " \t - mandatory option";
       "a", Accu  , Mandatory, "    \t - accumulator"
     ]
    (fun () -> ["o", Str "a.out"])
  in
  let conf, rest, warnings =
    match oParser (List.tl (Array.to_list Sys.argv)) with
    | Ok (conf, rest) -> conf, rest, []
    | Warnings (conf, rest, warnings) -> conf, rest, warnings
  in
  let concat l = List.fold_left (fun c w -> c ^ "\n  " ^ w) "" warnings in
  Printf.printf "Settings:\n  %s\nFree parameters:%s\nWarnings:%s\n"
    (conf.toString ())
    (concat rest)
    (concat warnings);
  Printf.printf "Option -o: %s\n" (match conf.get "o" with None -> "not set" | Some (Str x) -> x);
  Printf.printf "Option -m: %s\n" (match conf.get "m" with None -> "not set" | Some (Str x) -> x);
  Printf.printf "Option -a: %s\n" (match conf.get "a" with None -> "not set" | Some (Str x) -> x);
  Printf.printf "Help:\n%s\n" (conf.help ())
;;

main ()
      




