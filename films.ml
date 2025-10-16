type movie = {
  id : int;
  title : string;
  year : int;
  runtime : int;
  rank : int
}

type res = Movie of movie | Invalid | Eof

let input_movie in_c =
  try
    let s = input_line in_c in 
    match String.split_on_char ';' s with
    [ s_id; title; s_year; s_runtime; s_rank ] -> 
      Movie({
        id = int_of_string s_id;
        title = title;
        year = int_of_string s_year;
        runtime = int_of_string s_runtime;
        rank = int_of_string s_rank;
      })
      |_ -> Invalid 
  with
  End_of_file -> Eof
  |_ -> Invalid

let load_movies f =
  let in_c = open_in f in
  let rec loop in_c acc =
    match input_movie in_c with
    | Eof -> acc
    | Invalid -> loop in_c acc
    | Movie m -> loop in_c (m :: acc)
  in
    let res = loop in_c [] in
    close_in in_c;
    res

let movies = load_movies "movies.csv"

(*Fonction qui afficher un film dans la console*)
let pr_movie (m:movie) : unit =
  Printf.printf "{ id = %d; title = \"%s\"; year = %d; runtime = %d; rank = %d }\n" m.id m.title m.year m.runtime m.rank


let pr_movies (l:movie list) : unit = List.iter pr_movie l

let moviesTop10 (l:movie list) : movie list =
  List.filter (fun m -> m.rank <= 10) l

let movies1980 (l:movie list) : movie list = List.filter (fun m -> m.year < 1990 && m.year > 1979) l

let movies_titles (l:movie list) : string list =
  List.map (fun m -> m.title) l

let max_id (l:movie list) : int = 
  match l with 
  |[] -> 0
  |_::_ -> 
    List.fold_left (fun acc m -> if acc > m.id then acc else m.id) 0 l


let () =
(* match movies with
 |m::_ -> pr_movie m
 |[] -> ()*)
  (*List.iter (fun t -> Printf.printf "%s\n" t) (movies_titles movies)*)
  Printf.printf "%d\n" (max_id movies)