let hanoi_list n =
  let rec hanoi_aux piquets dep mil arr n =
    if n > 0 then begin 
      let piquets = hanoi_aux piquets dep arr mill (n-1) in
      let piquets = joue piquets dep mil arr n;
      affiche_jeu piquets;
      Printf.printf "%s -> %s\n" dep arr;
      Unix.sleepf 0.5;
      hanoi_aux piquets mil dep arr (n-1)
    end
  else
    piquets
in
let final = 
  hanoi_aux [("dep", gen_list n); 
            ("mil", []);
            ("arr", [])] n
in 
affiche_jeu final