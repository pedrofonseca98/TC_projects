let minOfInt x y z = min x (min y z);;

let editDist a b m n subMatrix =
  for i = 0 to m do
    for j = 0 to n do
      if i = 0 then subMatrix.(i).(j) <- j
      else if j = 0 then subMatrix.(i).(j) <- i
      else if (String.get a (i - 1)) = (String.get b (j - 1)) then subMatrix.(i).(j) <- subMatrix.(i - 1).(j - 1)
      else subMatrix.(i).(j) <- 1 + (minOfInt subMatrix.(i).(j - 1) subMatrix.(i - 1).(j) subMatrix.(i - 1).(j - 1))
    done;
  done;

  subMatrix.(m).(n)


(*--------------------------------------------------------------------------*)
let () =
  let (a:string) = read_line() in
  let (b:string) = read_line() in
  let m = String.length(a) in
  let n = String.length(b) in
  let subMatrix = Array.make_matrix (m + 1) (n + 1) 0 in
    print_endline(string_of_int(editDist a b m n subMatrix))


(*
Baseado no algoritmo da Distância Levenshtein / Distância de Edição,
(wikipedia.com ; nlp.stanford.edu)
*)
