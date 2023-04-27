(* Task 1 *)
let rec compare num numbers = 
  match numbers with
  | [] -> false
  | x :: xs -> if x == num then true else compare num xs;;


let rec equal_second_components tuple tuples = 
    let destructure tuple = match tuple with (x,y) -> y
    in 
    let forComp = destructure tuple in

    match tuples with
    | [] -> false
    | (_, y)::xs -> if forComp == y then true else equal_second_components tuple xs


let rec evens_eq_evens_odds_eq_odds num numbers = 
  match numbers with
  | [] -> false
  | x::xs -> if (num mod 2) == (x mod 2) then true else evens_eq_evens_odds_eq_odds num xs;;

  
let member c t l = c t l;;

let testing_member () =
  let l =
    [
      __LINE_OF__ ((member compare 3 [1; 2; 3]) = true);
      __LINE_OF__ ((member compare 4 [1; 2; 3]) = false);
      __LINE_OF__ ((member compare 'a' ['a'; 'b'; 'c']) = true);
      __LINE_OF__ ((member equal_second_components ('a',5) [(1,2); (3,4); (5,6)]) = false);
      __LINE_OF__ ((member equal_second_components ('a',6) [(1,2); (3,4); (5,6)]) = true);
      __LINE_OF__ ((member equal_second_components (42, 6) [(1,2); (3,4); (5,6)]) = true);
      __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 2; 3]) = true);
      __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 3; 5]) = false);
   ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The member test succeeds.\n"; [])
  else (Printf.printf "The member test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)



(* Task 2 *)

let count_occurrences_longer l = 
  let rec innerCounter l tuples =
    match l with
    | [] -> tuples
    | x::xs -> 
    let rec iterativeCounter l number count = 
      match l with
      | [] -> (number, count)
      | y::ys -> if number = y then iterativeCounter ys number (count+1) else iterativeCounter ys number count
      in 
      let myTuple = iterativeCounter l x 0
      in let destructure = match myTuple with (letter, count) -> letter
      in 
      let rec checkIfExists tuples myTuple =  match tuples with
      | [] -> false
      | (c,_)::cs -> if c = destructure then true else checkIfExists cs myTuple
      in 
      let exists = checkIfExists tuples myTuple
      in 
      if exists == false then innerCounter xs (tuples@[myTuple])
      else 
      innerCounter xs (tuples)
      in (innerCounter l ([]:(('a * int) list)))

let count_occurrences lst = 
        let rec innerHelper l newL = 
          match l with
          | [] -> newL
          | x::xs -> 
          let rec checkTimes el l counter= 
           match l with 
           | [] -> counter
           | y::ys -> if el = y then checkTimes el ys (counter+1) else checkTimes el ys counter
           in 
           let times = checkTimes x l 0
           in 
           let rec checkInNewL el newL = 
            match newL with
            | [] -> false
            | (item, times)::tl -> if el = item then true else checkInNewL el tl
            in
            let isIn = checkInNewL x newL
            in
            if (isIn) then innerHelper xs newL else innerHelper xs ((x, times)::newL)
            in List.sort (fun (x, timesX) (y, timesY)-> if timesX > timesY then -1 else 1 ) (innerHelper lst [])

            

      let testing_count_occurrences () =
        let l =
          [
            __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
            __LINE_OF__ ((count_occurrences ['a'; 'b'; 'a'; 'c'; 'c'; 'a'; 'd']) = [('a', 3); ('c', 2); ('b', 1); ('d', 1)]);
            __LINE_OF__ ((count_occurrences [0; 0; 0; -2; 3; -1; -1; 3; 3; 0]) = [(0, 4); (3, 3); (-1, 2); (-2, 1)]);
            __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
          ] in
        let result = List.fold_left (&&) true (List.map snd l) in
        if result then (Printf.printf "The count_occurrences test succeeds.\n"; [])
        else (Printf.printf "The count_occurrences test fails.\n Check the corresponding line numbers in the list below.\n";
              (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


(* Task 3 *)

(* not so straight-away *)
let drop_last_but_longer l = 
  if l = [] then failwith "Empty list has no last elements" else
  let rec getLast l lastEl =
    match l with
    | [] -> lastEl
    | x::xs -> getLast xs x
    in 
    let lastEl = getLast l (0)
    in  
    let rec createNewL lst newL lastElToCompare =
      match lst
      with 
      | [] -> newL
      | y::ys -> if y=lastEl && ys=[] then createNewL ys newL lastElToCompare else  createNewL ys (newL@[y]) lastElToCompare
      in
      createNewL l [] lastEl;;


let drop_last l =
      let rec innerOne l newL = match l with
        |[] -> failwith "Empty list has no last elemnets"
        |x::xs -> if xs=[] then newL else innerOne xs (newL@[x])
        in innerOne l []


let testing_drop_last () =
          let l =
            [
              __LINE_OF__ ((drop_last [1; 2; 3; 4]) = [1; 2; 3]);
              __LINE_OF__ ((drop_last [1]) = []);
              __LINE_OF__ ((try Some (drop_last []) with (Failure _) -> None) = None) (* If this line is reported during testing, you have an rrror in raising Failure *)
            ] in
          let result = List.fold_left (&&) true (List.map snd l) in
          if result then (Printf.printf "The drop_last test succeeds.\n"; [])
          else (Printf.printf "The drop_last test fails.\n Check the corresponding line numbers in the list below.\n";
                (List.filter (fun (x,y) -> y=false) l) |> List.map fst)



(* Task 4 *)  


let drop_last_opt l =
  let rec innerOne l newL = match l with
  |[] -> None
  |x::xs -> if xs=[] then Some newL else innerOne xs (newL@[x])
  in innerOne l [];;

let testing_drop_last_opt () =
    let l =
      [
        __LINE_OF__ ((drop_last_opt []) = None);
        __LINE_OF__ ((drop_last_opt [1]) = Some []);
        __LINE_OF__ ((drop_last_opt [1;2;3]) = Some [1;2])
      ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The drop_last_opt test succeeds.\n"; [])
    else (Printf.printf "The drop_last_opt test fails.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst)



(* Task 5 *)  

let zip_with func l1 l2 =
  let rec helper newL l1 l2 = match l1 with
  | [] -> newL
  | x :: xs -> 
      let l2Helper l2 = match l2 with
  | [] -> newL
  | y::ys -> helper (newL@[(func x y)]) xs ys
  in l2Helper l2
  in
  helper [] l1 l2


  let testing_zip_with () =
    let l =
      [
        __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6]) = [[1; 5]; [2; 6]]);
        __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6;7;8]) = [[1; 5]; [2; 6]; [3; 7]]);
        __LINE_OF__ ((zip_with (fun x y -> (x,y)) [1;2;3] ['a';'b']) = [(1, 'a'); (2, 'b')]);
        __LINE_OF__ ((zip_with (+) [1;2;3] [5;6]) =[6; 8]);
        __LINE_OF__ ((zip_with (^) ["aa";"bb";"cc"] ["1";"2"]) = ["aa1"; "bb2"]);
 
      ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The zip_with test succeeds.\n"; [])
    else (Printf.printf "The zip_with test fails.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst)



(* Task 6 *) 


let without_fold l =
    let rec gatherFirst givenL newLF = 
      match givenL with
      | [] -> newLF
      | x::xs -> gatherFirst xs (newLF@[match x with (a,_) -> a])
      in 

      let rec gatherSecond givenL newLS =
        match givenL with
        | [] -> newLS
        | y::ys -> gatherSecond ys (newLS@[match y with (_, b) -> b])
        in 
        
        (gatherFirst l [], gatherSecond l [])
     
        

let unzip l = 
    (List.fold_right (fun tuple newL -> (fst tuple) :: newL) l [], 
    List.fold_right (fun tuple newL -> (snd tuple) :: newL) l [])



let testing_unzip () =
          let l =
            [
              __LINE_OF__ ((unzip [('a',1); ('b',2)]) = (['a';'b'], [1;2]));
              __LINE_OF__ ((unzip []) = ([], []));
              __LINE_OF__ ((unzip [('a',1)]) = (['a'], [1]));
       
            ] in
          let result = List.fold_left (&&) true (List.map snd l) in
          if result then (Printf.printf "The unzip test succeeds.\n"; [])
          else (Printf.printf "The unzip test fails.\n Check the corresponding line numbers in the list below.\n";
                (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


                
(* Task 7 *)

(* Show the evaluation steps of unzip [('a',1); ('b',2)] *)

(*
First of all, I will say that the unzip function is using the List module functionality, 
   one of which is the "folding". In addition, it shall be mentioned that the List.fold_right
   function is taking the function to be applied to the each element of the passed list(as an argument)
   on each iteration and the result of the applied function must be saved into an accumulator.

   I've used List.fold_right and not List.fold_left, even though the main difference isn't 
   necesserily obvious in this case, I'd still have to reverse the list for the
   List.fold_left function, because I was getting the reversed answer as a result. 
   For example, if I had to get, in this case, (['a'; 'b'], [1; 2]), I was getting the
   ([1; 2], ['a', 'b']) tuple, which isn't a breaking change, but still wasn't fulfilling as the final result,
   so I chose to stick with the right one and the reversed answer would be given in the end without 
   additional List module 'rev' method.
   
   To start with the evalution, I will say that the algorithm is fairly simple, I have to take the first
    and the second elements of each tuple and group them together.
    So, in this case the first element would be -> ('a', 1). From this tuple, 
    im taking the first element and I also want to mention
    that I'm using two fold_right functions and then combining them in a tuple to get the answer,
    so, I would just get the ['a'] and I would move to the next element and the accumulator(newL) would
    increase in size as well and ['a'] would be my "newL" after first element. 
    For the second element, I would still do the same and the final newL would be ['a'; 'b'];
    The first part of the function is done and I have something like this (['a'; 'b'], List.fold_right ...).
    The second part of the unzip function is basically operating on the second element of each pair in my given
    list and the first operation will take the 1 from ('a', 1), thus my accumulator(newL) looks like this for now [1],
    the second tuple will return the result in a same fashion and from ('b', 2) -> 2 will be extracted.
    Once again, the accumulator(newL) will get an additional element and the final result from this second 
    List.fold_right function will be [1; 2].
    At the end, the function will return the (['a'; 'b'], [1; 2]) as a result;
   *)




(* Task 8 *)

type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha;;

let wc22_C = 
  [(Arg, ["Messi"], Sau, ["Al-Shehri"; "Al-Dawsari"]);
   (Mex, [], Pol, []);
   (Pol, ["Zielinski"; "Lewandowski"], Sau, []);
   (Arg, ["Messi"; "Fernandez"], Mex, []);
   (Pol, [], Arg, ["Mac Allister"; "Alvarez"]);
   (Sau, ["Al-Dawsari"], Mex, ["Martin"; "Chavez"])
  ]

let wc22_H = 
    [(Uru, [], Kor, []);
     (Por, ["Ronaldo"; "Felix"; "Leao"], Gha, ["Ayew"; "Bukari"]);
     (Kor, ["Cho Gue-sung"; "Cho Gue-sung"], Gha, ["Salisu"; "Kudus"; "Kudus"]);
     (Por, ["Fernandes"; "Fernandes"], Uru, []);
     (Kor, ["Kim Young-gwon"; "Hwang Hee-chan"], Por, ["Horta"]);
     (Gha, [], Uru, ["De Arrascaeta"; "De Arrascaeta"])
    ]

let table_and_scorers lst = 
  let rec eachTeamStats l stats = 
    let organized =  List.map (fun (t1, s1, t2, s2) -> ((t1, s1),(t2, s2))) l
    in 
    let rec statisticalLine organized lineStats = 
      match organized with
    | [] -> lineStats
    | ((t1, s1), (t2,s2))::others -> 
      let whoWon = if List.length s1 > List.length s2 then 1 else if List.length s1 < List.length s2 then 2 else 0 in
      let firstScored = List.length s1 in
      let secondScored = List.length s2 in
      let goalsAgainst1 = List.length s2 in
      let goalsAgainst2 = List.length s1 in
      statisticalLine others (lineStats@[((t1, firstScored, goalsAgainst1),(t2, secondScored, goalsAgainst2), whoWon)])
     in 
     let readAbleStats = statisticalLine organized [] in
     let rec operateOnReadables stats calculated = match stats with
     | [] -> calculated
     | ((t1, gf1, ga1), (t2, gf2, ga2), whoWon)::others ->
        let rec compareToExisting1 existingStats t g w d l gf ga p = match existingStats with
        | [] -> (t,g,w,d,l,gf,ga,p) 
        | ((nt1, ngf1, nga1), (nt2, ngf2, nga2) ,nwhoWon)::xs -> 
        if t = nt1 then
        
        let won = if nwhoWon = 1 then 1 else 0 in
        let draws = if nwhoWon = 0 then 1 else 0 in
        let loses = if nwhoWon = 2 then 1 else 0 in
        let points = (if won = 1 then 3 else 0) + (if draws = 1 then 1 else 0) + (if loses = 1 then 0 else 0) in
          compareToExisting1 xs t (g+1) (w+won) (d+draws) (l+loses) (gf+ngf1) (ga+nga1) (p+points)
            else if t = nt2 then
            let won2 = if nwhoWon = 2 then 1 else 0 in
            let draws2 = if nwhoWon = 0 then 1 else 0 in
            let loses2 = if nwhoWon = 1 then 1 else 0 in
            let points2 = (if won2 = 1 then 3 else 0) + (if draws2 = 1 then 1 else 0) + (if loses2 = 1 then 0 else 0) in
              compareToExisting1 xs t (g+1) (w+won2) (d+draws2) (l+loses2) (gf+ngf2) (ga+nga2) (p+points2)
      else
        compareToExisting1 xs t g w d l gf ga p
        in
              let for1 = compareToExisting1 readAbleStats t1 0 0 0 0 0 0 0 
        in
              let for2 = compareToExisting1 readAbleStats t2 0 0 0 0 0 0 0
        in
             operateOnReadables others calculated@[for1]@[for2]       
     in 
     let needsFiltering = (operateOnReadables readAbleStats []) in
     let rec filterDuplicates lst filtered = 
      match lst with
      | [] -> filtered
      | x::xs -> 
        let rec loopOver sameList item = match sameList with
        | [] -> true
        | y::ys -> if item = y then false else loopOver ys item
        in 
        let isUnique = loopOver xs x
        in 
        if isUnique then filterDuplicates xs (filtered@[x]) else filterDuplicates xs filtered
        in
        let finalStats = filterDuplicates needsFiltering []
        in
        List.sort (fun (t,g,w,d,l,gf,ga,p) (t1,g1,w1,d1,l1,gf1,ga1,p1) -> 
              if p > p1 then -1 else if p < p1 then 1 else
              if (gf-ga) > (gf1-ga1) then -1 else if (gf-ga) < (gf1-ga1) then 1 else
              if gf > gf1 then -1 else if gf < gf1 then 1 else
              0
        ) finalStats
        in 
         let sortedStats = eachTeamStats lst [] in
        let rec scorers l players=
          match l with 
          | [] -> players
          | (team1, scorers1, team2, scorers2)::others -> 
            let mappedScorers1 = List.map (fun each -> (each, team1)) scorers1 in
            let mappedScorers2 = List.map (fun each -> (each, team2)) scorers2 in
            scorers others (players@[mappedScorers1]@[mappedScorers2]) in
            let res = List.filter (fun each ->  each != []) (scorers lst [])
            in
            let rec uniteInOneList lst newL = match lst with
            | [] -> newL
            | x::xs -> 
            let rec innerOne innerL outCome = match innerL with 
                | [] -> outCome
                | each::others -> innerOne others (outCome@[each])
                in uniteInOneList xs ((innerOne x [])@newL) in
            
         let sortedScorers = List.sort (
                  fun (name1, _, score1) (name2, _, score2) ->
                  if score1 > score2 then -1 
                  else if score1 < score2 then 1 
                  else if name1 < name2 then -1 
                  else if name1 > name2 then 1 else 0)
            (List.map (fun ((name, team), scored) -> (name, team, scored) ) (count_occurrences (uniteInOneList res [])))
        
      in (sortedStats, sortedScorers)



let testing_table_and_scorers () =
        let l =
          [
            __LINE_OF__ (table_and_scorers wc22_H =
                           ([(Por, 3, 2, 0, 1, 6, 4, 6);
                             (Kor, 3, 1, 1, 1, 4, 4, 4);
                             (Uru, 3, 1, 1, 1, 2, 2, 4);
                             (Gha, 3, 1, 0, 2, 5, 7, 3)],
                            [("Cho Gue-sung", Kor, 2);
                             ("De Arrascaeta", Uru, 2);
                             ("Fernandes", Por, 2);
                             ("Kudus", Gha, 2);
                             ("Ayew", Gha, 1);
                             ("Bukari", Gha, 1);
                             ("Felix", Por, 1);
                             ("Horta", Por, 1);
                             ("Hwang Hee-chan", Kor, 1);
                             ("Kim Young-gwon", Kor, 1);
                             ("Leao", Por, 1);
                             ("Ronaldo", Por, 1);
                             ("Salisu", Gha, 1)]));
            __LINE_OF__ (table_and_scorers wc22_C =
                           ([(Arg, 3, 2, 0, 1, 5, 2, 6);
                             (Pol, 3, 1, 1, 1, 2, 2, 4);
                             (Mex, 3, 1, 1, 1, 2, 3, 4);
                             (Sau, 3, 1, 0, 2, 3, 5, 3)],
                            [("Al-Dawsari", Sau, 2);
                             ("Messi", Arg, 2);
                             ("Al-Shehri", Sau, 1);
                             ("Alvarez", Arg, 1);
                             ("Chavez", Mex, 1);
                             ("Fernandez", Arg, 1);
                             ("Lewandowski", Pol, 1);
                             ("Mac Allister", Arg, 1);
                             ("Martin", Mex, 1);
                             ("Zielinski", Pol, 1)]))
          ] in
        let result = List.fold_left (&&) true (List.map snd l) in
        if result then (Printf.printf "The table_and_scorers test succeeds.\n"; [])
        else (Printf.printf "The table_and_scorers test fails.\n Check the corresponding line numbers in the list below.\n";
              (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
