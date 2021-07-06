type 'a graph = Gr of ('a * 'a * 'a) list;;

let grafo =  Gr [(1,3,2);(1,1,5);(2,2,3);(5,5,3);(5,4,6);(3,1,6);(3,7,4);(6,2,7);(4,4,6)];;

let listagrafo = [(1,3,2);(1,1,5);(2,2,3);(5,5,3);(5,4,6);(3,1,6);(3,7,4);(6,2,7);(4,4,6)];;



let succ (Gr arcs) n =
    let rec aux = function
         [] -> []
         | (x,y,z):: rest ->
            if n = x then
                z::(aux rest)
            else if n = z then
                x::(aux rest)
           else aux rest 
    in aux arcs
;;


(* get_single_weight (-1,3,2,listagrafo) restituisce il peso dell'arco che connette i nodi 3 e 2, nel grafo listagrafo *)
(* il -1 iniziale è un flag che stabilisce se finora ho incontrato o meno l'arco che mi interessa, bad cosa se ci sono archi con peso -1! *)
let rec get_single_weight = function
    (-1,n,m,[])-> -1
    |(-1,n,m,(a,w,b)::t) ->
        if ((n=a && m=b)||(n=b && m=a)) then
            get_single_weight(w,n,m,t)
        else
            get_single_weight(-1,n,m,t)
    |(s,n,m,(a,w,b)::t) -> s
;;






(* restituisce il cammino da start a stop *)
let bfs_w graph_w start stop=
    let rec search visited = function
        | [] -> raise Not_found
        | n :: rest ->
        if List.mem n visited
        then search visited rest
        else if n=stop then [n]
        else n:: search (n::visited) (succ graph_w n)
    in search [] [start]
;;

(* se ora ricostruisco una lista di triple con gli archi che mi interessano e sfrutto get_single_weight, ho fatto *)





(* se chiamo tupla_path [] (bfs_w grafo 3 7), restituisce le tuple corrispondenti ai nodi della lista ritornata da bfs_w grafo 3 7 *)
let rec tupla_path final_lista = function
    [] -> []
    |[x] -> final_lista
    |head::tail ->
        let a = List.hd tail
        in tupla_path ((head,a)::final_lista) tail;
;;


(* sfrutta get_single_weight per restituire il peso del cammino *)
let rec get_total_weight = function
    |([],k) -> k
    |((a,b)::tail,k) ->
        get_total_weight (tail,(k+get_single_weight(-1,a,b,listagrafo)))
;;

(* let peso_totale = get_total_weight (tupla_path [] (bfs_w grafo 3 7),0);; *)


(* controlla se il peso totale, ritornato da get_total_weight, supera il k dato *)
let control_peso peso_tot k =
    if peso_tot >= k then
        true
    else
        false
;;


(* restituisce true o false se il cammino da start a stop con peso almeno k esiste o no *)
let solution start stop k=
    if ((control_peso (get_total_weight (tupla_path [] (bfs_w grafo start stop),0)) k) = true) then
        Printf.printf ("Esiste un cammino dal nodo " ^^ "%d" ^^ " al nodo " ^^ "%d" ^^ " con peso al massimo " ^^ "%d\n") start stop k
    else
        Printf.printf "Non esiste nessun cammino con i parametri inseriti\n"
;;


solution (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(3));;