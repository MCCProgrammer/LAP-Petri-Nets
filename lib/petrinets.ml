open Utils

(* Definições de tipos para componentes da Rede de Petri *)
type place = string                    (* Um lugar é representado por um identificador string *)
type transition = string               (* Uma transição é representada por um identificador string *)
type arc = {                          (* Um arco conecta lugares e transições *)
  source: place;                      (* Nó de origem (lugar ou transição) *)
  target: transition;                 (* Nó de destino (lugar ou transição) *)
  weight: int;                        (* Número de tokens que fluem através deste arco *)
  is_incoming: bool;                  (* Verdadeiro se o arco vai da transição para o lugar, falso caso contrário *)
}

type place_info = {                   (* Informações sobre um lugar *)
  capacity: int;                      (* Número máximo de tokens que o lugar pode conter *)
  tokens: int;                        (* Número atual de tokens no lugar *)
}

type petri_net = {                    (* Estrutura completa da Rede de Petri *)
  places: (place * place_info) list;  (* Lista de lugares com suas informações *)
  transitions: transition list;        (* Lista de transições *)
  arcs: arc list;                     (* Lista de arcos,ligação entre lugares e transições *)
}

(* Cria uma Rede de Petri vazia *)
let empty = {
  places = [];
  transitions = [];
  arcs = [];
}

(* Funções auxiliares para criar lugares e transições *)
let mk_place name = name              (* Cria um lugar com o nome dado *)
let mk_transition name = name         (* Cria uma transição com o nome dado *)

(* Funções para criar arcos *)
let mk_outgoing_arc p t w = {        (* Cria um arco do lugar para a transição *)
  source = p;
  target = t;
  weight = w;
  is_incoming = false;
}

let mk_incoming_arc t p w = {        (* Cria um arco da transição para o lugar *)
  source = t;
  target = p;
  weight = w;
  is_incoming = true;
}

(* Adiciona um lugar à Rede de Petri *)
let add_place p c t net = {
  places = (p, {capacity = c; tokens = t}) :: List.filter (fun (p', _) -> p' <> p) net.places;
  transitions = net.transitions;
  arcs = net.arcs;
}

(* Adiciona uma transição à Rede de Petri *)
let add_transition t net = {
  places = net.places;
  transitions = t :: List.filter (fun t' -> t' <> t) net.transitions;
  arcs = net.arcs;
}

(* Adiciona um arco à Rede de Petri se for válido *)
let add_arc a net =
  let place_exists p = List.exists (fun (p', _) -> p' = p) net.places in
  let transition_exists t = List.exists (fun t' -> t' = t) net.transitions in
  if a.is_incoming then
    if transition_exists a.source && place_exists a.target then
      Some {net with arcs = a :: net.arcs}
    else
      None
  else
    if place_exists a.source && transition_exists a.target then
      Some {net with arcs = a :: net.arcs}
    else
      None

(* Obtém o número de tokens em um lugar *)
let marking_of_place p net =
  try
    let (_, info) = List.find (fun (p', _) -> p' = p) net.places in
    Some info.tokens
  with Not_found -> None

(* Obtém todos os lugares na rede *)
let places net = List.map fst net.places

(* Obtém todas as transições na rede *)
let transitions net = net.transitions

(* Obtém todos os arcos na rede *)
let arcs net = net.arcs

(* Funções de comparação para lugares, transições e arcos *)
let compare_places p1 p2 = String.compare p1 p2
let compare_transitions t1 t2 = String.compare t1 t2
let compare_arcs a1 a2 =
  match String.compare a1.source a2.source with
  | 0 -> (match String.compare a1.target a2.target with
         | 0 -> (match compare a1.weight a2.weight with
                | 0 -> compare a1.is_incoming a2.is_incoming
                | n -> n)
         | n -> n)
  | n -> n

(* Verifica se uma transição está habilitada (pode disparar) *)
let is_enabled t net =
  let outgoing_arcs = List.filter (fun a -> not a.is_incoming && a.target = t) net.arcs in
  let incoming_arcs = List.filter (fun a -> a.is_incoming && a.source = t) net.arcs in
  
  let has_enough_tokens a =
    match marking_of_place a.source net with
    | Some tokens -> tokens >= a.weight
    | None -> false
  in
  
  let has_enough_capacity a =
    match List.find_opt (fun (p, _) -> p = a.target) net.places with
    | Some (_, info) ->
        let current_tokens = info.tokens in
        let new_tokens = current_tokens + a.weight in
        new_tokens <= info.capacity
    | None -> false
  in
  
  if outgoing_arcs = [] || incoming_arcs = [] then
    false
  else
    List.for_all has_enough_tokens outgoing_arcs &&
    List.for_all has_enough_capacity incoming_arcs

(* Atualiza o número de tokens em um lugar *)
let update_place_tokens places p tokens =
  List.map (fun (p', info) ->
    if p' = p then (p', {info with tokens = tokens})
    else (p', info)
  ) places

(* Dispara uma transição se estiver habilitada *)
let fire t net =
  let outgoing_arcs = List.filter (fun a -> not a.is_incoming && a.target = t) net.arcs in
  let incoming_arcs = List.filter (fun a -> a.is_incoming && a.source = t) net.arcs in
  
  if outgoing_arcs = [] || incoming_arcs = [] then
    None
  else if not (is_enabled t net) then
    None
  else
    let places_after_consumption =
      List.fold_left (fun places a ->
        match marking_of_place a.source {net with places} with
        | Some tokens -> update_place_tokens places a.source (tokens - a.weight)
        | None -> places
      ) net.places outgoing_arcs
    in
    
    let places_after_production =
      List.fold_left (fun places a ->
        match marking_of_place a.target {net with places} with
        | Some tokens -> update_place_tokens places a.target (tokens + a.weight)
        | None -> places
      ) places_after_consumption incoming_arcs
    in
    
    Some {net with places = places_after_production}

(* Dispara múltiplas transições em sequência *)
let fire_all ts net =
  let rec fire_all_aux ts net =
    match ts with
    | [] -> Some net
    | t :: rest ->
        match fire t net with
        | None -> None
        | Some net' -> fire_all_aux rest net'
  in
  fire_all_aux ts net

(* Verifica se a rede está em estado de deadlock (sem transições habilitadas) *)
let is_stuck net =
  not (List.exists (fun t -> is_enabled t net) net.transitions)
