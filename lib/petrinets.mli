type place
type transition
type arc
type petri_net

(** [empty] is an empty petri net. *)
val empty : petri_net

(** [places net] returns the list of places in the petri net [net]. *)
val places : petri_net -> place list

(** [transitions net] returns the list of transitions in the petri net [net]. *)
val transitions : petri_net -> transition list

(** [arcs net] returns the list of arcs in the petri net [net]. *)
val arcs : petri_net -> arc list

(** [compare_places] compares two given places [p1] and [p2]. *)
val compare_places : place -> place -> int

(** [compare_transitions] compares two given transitions [t1] and [t2]. *)
val compare_transitions : transition -> transition -> int

(** [compare_arcs] compares two given arcs [a1] and [a2]. *)
val compare_arcs : arc -> arc -> int

(** [mk_place name] creates a place with the given [name]. *)
val mk_place : string -> place

(** [mk_transition name] creates a transition with the given [name]. *)
val mk_transition : string -> transition

(** [mk_outgoing_arc p t w] creates an arc from place [p] to transition [t] with weight [w]. *)
val mk_outgoing_arc : place -> transition -> int -> arc

(** [mk_incoming_arc t p w] creates an arc from transition [t] to place [p] with weight [w]. *)
val mk_incoming_arc : transition -> place -> int -> arc

(** [add_place p c t net] adds place [p] to the petri net [net]. 
    The place is initialized with capacity [c] and initial token count [t].
    If a place with the same name already exists, it is replaced. *)
val add_place : place -> int -> int -> petri_net -> petri_net

(** [add_transition t net] adds transition [t] to the petri net [net].
     If a transition with the same name already exists, it is replaced. *)
val add_transition : transition -> petri_net -> petri_net

(** [add_arc a net] adds arc [a] to the petri net [net]. 
    Returns None if the arc is not well defined in the petri net. *)
val add_arc : arc -> petri_net -> petri_net option

(** [is_enabled t net] checks if transition [t] is enabled in the petri net [net]. *)
val is_enabled : transition -> petri_net ->  bool

(** [marking_of_place p net] returns the number of tokens in place [p] in the petri net [net]. 
    Returns None if the place does not exist in the petri net. *)
val marking_of_place : place -> petri_net -> int option
  
(** [fire t net] fires the transition [t] in the petri net [net] if it is enabled, 
    returning the resulting petri net. Returns None if the transition does not exist or
    is not enabled. *)
val fire : transition -> petri_net -> petri_net option

(** [fire_all ts net] fires the transitions in the list [ts] in the petri net [net] in        
    sequence, returning the resulting petri net. Returns None if any of the transition cannot be fired. *)
val fire_all : transition list -> petri_net -> petri_net option

(** [is_stuck net] checks if the petri net [net] is stuck (if there are no enabled  
    transitions). *)
val is_stuck : petri_net -> bool 