module StringBase : sig type t = string val compare : 'a -> 'a -> int end
module StringSet :
  sig
    type elt = StringBase.t
    and t = Mc_set.McMake(StringBase).t
    val empty : t
    val is_empty : t -> bool
    val mem : t -> elt -> bool
    val add : t -> elt -> t
    val singleton : elt -> t
    val remove : t -> elt -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val add_list : t -> elt list -> t
    val subtract_list : t -> elt list -> t
    val of_list : elt list -> t
    val to_list : t -> elt list
  end
module StringTable :
  sig
    type key = StringBase.t
    and 'a t = 'a Mc_map.McMake(StringBase).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val forall : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val isect_mem : 'a t -> (key -> bool) -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
    val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
    val keys : 'a t -> key list
    val data : 'a t -> 'a list
  end
module IntBase : sig type t = int val compare : 'a -> 'a -> int end
module IntSet :
  sig
    type elt = IntBase.t
    and t = Mc_set.McMake(IntBase).t
    val empty : t
    val is_empty : t -> bool
    val mem : t -> elt -> bool
    val add : t -> elt -> t
    val singleton : elt -> t
    val remove : t -> elt -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val add_list : t -> elt list -> t
    val subtract_list : t -> elt list -> t
    val of_list : elt list -> t
    val to_list : t -> elt list
  end
module IntTable :
  sig
    type key = IntBase.t
    and 'a t = 'a Mc_map.McMake(IntBase).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val forall : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val isect_mem : 'a t -> (key -> bool) -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
    val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
    val keys : 'a t -> key list
    val data : 'a t -> 'a list
  end
module IntMTable :
  sig
    type key = IntBase.t
    and 'a t = 'a Mc_map.McMakeList(IntBase).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val forall : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val isect_mem : 'a t -> (key -> bool) -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
    val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
    val keys : 'a t -> key list
    val data : 'a t -> 'a list
    val filter : 'a t -> key -> ('a list -> 'a list) -> 'a t
    val find_all : 'a t -> key -> 'a list
    val iter_all : (key -> 'a list -> unit) -> 'a t -> unit
    val mapi_all : (key -> 'a list -> 'b list) -> 'a t -> 'b t
    val fold_all : ('a -> key -> 'b list -> 'a) -> 'a -> 'b t -> 'a
    val data_all : 'a t -> 'a list list
  end
type pos = string * int * int * int * int
and id = string * pos
and token_option =
    Token_extend of id list option
  | Token_remove of (string * pos) list
  | Token_override of id list
and term_option = Term_extend of string * (id * int) list
and loption =
    Lo_longest
  | Lo_first
  | Lo_naml
  | Lo_fc
  | Lo_pasqual
  | Lo_pascal
and directive =
    Dir_nonassoc of id list
  | Dir_leftassoc of id list
  | Dir_rightassoc of id list
and assoc = NonAssoc | LeftAssoc | RightAssoc
and goption = Go_start of string | Go_unknown
exception ParseError of pos * string
type cregexp = bool * string * Str.regexp
and multi_regexp = bool * string * string list
and psymbol = NonTerminal of string | Terminal of string | Empty | Eof
val compare_strings : 'a -> 'a -> int
val psymbol_compare : psymbol -> psymbol -> int
val psymbol_list_compare : psymbol list -> psymbol list -> int
module PSymbolBase :
  sig type t = psymbol val compare : psymbol -> psymbol -> int end
module PSymbolSet :
  sig
    type elt = PSymbolBase.t
    and t = Mc_set.McMake(PSymbolBase).t
    val empty : t
    val is_empty : t -> bool
    val mem : t -> elt -> bool
    val add : t -> elt -> t
    val singleton : elt -> t
    val remove : t -> elt -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val add_list : t -> elt list -> t
    val subtract_list : t -> elt list -> t
    val of_list : elt list -> t
    val to_list : t -> elt list
  end
module PSymbolTable :
  sig
    type key = PSymbolBase.t
    and 'a t = 'a Mc_map.McMake(PSymbolBase).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val forall : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val isect_mem : 'a t -> (key -> bool) -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
    val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
    val keys : 'a t -> key list
    val data : 'a t -> 'a list
  end
module PSymbolMTable :
  sig
    type key = PSymbolBase.t
    and 'a t = 'a Mc_map.McMakeList(PSymbolBase).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val forall : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val isect_mem : 'a t -> (key -> bool) -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
    val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
    val keys : 'a t -> key list
    val data : 'a t -> 'a list
    val filter : 'a t -> key -> ('a list -> 'a list) -> 'a t
    val find_all : 'a t -> key -> 'a list
    val iter_all : (key -> 'a list -> unit) -> 'a t -> unit
    val mapi_all : (key -> 'a list -> 'b list) -> 'a t -> 'b t
    val fold_all : ('a -> key -> 'b list -> 'a) -> 'a -> 'b t -> 'a
    val data_all : 'a t -> 'a list list
  end
val psymbol_set_compare : PSymbolSet.t -> PSymbolSet.t -> int
type mp_pre_term = Refiner.Refiner.Term.term * pos
and mp_term = Refiner.Refiner.Term.term
and mp_pre_rewrite = mp_pre_term list * mp_pre_term
and mp_pre_term_rewrite = mp_pre_term * mp_pre_term
and mp_rewrite = Refiner.Refiner.Rewrite.rewrite_rule
and pre_rule = id * (id list * id option * mp_pre_rewrite list)
and rule = psymbol * pos * (psymbol * pos) list * id option *
           mp_pre_rewrite list
and pre_grammar = pre_rule list
and grammar = rule list
and grammar_table = psymbol list PSymbolMTable.t
and production_id = int
val production_id_compare : int -> int -> int
type production = psymbol * psymbol list
val production_compare :
  psymbol * psymbol list -> psymbol * psymbol list -> int
module ProductionBase :
  sig
    type t = production
    val compare : psymbol * psymbol list -> psymbol * psymbol list -> int
  end
module ProductionIdBase :
  sig type t = production_id val compare : int -> int -> int end
module ProductionTable :
  sig
    type key = ProductionBase.t
    and 'a t = 'a Mc_map.McMake(ProductionBase).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val forall : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val isect_mem : 'a t -> (key -> bool) -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
    val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
    val keys : 'a t -> key list
    val data : 'a t -> 'a list
  end
module ProductionIdTable :
  sig
    type key = ProductionIdBase.t
    and 'a t = 'a Mc_map.McMake(ProductionIdBase).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val forall : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val isect_mem : 'a t -> (key -> bool) -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
    val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
    val keys : 'a t -> key list
    val data : 'a t -> 'a list
  end
module ProductionIdMTable :
  sig
    type key = ProductionIdBase.t
    and 'a t = 'a Mc_map.McMakeList(ProductionIdBase).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val forall : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val isect_mem : 'a t -> (key -> bool) -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
    val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
    val keys : 'a t -> key list
    val data : 'a t -> 'a list
    val filter : 'a t -> key -> ('a list -> 'a list) -> 'a t
    val find_all : 'a t -> key -> 'a list
    val iter_all : (key -> 'a list -> unit) -> 'a t -> unit
    val mapi_all : (key -> 'a list -> 'b list) -> 'a t -> 'b t
    val fold_all : ('a -> key -> 'b list -> 'a) -> 'a -> 'b t -> 'a
    val data_all : 'a t -> 'a list list
  end
type production_table = production_id ProductionTable.t
and production_id_table = (psymbol * psymbol list * string option)
                          ProductionIdTable.t
and rewrite_table = mp_pre_rewrite ProductionIdMTable.t
and crewrite_table = mp_rewrite ProductionIdMTable.t
and lexer_rewrite_table = mp_pre_rewrite PSymbolMTable.t
and lexer_crewrite_table = mp_rewrite PSymbolMTable.t
and crewrites = {
  rw_lexer : lexer_crewrite_table;
  rw_parser : crewrite_table;
} 
and item = production_id * int
val item_compare : int * int -> int * int -> int
module ItemBase :
  sig type t = item val compare : int * int -> int * int -> int end
type comp_item = int
val item_of_comp_item : int -> int * int
val comp_item_of_item : int * int -> int
val comp_item_compare : int -> int -> int
exception IntSetCompareResult of int
val int_set_compare : IntSet.t -> IntSet.t -> int
module IntSetBase :
  sig type t = IntSet.t val compare : IntSet.t -> IntSet.t -> int end
module IntSetMap :
  sig
    type key = IntSetBase.t
    and 'a t = 'a Mc_map.McMake(IntSetBase).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val forall : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val isect_mem : 'a t -> (key -> bool) -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
    val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
    val keys : 'a t -> key list
    val data : 'a t -> 'a list
  end
module Parser_state :
  sig
    type key = ItemBase.t
    and 'a t = 'a Mc_map.McMake(ItemBase).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val forall : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val isect_mem : 'a t -> (key -> bool) -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
    val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
    val keys : 'a t -> key list
    val data : 'a t -> 'a list
  end
type parser_state = PSymbolSet.t Parser_state.t
and state_struct = { state_map : parser_state; state_cache : IntSet.t; } 
and state_list = parser_state list
and state_list_struct = {
  states_list : state_struct list;
  states_cache : int IntSetMap.t;
} 
and state_with_id_list_struct = (state_struct * int) list
and state_id = int
and action_edge = state_id * state_id * psymbol
val action_edge_compare : 'a * 'b * psymbol -> 'a * 'b * psymbol -> int
module ActionEdgeBase :
  sig
    type t = action_edge
    val compare : 'a * 'b * psymbol -> 'a * 'b * psymbol -> int
  end
module Action_edges :
  sig
    type elt = ActionEdgeBase.t
    and t = Mc_set.McMake(ActionEdgeBase).t
    val empty : t
    val is_empty : t -> bool
    val mem : t -> elt -> bool
    val add : t -> elt -> t
    val singleton : elt -> t
    val remove : t -> elt -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val add_list : t -> elt list -> t
    val subtract_list : t -> elt list -> t
    val of_list : elt list -> t
    val to_list : t -> elt list
  end
module AcceptBase : sig type t = state_id val compare : 'a -> 'a -> int end
module Accepts :
  sig
    type elt = AcceptBase.t
    and t = Mc_set.McMake(AcceptBase).t
    val empty : t
    val is_empty : t -> bool
    val mem : t -> elt -> bool
    val add : t -> elt -> t
    val singleton : elt -> t
    val remove : t -> elt -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val add_list : t -> elt list -> t
    val subtract_list : t -> elt list -> t
    val of_list : elt list -> t
    val to_list : t -> elt list
  end
type token_info = id * token_option list
and grammar_state = {
  grammar_nonterminals : StringSet.t;
  grammar_terminals : StringSet.t;
  grammar_assocs : directive list;
  grammar_token_rules :
    (bool * token_info * string * mp_pre_rewrite list) list;
  grammar_start_symbol : psymbol;
  grammar_grammar : grammar;
  grammar_termsets : term_option list list;
  grammar_local_rewrites : mp_pre_term_rewrite list;
  grammar_post_rewrites : mp_pre_term_rewrite list list;
  grammar_inline_forms : mp_pre_term list;
} 
and lexer_env = {
  lexer_regexps : multi_regexp list;
  lexer_options : loption list;
  lexer_rewrites : lexer_rewrite_table;
} 
and clexer_env = {
  clexer_regexps : cregexp list;
  clexer_options : loption list;
  clexer_rewrites : lexer_rewrite_table;
} 
and parser_env = {
  parser_module : string;
  parser_grammar : grammar_table;
  parser_prod_ids : production_id_table;
  parser_prods : production_table;
  parser_nullables : PSymbolSet.t;
  parser_first_set : PSymbolSet.t PSymbolTable.t;
  parser_follow_set : PSymbolSet.t PSymbolTable.t;
  parser_rewrites : rewrite_table;
} 
and ploc = state_id * psymbol
val ploc_compare : 'a * psymbol -> 'a * psymbol -> int
module FABase :
  sig type t = ploc val compare : 'a * psymbol -> 'a * psymbol -> int end
module ParserFA :
  sig
    type key = FABase.t
    and 'a t = 'a Mc_map.McMakeList(FABase).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val forall : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val isect_mem : 'a t -> (key -> bool) -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
    val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
    val keys : 'a t -> key list
    val data : 'a t -> 'a list
    val filter : 'a t -> key -> ('a list -> 'a list) -> 'a t
    val find_all : 'a t -> key -> 'a list
    val iter_all : (key -> 'a list -> unit) -> 'a t -> unit
    val mapi_all : (key -> 'a list -> 'b list) -> 'a t -> 'b t
    val fold_all : ('a -> key -> 'b list -> 'a) -> 'a -> 'b t -> 'a
    val data_all : 'a t -> 'a list list
  end
type pentry =
    Shift of state_id
  | Goto of state_id
  | Reduce of production_id
  | Accept
  | Error
and parsing_table = pentry ParserFA.t
and parsing_table_error = ploc list
and stack_entry = Sta_state of state_id | Sta_term of mp_term
and stack = stack_entry Stack.t
and phobos_parser_return_type = {
  phobos_module_name : string;
  phobos_includes : string list;
  phobos_lexer_info :
    (bool * token_info * string * mp_pre_rewrite list) list * loption list;
  phobos_assoc_info : directive list;
  phobos_grammar_info : pre_rule list * goption list;
  phobos_termsets : term_option list list;
  phobos_local_rewrites : mp_pre_term_rewrite list;
  phobos_post_rewrites : mp_pre_term_rewrite list list;
  phobos_inline_forms : mp_pre_term list;
} 
and source = (psymbol * string * pos) list
