(** Ast **)
type qualifier = Lin | Un

type pretype =
  | TyBool of qualifier
  | TyPair of qualifier * pretype * pretype
  | TyFunc of qualifier * pretype * pretype

type term =
  | TmVar of string
  | TmBoolean of qualifier * bool
  | TmIf of term * term * term
  | TmPair of qualifier * term * term
  | TmSplit of term * string * string * term
  | TmAbs of qualifier * string * pretype * term
  | TmApp of term * term

type toplevel = (string * term) list

let qualifier_of_string q = match q with Lin -> "lin" | Un -> "un"

let rec type_of_string t =
  match t with
  | TyBool q -> qualifier_of_string q ^ " Bool"
  | TyPair (q, t1, t2) ->
      qualifier_of_string q ^ " (" ^ type_of_string t1 ^ "," ^ type_of_string t2
      ^ ")"
  | TyFunc (q, t1, t2) ->
      qualifier_of_string q ^ " (" ^ type_of_string t1 ^ "->"
      ^ type_of_string t2 ^ ")"

let rec print_ast t =
  match t with
  | TmVar s -> "Var " ^ s
  | TmBoolean (q, b) -> qualifier_of_string q ^ " " ^ string_of_bool b
  | TmIf (t1, t2, t3) ->
      "If (" ^ print_ast t1 ^ ") (" ^ print_ast t2 ^ ") (" ^ print_ast t3 ^ ")"
  | TmPair (q, t1, t2) ->
      qualifier_of_string q ^ " (" ^ print_ast t1 ^ "," ^ print_ast t2 ^ ")"
  | TmSplit (t1, x, y, t2) ->
      "Split (" ^ print_ast t1 ^ ") (" ^ x ^ "," ^ y ^ ") (" ^ print_ast t2
      ^ ")"
  | TmAbs (q, n, tp, t1) ->
      qualifier_of_string q ^ " Abs (" ^ n ^ ") (" ^ type_of_string tp ^ ") ("
      ^ print_ast t1 ^ ")"
  | TmApp (t1, t2) -> "App (" ^ print_ast t1 ^ ") (" ^ print_ast t2 ^ ")"
