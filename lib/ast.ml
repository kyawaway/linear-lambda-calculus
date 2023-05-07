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

type toplevel = string * term

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

type context = (string * pretype) list

(* print *)
let print_context ctx : unit =
  let _ =
    List.map
      (fun (x, y) -> print_string ("(" ^ x ^ "," ^ type_of_string y ^ ")\n"))
      ctx
  in
  ()

let rec ast_of_string t =
  match t with
  | TmVar s -> "Var " ^ s
  | TmBoolean (q, b) -> qualifier_of_string q ^ " " ^ string_of_bool b
  | TmIf (t1, t2, t3) ->
      "If (" ^ ast_of_string t1 ^ ") (" ^ ast_of_string t2 ^ ") ("
      ^ ast_of_string t3 ^ ")"
  | TmPair (q, t1, t2) ->
      qualifier_of_string q ^ " (" ^ ast_of_string t1 ^ "," ^ ast_of_string t2
      ^ ")"
  | TmSplit (t1, x, y, t2) ->
      "Split (" ^ ast_of_string t1 ^ ") (" ^ x ^ "," ^ y ^ ") ("
      ^ ast_of_string t2 ^ ")"
  | TmAbs (q, n, tp, t1) ->
      qualifier_of_string q ^ " Abs (" ^ n ^ ") (" ^ type_of_string tp ^ ") ("
      ^ ast_of_string t1 ^ ")"
  | TmApp (t1, t2) ->
      "App (" ^ ast_of_string t1 ^ ") (" ^ ast_of_string t2 ^ ")"
