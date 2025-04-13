signature PARSER_SIG =
sig
	type 'a parser
	val create_parser : (string -> ('a * string) option) -> 'a parser
	val run_parser : 'a parser -> string -> ('a * string) option
	val charP : char -> char parser
end

functor Parser_Functor() :> PARSER_SIG =
struct
type 'a parser = string -> ('a * string) option

(* Wrapper for Constructing parser types.
 *
 * f : (string -> ('a * string) option) -> 'a parser *)
fun create_parser p = p


(* Run a 'a parser p with the string s.
 *
 * f : 'a parser -> string -> 'a * string option *)
fun run_parser p s = p s
	

(* Creates a closure for parsing the char c.
 *
 * f : char -> char parser *)
fun charP c = (fn s => 
				  if String.size s < 1
				  then NONE
				  else
					  let
						  val (s', xs) = (String.sub(s, 0), String.extract(s, 1, NONE))
					  in
						  if s' = c 
						  then SOME (c, xs) 
						  else NONE
					  end)
end

structure Parser = Parser_Functor()
