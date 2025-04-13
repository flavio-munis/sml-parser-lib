signature PARSER_SIG =
sig
	include FUNCTOR_SIG
	include APPLICATIVE_SIG

	val create_parser : (string -> ('a * string) option) -> 'a t
	val run_parser : 'a t -> string -> ('a * string) option
	val charP : char -> char t
end

structure Parser :> PARSER_SIG =
struct
type 'a t = string -> ('a * string) option
type 'a parser = 'a t

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

(* "Penetrates" the parser and apply f to it's result.
 *
 * f : ('a -> 'b) -> 'a parser -> 'b parser *)
fun fmap f p = 
	(fn s =>
       case p s of
           NONE => NONE
		 | SOME (x, rest) => SOME (f x, rest))


(* Returns a parser that ignores it's input and foward it as SOME (x, input).
 *
 * f : 'a -> 'a parser *)
fun pure x = fn input => SOME (x, input)
	
end
