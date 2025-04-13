signature PARSER_SIG =
sig
	include APPLICATIVE_SIG

	val create_parser : (string -> ('a * string) option) -> 'a t
	val run_parser    : 'a t -> string -> ('a * string) option
	val charP         : char -> char t
end

structure Parser :> PARSER_SIG =
struct

type 'a t = string -> ('a * string) option
type 'a parser = 'a t

(* FUNCTOR_SIG Definitions *)

(* "Penetrates" the parser and apply f to it's result.
 *
 * f : ('a -> 'b) -> 'a parser -> 'b parser *)
fun fmap f p = 
	(fn s =>
       case p s of
           NONE => NONE
		 | SOME (x, rest) => SOME (f x, rest))

infix 1 <$>
infix 1 <$
infix 1 $>

(* Infix Operator for fmap.
 *
 * f : 'a parser * ('a -> 'b)  -> 'b parser *)
fun op <$> (p, f) = fmap f p


(* Replaces 'b with 'a if parser succeds.
 *
 * f : 'a * 'b parser -> 'a parser *)
fun op <$ (a, p) = p <$> (fn _ => a)


(* Replaces 'a with 'b if parser succeds.
 *
 * f : 'a parser * 'b -> 'b parser *)
fun op $> (p, b) = b <$ p


(* APPLICATIVE_SIG Definitions *)

(* Returns a parser that ignores it's input and foward it as SOME (x, input).
 *
 * f : 'a -> 'a parser *)
fun pure x = fn input => SOME (x, input)

infix 1 <*>

fun op <*> p1 p2 =
	(fn s =>
		case p1 s of
			NONE => NONE
		  | SOME (f, s') => 
			(case p2 s' of
				 NONE => NONE
			   | SOME (a, s'') => SOME (f a, s'')))


(* PARSER_SIG Definitions *)

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
