signature PARSER_SIG =
sig
	include APPLICATIVE_SIG

	val create_parser : (string -> ('a * string) option) -> 'a t
	val run_parser    : 'a t -> string -> ('a * string) option
	val charP         : char -> char t
	val stringP       : string -> char list t
end

structure Parser :> PARSER_SIG =
struct

(*open Option_Ext*)

type 'a t = string -> ('a * string) option
type 'a parser = 'a t


(* Infix Operators Orders *)
(* FUNCTOR_SIG Operators *)
infix 2 <$>
infix 2 <$
infix 2 $>

(* APPLICATIVE_SIG Operators *)
infix 1 <*>
infix 1 *>
infix 1 <*

(* MONAD_SIG Operators *)
infix 1 >=>
infix 1 >>=


(* FUNCTOR_SIG Definitions *)

(* "Penetrates" the type 'a t and apply f to it's value.
 *
 * f : ('a -> 'b) ->/* 'a parser -> 'b parser *)
fun fmap f p = 
	(fn s =>
       case p s of
           NONE => NONE
		 | SOME (x, rest) => SOME (f x, rest))

fun op <$> (p, f) = fmap f p


(* "Penetrates" type 'b t and change it to 'a t without losing context.
 *
 * f : 'a ->/* 'b parser -> 'a parser *)
fun rplc_left a p = p <$> (fn _ => a)
fun op <$ (a, p) = rplc_left a p

(* "Penetrates" type 'a t and change it to 'b t without losing context.
 *
 * f : 'a parser ->/* 'b -> 'b parser *)
fun rplc_right p b = b <$ p
fun op $> (p, b) = rplc_right p b


(* APPLICATIVE_SIG Definitions *)

(* Returns a parser that ignores it's input and foward it as SOME (x, input).
 *
 * f : 'a -> 'a parser *)
fun pure x = fn input => SOME (x, input)


(* Apply Operator, apply a function wrapped in context t to a value in a context t.
 *
 * f : ('a -> 'b) parser ->/* 'a parser -> 'b parser *)
fun apply p1 p2 = 
	(fn s =>
		case p1 s of
			NONE => NONE
		  | SOME (f, s') => 
			(case p2 s' of
				 NONE => NONE
			   | SOME (a, s'') => SOME (f a, s'')))

fun op <*> (p1, p2) = apply p1 p2

(* Ignores the left value.
 *
 * f : 'a parser ->/* 'b parser -> 'a parser *)
fun leftsq p1 p2 = p1
fun op <* (p1, p2) = p1

(* Ignores the right value.
 *
 * f : 'a parser ->/* 'b parser -> 'b parser *)
fun rightsq p1 p2 = p2
fun op *> (p1, p2) = p2

(* Helper function to append to a list. (until List_Ext is made).
 *
 * f : 'a parser -> ('a -> b' parser) -> 'b parser *)
fun append x y = x::y;

(* Turns a type inside out.
 *
 * f : 'a parser list -> 'a list parser *)
fun sequenceA ps = 
	case ps of
		[] => pure []
	  | x::xs' => x <$> append <*> (sequenceA xs')


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

(* Parse a sequence of char.
 *
 * f : string -> char list parser *)	
fun stringP s = sequenceA (map charP (explode s))

end
