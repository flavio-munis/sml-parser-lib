signature PARSER_SIG =
sig
	include FUNCTOR_SIG
	include APPLICATIVE_SIG
	include ALTERNATIVE_SIG

	type 'a parser

	val create_parser : (string -> ('a * string) option) -> 'a parser
	val run_parser    : 'a parser -> string -> ('a * string) option
	val charP         : char -> char parser
	val stringP       : string -> char list parser
	val not_null      : 'a list parser -> 'a list parser
	val spanP         : (char -> bool) -> char list parser
	val natP          : int parser
end

structure Parser : PARSER_SIG =
struct

(*open Option_Ext*)
type 'a parser = string -> ('a * string) option
type 'a t = 'a parser


(* Infix Operators Orders *)
(* FUNCTOR_SIG Operators *)
infix 2 <$> <$ $>

(* APPLICATIVE_SIG Operators *)
infix 1 <*> *> <*

(* MONAD_SIG Operators *)
infix 1 >=> >>=

(* ALTERNATIVE_SIG Operators *)
infix 1 <|>


(* ALTERNATIVE_SIG Operators *)

(* Empty value in a type t context.
 *
 * f : unit -> 'a parser *)
fun empty () = (fn s => NONE)

(* Or operator for comparing two values of type 'a t.
 *
 * f : 'a parser -> 'a parser -> 'a parser *)
fun op <|> (p1, p2) = 
	(fn s => 
		case (p1 s) of
			NONE => (p2 s)
		  | SOME (s', rest) => SOME (s', rest))


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
	(fn s : string =>
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
fun leftsq p1 p2 = 
	(fn s =>
		case p1 s of
			NONE => NONE
		  | SOME (a, s') => 
			(case p2 s' of
				 NONE => NONE
			   | SOME (_, s'') => SOME (a, s'')))
	
fun op <* (p1, p2) = leftsq p1 p2

(* Ignores the right value.
 *
 * f : 'a parser ->/* 'b parser -> 'b parser *)
fun rightsq p1 p2 = 
	(fn s =>
		case p1 s of
			NONE => NONE
		  | SOME (_, s') => p2 s')

fun op *> (p1, p2) = rightsq p1 p2


(* Elevate a function f to a context and apply both elements.
 *
 * f : ('a -> 'b -> 'c) -> 'a parser -> 'b parser -> 'c parser *)
fun liftA2 f p1 p2 = apply (apply (pure f) p1) p2


(* Helper function to append to a list. (until List_Ext is made).
 *
 * f : 'a parser -> ('a -> b' parser) -> 'b parser *)
fun append x y = x::y

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


(* Checks if a Parser is not Null.
 *
 * f : 'a list parser -> 'a list parser *)	
fun not_null p =
	(fn s =>
		case p s of
			NONE => NONE
		  | SOME (s', rest) => 
			case s' of
				[] => NONE
			  | _ => SOME (s', rest))


(* Consumes characters until f a is false.
 *
 * f : (char -> bool) -> char list parser *)	
fun spanP f =
	(fn s =>
		let
			val len = String.size s

			(* Accumulate characters one by one *)
			fun loop i =
				if i >= len 
				then SOME (String.extract (s, 0, SOME len), "") 
				else
					let 
						val c = String.sub(s, i) 
					in
						if f c 
						then loop i + 1
						else SOME (String.extract (s, 0, SOME i),
								   String.extract (s, i, SOME (len - i)))
					end
		in
			loop 0
		end)

(* Parses a natural number x >= 0.
 *
 * f : unit -> int parser *)	
val natP =
    let
        fun digitsToInt ds =
            foldl (fn (d, acc) => acc * 10 + (ord d - ord #"0")) 0 ds
    in
        (not_null (spanP Char.isDigit)) <$> digitsToInt
    end


(* Matches type t zero or more times creating a list.
 *
 * f : 'a parser -> 'a list parser *)
fun many p =
    let 
        fun step input =
            case p input of
                NONE => SOME ([], input)
              | SOME (x, rest) =>
                case step rest of
                    NONE => SOME ([x], rest)
                  | SOME (xs, rest') => SOME (x::xs, rest')
    in
        create_parser step
    end

(* Matches type t one or more times creating a list.
 *
 * f : 'a parser -> 'a list parser *)
fun some p = p <$> append <*> many p

end
