signature PARSER_SIG =
sig
	include FUNCTOR_SIG
	include APPLICATIVE_SIG
	include ALTERNATIVE_SIG

	type position			  
	type parser_state

	datatype 'a parser_result =  
			 SUCCESS of 'a * parser_state
		   | FAILURE of string * parser_state
	
	type 'a parser = parser_state -> 'a parser_result

	(*val <<*          : 'a parser * 'b parser -> 'a parser
	val *>>          : 'a parser * 'b parser -> 'b parser*)

	val run_parser   : 'a parser -> string -> 'a parser_result
	val charP        : char ->  char parser
	val stringP      : string -> char list parser
	val not_null     : 'a list parser -> 'a list parser
	val parse_if     : (char -> bool) -> char parser
	val spanP        : (char -> bool) -> char list parser
	val natP         : int parser
	val intP         : int parser
	val doubleP      : real parser
	val hexP         : int parser
	val sepBy        : 'a parser -> 'b parser -> 'b list parser
	val replicate    : int -> 'a parser -> 'a parser list
end

structure Parser : PARSER_SIG =
struct

type position = int * int

type parser_state = {
	input: string,
	pos: position,
	full_input: string,
	line_index: int list
}

datatype 'a parser_result =  
		 SUCCESS of 'a * parser_state
	   | FAILURE of string * parser_state

type 'a parser = parser_state -> 'a parser_result
type 'a t = 'a parser

(* BASIC PARSER FUNCTIONS *)
(* Create a new state for a parser.
 *
 * f : string -> parser_state *)
fun create_state input = 
	let
		(* Return a list with the start index of all lines in a input.
		 *
		 * f : string -> int list -> int list *)
		fun create_line_index s acc =
			let
				val nl_index = CharVector.findi (fn (i, x) => x = #"\n") s
			in
				case nl_index of
					NONE => acc
				  | SOME (i, _) => 
					case acc of 
						x::xs => create_line_index 
									 (String.extract (s, i + 1, NONE))
									 ((i + 1 + x)::acc)
					  | [] => 0::[] (* Should never happen *)
			end
	in
		{
		  input = input,
		  pos = (0, 0),
		  full_input = input,
		  line_index = create_line_index input (0::[])
		}
	end

(* Returns the bounds for the current line based on the line_index.
 *
 * f : int list -> parser_state *)
fun get_line_index line_index line =
	let
		val len = (List.length line_index) - 1
		val i = len - line
	in
		if line = len
		then (NONE, SOME (List.nth (line_index, i))) 
		else (SOME (List.nth (line_index, i - 1)), 
			  SOME (List.nth (line_index, i))) 
	end

(* Returns a window (max 40 chr) of the line that the error ocurred concatenated with the caret pointing the exact error location.
 *
 * f : int -> int -> string -> int -> string *)
fun get_caret_line upper lower s column = 
	let
        val max_len = 40
        val line = String.extract (s, lower, SOME (upper - lower))
        val line_len = String.size line

        (* Compute window *)
        val left_pad = Int.max (0, column - (max_len div 2))
        val right_pad = Int.min (line_len, left_pad + max_len)

        (* Final substring *)
        val visible = String.extract (line, left_pad, SOME (right_pad - left_pad))

        (* Caret under the correct character *)
        val caret_pos = Int.max (0, 
								 Int.min (column - left_pad, 
										  String.size visible - 1))
        val caret_line = String.concat [
            String.implode (List.tabulate (caret_pos, fn _ => #" ")),
            "^"
        ]
    in
        "  " ^ visible ^ "\n  " ^ caret_line
    end
	

(* Returns a complete error message with the line, column, a window of characters pointing to the exact location fo the error in the string, and a custum error message from the parser.
 *
 * f : string -> parser_state -> string *)
fun error_msg msg {input = _, 
				   full_input = fi, 
				   line_index = li, 
				   pos = (line, column)} =
	let
		
		(* Default Header for error*)
		val loc_msg = "Error on line " ^ (Int.toString (line + 1)) ^ 
	", column " ^ (Int.toString (column + 1)) ^ ":\n"
								
		(* Get the lower and upper bound of the line in which the error ocurred *)
		val error_btw = get_line_index li line
		val (upper, lower) = 
			case error_btw of
				(SOME upper, SOME lower) => (upper - 1, lower)
			  | (NONE, SOME lower) => (String.size fi, lower)
			  | _ => (0, 0) (* Should Never Happen *) 

		val caret_line = get_caret_line upper lower fi column
	in
		loc_msg ^ caret_line ^ "\n" ^ msg ^ "\n\n"
	end

(* Run a 'a parser p with the string s.
 *
 * f : 'a parser -> string -> 'a parser_result *)
fun run_parser p s = 
	let
		val res = p (create_state s)
	in
		case res of
			FAILURE (msg, state) => (print (error_msg msg state); res)
		  | _ => res 
	end

(* INTERFACES DEFINITIONS *)

(* Infix Operators Orders *)
(* FUNCTOR_SIG Operators *)
infix 2 <$> <$ $>

(* APPLICATIVE_SIG Operators *)
infix 1 <*> *> <*
		
(* ALTERNATIVE_SIG Operators *)
infix 1 <|> <||>


(* FUNCTOR_SIG Definitions *)

(* "Penetrates" the type 'a t and apply f to it's value.
 *
 * f : ('a -> 'b) ->/* 'a parser -> 'b parser *)
fun fmap f p = 
	(fn state =>
		case p state of
			FAILURE x => FAILURE x
		  | SUCCESS (x, state') => SUCCESS (f x, state'))

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
fun pure x = fn state => SUCCESS (x, state)


(* Apply Operator, apply a function wrapped in context t to a value in a context t.
 *
 * f : ('a -> 'b) parser ->/* 'a parser -> 'b parser *)
fun apply p1 p2 = 
	(fn state =>
		case p1 state of
			FAILURE x => FAILURE x
		  | SUCCESS (f, state') =>
			(case p2 state' of
				 FAILURE x' => FAILURE x'
			   | SUCCESS (a, state'') => SUCCESS (f a, state'')))

fun op <*> (p1, p2) = apply p1 p2


(* Ignores the left value.
 *
 * f : 'a parser ->/* 'b parser -> 'a parser *)
fun leftsq p1 p2 = 
	(fn state =>
		case p1 state of
			FAILURE x => FAILURE x
		  | SUCCESS (a, state') =>
			(case p2 state' of
				 FAILURE x' => FAILURE x'
			   | SUCCESS (_, state'') => SUCCESS (a, state'')))
	
fun op <* (p1, p2) = leftsq p1 p2


(* Ignores the right value.
 *
 * f : 'a parser ->/* 'b parser -> 'b parser *)
fun rightsq p1 p2 = 
	(fn state =>
		case p1 state of
			FAILURE x => FAILURE x
		  | SUCCESS (_, state') => p2 state')

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


(* Attempt to run a parser p and return FAILURE if the input fails and parttialy consumed.
 *
 * f : 'a parser -> 'a option parser_result *)
fun attempt p =
  (fn state : parser_state =>
      case p state of
		  SUCCESS (x, state') => SUCCESS (SOME x, state')
		| FAILURE (msg, state') =>
          let
			  val consumed =
				  String.size (#input state) - String.size (#input state')
          in
			  if consumed = 0 
			  then SUCCESS (NONE, state')
			  else FAILURE (msg, state')
          end)


(* Matches type t zero or more times creating a list. Fails if p consumes partial input on a element.
 *
 * f : 'a parser -> 'a list parser *)
fun many p =
  (fn state =>
      case attempt p state of
		  SUCCESS (SOME v, state') =>
          (case many p state' of
			   SUCCESS (vs, state'') => SUCCESS (v :: vs, state'')
			 | FAILURE err => FAILURE err)
		| SUCCESS (NONE, state') => SUCCESS ([], state')
		| FAILURE err => FAILURE err)


(* Matches type t one or more times creating a list.
 *
 * f : 'a parser -> 'a list parser *)
fun some p = p <$> append <*> many p


(* ALTERNATIVE_SIG Operators *)

(* Empty value in a type t context.
 *
 * f : 'a parser *)
val empty = (fn state => FAILURE ("", state))

(* Or operator for comparing two values of type 'a t.
 *
 * f : 'a parser -> 'a parser -> 'a parser *)
fun op <|> (p1, p2) = 
	(fn state => 
		case p1 state of
			FAILURE _ => p2 state
		  | SUCCESS x => SUCCESS x)

(* Improved <|> operator that preserves the most relevant error message 
 * 
 * f : 'a parser -> 'a parser -> 'a parser *)
fun op <||> (p1, p2) = 
  (fn state : parser_state => 
      case p1 state of
		  SUCCESS x => SUCCESS x
		| FAILURE (msg1, state1 : parser_state) => 
          case p2 state of
			  SUCCESS x => SUCCESS x
			| FAILURE (msg2, state2 : parser_state) =>
              (* Choose the error from the parser that consumed more input *)
              let
				  val consumed1 = 
					  String.size 
						  (#full_input state) - String.size (#input state1)
				  val consumed2 = 
					  String.size 
						  (#full_input state) - String.size (#input state2)
            in
              if consumed1 > consumed2 
			  then FAILURE (msg1, state1)
              else FAILURE (msg2, state2)
            end)


(* Parser Structure Functions*)

(* Creates a list of length n of an 'a parser.
 *
 * f : int -> 'a parser -> 'a parser list*)
fun replicate n p =
	if n < 1
	then [empty]
	else
		if n = 1
		then [p]
		else p::(replicate (n- 1) p)

(* Parsers a char c if f c is true.
 *
 * f : (char -> bool) -> char parser *)
fun parse_if f =
	(fn state : parser_state =>
		if String.size (#input state) < 1
		then FAILURE ("Empty String.", state)
		else
			let
				val (line, column) = #pos state

				val s = #input state
				val (c, rest) = (String.sub(s, 0), 
								 String.extract(s, 1, NONE))

				val newPos = if c = #"\n" 
							 then (line + 1, 0) 
							 else (line, column + 1)
			in
				if not (f c)
				then FAILURE ("Invalid Character.", state)
				else 
					SUCCESS (c, {
								input = rest,
								full_input = #full_input state,
								line_index = #line_index state,
								pos = newPos
							})
			end)

(* Wrapper for Parse If Function.
 *
 * f : char -> char parser *)
fun charP c = parse_if (fn c' => c' = c)


(* Parse a sequence of char.
 *
 * f : string -> char list parser *)	
fun stringP s =
	(fn state =>
		let
			val expected = s
			val p = sequenceA (map charP (explode s))
		in
			case p state of
				SUCCESS res => SUCCESS res
			  | FAILURE (_, failed_state) => 
				FAILURE ("Expected \"" ^ expected ^ "\"", failed_state)
		end)

(* Checks if a Parser is not Null.
 *
 * f : 'a list parser -> 'a list parser *)	
fun not_null p =
	(fn state =>
		case p state of
			FAILURE x => FAILURE x
		  | SUCCESS (a, state') => 
			case a of
				[] => FAILURE ("Empty Result.", state')
			  | _ => SUCCESS (a, state'))

(* Consumes characters until f a is false.
 *
 * f : (char -> bool) -> char list parser *)
fun spanP f = many (parse_if f)

(* Parses a natural number x >= 0.
 *
 * f : int parser *)	
val natP =
	let
		val digits_to_int = valOf o Int.fromString o implode
						
		val digits = some (parse_if Char.isDigit)
		fun nat state = (digits <$> digits_to_int) 
							state
							handle overflow => FAILURE ("Integer Overflow.", state)
	in
		nat
	end

(* Parse a integer value x ∈ Z.
 *
 * f : int parser *)
val intP = 
	let
		val sign = ((~1) <$ charP #"-") <|> (pure 1)
		fun compose_int sign n = n * sign;
	in
		sign <$> compose_int (* (int -> int) parser *) 
			 <*> natP        (* int parser *)
	end


(* Parse a fp in all of it legal forms.
 *
 * f : real parser *)
val doubleP =
	let
		val digits = some (parse_if Char.isDigit)
		val digits_to_Real = valOf o Real.fromString o implode

		(* Append all parts of a real string together and converto to real value
		 * 
		 * f : char list -> char list -> char list -> char list -> real*)
		fun aux sign n dec exp = 
			digits_to_Real (List.concat (sign::n::dec::exp::[]))

		val sign_char = charP #"-" <|> charP #"+" <|> (pure #"+")
		val sign = (sign_char <$> append <*> (pure [])) <|> (pure [])
		val dec = (charP #"." <$> append <*> digits) <|> (pure [])
		
		val e = charP #"e" <|> charP #"E"
		val exp = (e <$> append <*> (sign_char <$> append <*> digits)) <|> (pure []) 
	in
		sign <$> aux
			 <*> digits
			 <*> dec
			 <*> exp
	end


(* Parses a Hexadecimal Number.
 *
 * f : int parser *)	
val hexP =
	(fn state =>
		let
			(* Accumulate hex values until the final result *)
			fun aux (d, acc) =
				let
					val d_ord = ord d
					val d_value = 
						if d_ord >= 48 andalso d_ord <= 57
						then d_ord - ord #"0"
						else 
							if d_ord >= 65 andalso d_ord <= 70
							then d_ord - ord #"A" + 10
							else d_ord - ord #"a" + 10
				in
					acc * 16 + d_value
				end

			fun digits_to_int ds =
				foldl aux 0 ds
		in
			((not_null (spanP Char.isHexDigit)) <$> digits_to_int) state 
			handle overflow => FAILURE ("Integer Overflow.", state)
		end)

(* Parsers n 'b type separated by a 'a separator. All elements must be of type 'b or else will return FAILURE.
 *
 * f : 'a parser -> 'b parser -> 'b list parser *)
fun sepBy sep element =
  (fn state =>
      case attempt element state of
		  SUCCESS (SOME v, state') =>
          (case many (sep *> element) state' of
			   SUCCESS (vs, state'') => SUCCESS (v::vs, state'')
			 | FAILURE err => FAILURE err)
		| SUCCESS (NONE, state') => SUCCESS ([], state')
		| FAILURE err => FAILURE err)

end

(*

fun aux v =
case v of
(JsonObject fields) => 
let
fun find (a, b) = if a = "name" then true else false
in
List.find find fields
end
| _ => NONE;

*)
