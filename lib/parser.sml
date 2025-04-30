signature PARSER_SIG =
sig
	(* Interfaces *)
	include FUNCTOR_SIG
	include APPLICATIVE_SIG
	include ALTERNATIVE_SIG

	(* Types *)
	type position			  
	type parser_state

	datatype 'a parser_result =  
			 SUCCESS of 'a * parser_state
		   | FAILURE of string * parser_state
	
	type 'a parser = parser_state -> 'a parser_result

	(* Run a Parser With Error Tracking *)
	val run_parser   : 'a parser -> string -> 'a parser_result
	
	(* Char and String Parsers *)
	val parse_if     : (char -> bool) -> char parser
	val charP        : char ->  char parser
	val stringP      : string -> char list parser
	val spanP        : (char -> bool) -> char list parser
	
	(* Parsing Numbers *)
	val natP         : int parser
	val intP         : int parser
	val doubleP      : real parser
	val hexP         : int parser
	val binP         : int parser

	(* Auxialiary Functions*)
	val not_null     : 'a list parser -> 'a list parser
	val sepBy        : 'a parser -> 'b parser -> 'b list parser
	val all_input    : 'a parser -> 'a parser
end

structure Parser : PARSER_SIG =
struct
local
	open Utils
in

(* 
 * PARSER_SIG Type Definitions
 *)

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


(* 
 * PARSER_SIG Run/Error Tracking Definitions
 *)

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
fun error_msg msg {input = s, 
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

(* Creates a state from the string s and run parser with error tracking.
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

(* 
 * Infix Opeartors Definitions
 *)

(* FUNCTOR_SIG Operators *)
infix 2 <$> <$ $>

(* APPLICATIVE_SIG Operators *)
infix 1 <*> *> <*
		
(* ALTERNATIVE_SIG Operators *)
infix 1 <|> <||>


(* 
 * FUNCTOR_SIG Definitions
 *)

(* "Penetrates" the type 'a t and apply f to it's value.
 *
 * f : ('a -> 'b) -> 'a parser -> 'b parser *)
fun fmap f p = 
	(fn state =>
		case p state of
			FAILURE x => FAILURE x
		  | SUCCESS (x, state') => SUCCESS (f x, state'))

(* Infix operator for fmap.
 *
 * f : 'a parser -> ('a -> 'b) -> 'b parser *)
fun op <$> (p, f) = fmap f p

(* Ignore result from parser and transform it's type from 'b to 'a.
 *
 * f : 'a -> 'b parser -> 'a parser *)
fun rplc_left a p = p <$> (fn _ => a)

(* Infix operator for rplc_left.
 *
 * f : 'a * 'b parser -> 'a parser *)
fun op <$ (a, p) = rplc_left a p

(* Ignore result from parser and transform it's type from 'a to 'b.
 *
 * f : 'a parser -> 'b -> 'b parser *)
fun rplc_right p b = b <$ p

(* Infix Operator for rplc_right.
 *
 * f : 'a parser * 'b -> 'b parser *)
fun op $> (p, b) = rplc_right p b


(* 
 * APPLICATIVE_SIG Definitions
 *)

(* Elevates a 'a to 'a parser.
 *
 * f : 'a -> 'a parser *)
fun pure x = fn state => SUCCESS (x, state)


(* Apply a function wrapped in context t to a value in a context t.
 *
 * f : ('a -> 'b) parser -> 'a parser -> 'b parser *)
fun apply p1 p2 = 
	(fn state =>
		case p1 state of
			FAILURE x => FAILURE x
		  | SUCCESS (f, state') =>
			(case p2 state' of
				 FAILURE x' => FAILURE x'
			   | SUCCESS (a, state'') => SUCCESS (f a, state'')))

(* Infix operator for apply
 *
 * f : ('a -> 'b) parser * 'a parser -> 'b parser *)
fun op <*> (p1, p2) = apply p1 p2


(* Chain two parser's together, if parser succed, then but ignores "right" parser result. Else, propates error.
 *
 * f : 'a parser -> 'b parser -> 'a parser *)
fun leftsq p1 p2 = 
	(fn state =>
		case p1 state of
			FAILURE x => FAILURE x
		  | SUCCESS (a, state') =>
			(case p2 state' of
				 FAILURE x' => FAILURE x'
			   | SUCCESS (_, state'') => SUCCESS (a, state'')))

(* Infix Operator for leftsq.
 *
 * f : 'a parser * 'b parser -> 'a parser *)	
fun op <* (p1, p2) = leftsq p1 p2


(* Chain two parser's together, if parser succed, then but ignores "left" parser result. Else, propates error.
 *
 * f : 'a parser -> 'b parser -> 'b parser *)
fun rightsq p1 p2 = 
	(fn state =>
		case p1 state of
			FAILURE x => FAILURE x
		  | SUCCESS (_, state') => p2 state')

(* Infix Operator for rightsq.
 *
 * f : 'a parser * 'b parser -> 'b parser *)
fun op *> (p1, p2) = rightsq p1 p2


(* Elevate a function f to a context and apply both elements.
 *
 * f : ('a -> 'b -> 'c) -> 'a parser -> 'b parser -> 'c parser *)
fun liftA2 f p1 p2 = (pure f) <*> p1 <*> p2


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


(* 
 * ALTERNATIVE_SIG Definitions
 *)

(* Empty value in a type t context.
 *
 * f : 'a parser *)
val empty = (fn state => FAILURE ("", state))

(* Or operator, if p1 succeed, return p1, else return p2.
 *
 * f : 'a parser -> 'a parser -> 'a parser *)
fun op <|> (p1, p2) = 
	(fn state => 
		case p1 state of
			FAILURE _ => p2 state
		  | SUCCESS x => SUCCESS x)

(* Improved <|> operator that preserves the most relevant error message.
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

(* 
 * PARSER'S Definitions
 *)

(* 
 * Char/String Parsers
 *)

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

(* Consumes characters until f a is false.
 *
 * f : (char -> bool) -> char list parser *)
fun spanP f = many (parse_if f)


(* 
 * Utility Parsers
 *)

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

(* Ensure the parser consumes the entire input
 *
 * f : 'a parser -> 'a parser *)
fun all_input p =
	(fn state : parser_state =>
		case p state of
			FAILURE x => FAILURE x
		  | SUCCESS (result, state' : parser_state) => 
			if String.size (#input state') = 0 
			then SUCCESS (result, state')
			else FAILURE ("Unexpected content", state'))

(* 
 * Int/Real Parsers
 *)

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
	let

		fun digits_to_hex hex_chars = 
			let
				val hex_s = implode hex_chars
			in
				case StringCvt.scanString (Int.scan StringCvt.HEX) hex_s of
					NONE => 0
				  | SOME hex => hex
			end

		fun concat_hex sign prefix hex =
			digits_to_hex (List.concat (sign::prefix::hex::[]))

		val sign_char = charP #"-" <|> charP #"+" <|> (pure #"+")
		val sign = (sign_char <$> append <*> (pure [])) <|> (pure [])

		val prefix_char = stringP "0x" <|> stringP "0X" 
		val prefix = prefix_char <|> (pure [])
	
		val digits = some (parse_if Char.isHexDigit)
	in
		sign <$> concat_hex
			 <*> prefix
			 <*> digits
	end

(* Parses a Binary Number.
 *
 * f : int parser *)	
val binP =
	let

		fun digits_to_bin bin_chars = 
			let
				val bin_s = implode bin_chars
			in
				case StringCvt.scanString (Int.scan StringCvt.BIN) bin_s of
					NONE => 0
				  | SOME bin => bin
			end

		fun concat_bin sign bin =
			digits_to_bin (List.concat (sign::bin::[]))

		val sign_char = charP #"-" <|> charP #"+" <|> (pure #"+")
		val sign = (sign_char <$> append <*> (pure [])) <|> (pure [])
	
		val digits = some (parse_if (fn c => c = #"0" orelse c = #"1"))
	in
		sign <$> concat_bin
			 <*> digits
	end

end
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
