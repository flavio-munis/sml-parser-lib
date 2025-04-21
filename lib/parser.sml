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

	val run_parser   : 'a parser -> string -> 'a parser_result
	val charP        : char ->  char parser
	val stringP      : string -> char list parser
	val not_null     : 'a list parser -> 'a list parser
	val spanP        : (char -> bool) -> char list parser
	val natP         : int parser
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
		val loc_msg = "Error on line " ^ (Int.toString line) ^ 
	", column " ^ (Int.toString column) ^ ":\n"
								
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

(* Matches type t zero or more times creating a list.
 *
 * f : 'a parser -> 'a list parser *)
fun many p =
    let 
        fun step state =
            case p state of
                FAILURE _ => SUCCESS ([], state)
              | SUCCESS (x, state') =>
                (case step state' of
                    FAILURE _ => SUCCESS ([x], state')
                  | SUCCESS (xs, state'') => SUCCESS (x::xs, state''))
    in
        step
    end

(* Matches type t one or more times creating a list.
 *
 * f : 'a parser -> 'a list parser *)
fun some p = p <$> append <*> many p


(* ALTERNATIVE_SIG Operators *)

(* Empty value in a type t context.
 *
 * f : unit -> 'a parser *)
fun empty () = (fn state => FAILURE ("", state))

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
              if consumed1 > consumed2 then
                FAILURE (msg1, state1)
              else
                FAILURE (msg2, state2)
            end)


(* Creates a closure for parsing the char c.
 *
 * f : char -> char parser *)
fun charP c = 
	(fn { input = s, 
		  full_input = fi,
		  line_index = li,
		  pos = (line, column)
		} =>
		if String.size s < 1
		then FAILURE ("Empty String", { input = s, 
										full_input = fi,
										line_index = li,
										pos = (line, column)
					 })
		else
			let
				val state =  {input = s, 
							  full_input = fi,
							  line_index = li,
							  pos = (line, column)}

				val (s', xs) = (String.sub(s, 0), String.extract(s, 1, NONE))
			in
				if s' = c
				then 
					if s' = #"\n"
					then SUCCESS (c, {input = xs, pos = (line + 1, 0), full_input=fi, line_index = li})
					else SUCCESS (c, {input = xs, pos = (line, column + 1), full_input=fi, line_index = li})
				else FAILURE ("Expected " ^ (Char.toString c) ^ ", But Got " ^ (Char.toString s'), state)
			end)

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
fun spanP f =
	let
		(* Helper to match a single character satisfying predicate f *)
		fun satisfyP f = 
			(fn {
				 input = s, 
				 full_input = fi, 
				 line_index = li, 
				 pos = (line, column)
			 } =>
				if String.size s < 1
				then 
					FAILURE ("Unexpected end of input", 
							  {
								input = s, 
								full_input = fi, 
								line_index = li, 
								pos = (line, column)
							  })
				else
					let
						val (c, rest) = (String.sub(s, 0), 
										 String.extract(s, 1, NONE))
						val newPos = if c = #"\n" 
									 then (line + 1, 0) 
									 else (line, column + 1)
					in
						if f c
						then SUCCESS (c, 
									  {
										input = rest, 
										full_input = fi, 
										line_index = li, 
										pos = newPos
									  })
						else FAILURE ("Character doesn't satisfy predicate", 
									  {
										input = s, 
										full_input = fi, 
										line_index = li, 
										pos = (line, column)
									  })
					end)
	in
		many (satisfyP f)
	end

(* Parses a natural number x >= 0.
 *
 * f : int parser *)	
val natP =
	(fn state =>
		let
			fun digitsToInt ds =
				foldl (fn (d, acc) => acc * 10 + (ord d - ord #"0")) 0 ds
		in
			((not_null (spanP Char.isDigit)) <$> digitsToInt) state 
			handle overflow => FAILURE ("Integer Overflow.", state)
		end)

end
