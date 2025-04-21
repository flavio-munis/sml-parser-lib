signature PARSER_SIG =
sig
	(*include FUNCTOR_SIG*)

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

	val run_parser   : 'a parser -> string -> 'a parser_result
	val create_state : string -> parser_state
	val charP        : char ->  char parser
	val error_msg    : string -> parser_state -> string
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
				then SUCCESS (c, {input = xs, pos = (line + 1, column), full_input=fi, line_index = li}) 
				else FAILURE ("Expected " ^ (Char.toString c) ^ ", But Got " ^ (Char.toString s'), state)
			end)

end
