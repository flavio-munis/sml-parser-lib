signature JSONPARSER_SIG =
sig
	datatype JsonValue = JsonNull
		   | JsonBool of bool
		   | JsonNumber of int
		   | JsonString of string
		   | JsonArray of JsonValue list
		   | JsonObject of (string * JsonValue) list

	val parseJson    : JsonValue Parser.parser
	val jsonToString : JsonValue -> string
	val read_file    : string -> TextIO.vector
end

structure JsonParser :> JSONPARSER_SIG =
struct
local
	open Parser
in

(* Infix Operators Orders *)
(* FUNCTOR_SIG Operators *)
infix 2 <$> <$ $>

(* APPLICATIVE_SIG Operators *)
infix 1 <*> *> <*

(* MONAD_SIG Operators *)
infix 1 >=> >>=

(* ALTERNATIVE_SIG Operators *)
infix 1 <|> <||>

datatype JsonValue = JsonNull
		   | JsonBool of bool
		   | JsonNumber of int
		   | JsonString of string
		   | JsonArray of JsonValue list
		   | JsonObject of (string * JsonValue) list


(* Helper function to provide better error messages
 *
 * f : string -> 'a parser -> 'a parser *)
fun withErrorMsg msg p =
  (fn state =>
    case p state of
      SUCCESS res => SUCCESS res
    | FAILURE (_, failed_state) => FAILURE (msg, failed_state))

(* Return a Null Value Parser
 *
 * f : JsonValue parser *)
val jsonNull = withErrorMsg "Expected \"null\" value" ((stringP "null") <$> (fn _ => JsonNull))

(* Parser for boolean values (true | false)
 *
 * f : JsonValue parser *)
val jsonBool = 
	let
		fun f s =
			if "true" = (implode s)
			then JsonBool true
			else JsonBool false
	in
		withErrorMsg "Expected \"true\" or \"false\"" (((stringP "true") <||> (stringP "false")) <$> f)
	end


(* Parser for natural numbers
 *
 * f : JsonValue parser *)
val jsonNumber = withErrorMsg "Expected Natural Number" (natP <$> (fn n => JsonNumber n))


(* Parser for strings encapsulated by double quotes \"...\"
 *
 * f : char list parser *)
val stringLiteral = 
	let
		val close_quote = withErrorMsg "Unclosed Bracket \"" ((charP #"\""))
	in 
		(charP #"\"") *> (spanP (fn c => c <> #"\"")) <* close_quote
	end


(* Parser for strings encapsulated by double quotes \"...\"
 *
 * f : JsonValue parser *)
val jsonString = stringLiteral <$> (fn s => JsonString (implode s))

(* Parser for checking for spaces
 *
 * f : char parser *)
val ws = spanP Char.isSpace

(* Parser for removing spaces between commas 
 *
 * f : char parser *)
val sep_comma = withErrorMsg "Expected ',' separator." (ws *> (charP #",") <* ws)

(* aux function 
 *
 * f : 'a -> 'a list -> 'a list *)
fun append x y = x::y

(* Getting JsonValues that are separated by a commom character.
 *
 * f : 'a parser -> 'b parser -> 'b list parser *)
fun sepBy sep element = 
	(element <$> append <*> (many (sep *> element))) <||> pure [] 


(* Return a recursive array parser.
 *
 * f : JsonValue parser -> JsonValue parser *)
fun jsonArray self =
	let
		val elements = sepBy sep_comma self
		val close_bracket = withErrorMsg "Expected ']' to close array." (charP #"]")
	in
		(charP #"[" *> ws *> elements <* ws <* close_bracket) <$> (fn x => JsonArray x)
	end

(* Return a recursive object parser.
 *
 * f : JsonValue parser -> JsonValue parser *)
fun jsonObject self =
	let
		fun make_pair x y = (implode x, y)

		val pair = withErrorMsg "Expected property name: value pair"
								(liftA2 make_pair 
										(stringLiteral 
											 <* ws <* 
											 (charP #":")
											 <* ws)
										self)
		val close_braces = withErrorMsg "Expected '}' to close Object."
										(charP #"}")
										
	in
		((charP #"{") 
		*> ws *>
		(sepBy sep_comma pair)
		<* ws <*
		close_braces) <$> (fn x => JsonObject x)
	end


(* non-recursive JsonValues.
 *
 * f : JsonValue parser *)
val non_recursive = 
	ws *>
	(jsonNull <||> jsonBool <||> jsonNumber <||> jsonString)
	<* ws

(* Fixed Point Combinator for recursive JsonValues.
 *
 * f : JsonValue parser *)
val recursive =
	let
		fun p input =
			(ws *> ((jsonObject p) <||> (jsonArray p) <||> non_recursive) <* ws) input
	in
		p
	end

(* Ensure the parser consumes the entire input *)
fun all_input p =
	(fn state : parser_state =>
		case p state of
			SUCCESS (result, state' : parser_state) =>
			if String.size (#input state') = 0 then
				SUCCESS (result, state')
			else
				FAILURE ("Unexpected content", state')
		  | FAILURE x => FAILURE x)

(* Function exposed by the sig.
 *
 * f : JsonValue parser *)
val parseJson = all_input recursive

(* Convert a JsonValue to String.
 *
 * f : JsonValue -> string *)
fun jsonToString (JsonObject fields) =
    "{" ^ String.concatWith ", " (List.map (fn (k, v) => "\"" ^ k ^ "\": " ^ jsonToString v) fields) ^ "}"
  | jsonToString (JsonArray elems) =
    "[" ^ String.concatWith ", " (List.map jsonToString elems) ^ "]"
  | jsonToString (JsonString s) = "\"" ^ s ^ "\""
  | jsonToString (JsonNumber n) = Int.toString n
  | jsonToString (JsonBool true) = "true"
  | jsonToString (JsonBool false) = "false"
  | jsonToString JsonNull = "null"

(* Reads a file and returns it's content.
 *
 * f : string -> TextIO.vector *)
fun read_file filename =
    let
        val instream = TextIO.openIn filename
        val content = TextIO.inputAll instream
        val _ = TextIO.closeIn instream
    in
        content
    end

end
end
