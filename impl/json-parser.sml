signature JSONPARSER_SIG =
sig

	datatype JsonValue = JsonNull
					   | JsonBool of bool
					   | JsonNumber of real
					   | JsonString of string
					   | JsonArray of JsonValue list
					   | JsonObject of (string * JsonValue) list

	val parse_json    : string -> JsonValue
	val prettify      : JsonValue -> string
	val minify        : JsonValue -> string
	val read_file     : string -> TextIO.vector
	val write_to_file : string -> string -> unit
end

structure JsonParser : JSONPARSER_SIG =
struct
local
	open Parser
in

type 'a t = 'a parser
type 'a parser = 'a parser
 
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
				   | JsonNumber of real
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


(* Parser for numbers.
 *
 * f : JsonValue parser *)
val jsonNumber = withErrorMsg "Invalid Number" (doubleP <$> JsonNumber)

(* Transform a word into it's utf-8 form.
 *
 * f :  word -> char list *)
fun utf8_encode cp =
	let
		fun w i = Word.fromInt i
		fun i w = Word.toInt w
	in
		if cp < (w 0x80) then
			[chr (i cp)]
		else if cp < (w 0x800) then
			[ chr (i ((w 0xC0) + (Word.>> (cp, w 6)))),
			  chr (i ((w 0x80) + (Word.andb (cp, w 0x3F)))) ]
		else if cp < (w 0x10000) then
			[ chr (i ((w 0xE0) + (Word.>> (cp, w 12)))),
			  chr (i ((w 0x80) + (Word.andb (Word.>> (cp, w 6), w 0x3F)))),
			  chr (i ((w 0x80) + (Word.andb (cp, w 0x3F)))) ]
		else if cp <= (w 0x10FFFF) then
			[ chr (i ((w 0xF0) + Word.>> (cp, w 18))),
			  chr (i ((w 0x80) + (Word.andb (Word.>> (cp, w 12), w 0x3F)))),
			  chr (i ((w 0x80) + (Word.andb (Word.>> (cp, w 6), w 0x3F)))),
			  chr (i ((w 0x80) + (Word.andb (cp, w 0x3F)))) ]
			
	else
		raise Fail ("utf8_encode: codepoint out of range: " ^ Int.toString (i cp))
	end



(* Parse a unicode character of the form \\u0000 and return in utf-8 encode.
 *
 * f : char list parser *)
val escape_unicode = 
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
				
		fun digits_to_word ds =
			Word.fromInt (foldl aux 0 ds)
	in
		(sequenceA (replicate 4 (parse_if Char.isHexDigit))) <$> (utf8_encode o digits_to_word)
						
	end

(* Parser for strings encapsulated by double quotes \"...\"
 *
 * f : char list parser *)
val stringLiteral = 
	let
		val close_quote = withErrorMsg "Unclosed Quote \"" ((charP #"\""))
		val normal_char = 
			(parse_if (fn c => c <> #"\"" andalso c <> #"\\")) <$> (fn c => [c])
		val escape_char = 
			([#"\""] <$ stringP "\\\"") <|>
			([#"\\"] <$ stringP "\\\\") <|>
			([#"/"]  <$ stringP "\\/")  <|>	  
			([#"\b"] <$ stringP "\\b")  <|>
			([#"\f"] <$ stringP "\\f")  <|>
			([#"\n"] <$ stringP "\\n")  <|>
			([#"\r"] <$ stringP "\\r")  <|>
			([#"\t"] <$ stringP "\\t")  <|>
			(stringP "\\u" *> escape_unicode)
	in 
		(charP #"\"") *> 
		(many (normal_char <|> escape_char) <$> List.concat)
		<* close_quote
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
val sep_comma = withErrorMsg "Expected ',' separator." (ws *> charP #"," <* ws)


(* Return a recursive array parser.
 *
 * f : JsonValue parser -> JsonValue parser *)
fun jsonArray self =
	let
		val elements = sepBy sep_comma self
		val close_bracket = withErrorMsg "Expected ']' to close array." (charP #"]")
	in
		(charP #"[" *> ws *> elements <* ws <* close_bracket) <$> JsonArray
	end

(* Return a recursive object parser.
 *
 * f : JsonValue parser -> JsonValue parser *)
fun jsonObject self =
	let
		fun make_pair x y = (implode x, y)

		val key = withErrorMsg "Invalid Key" (stringLiteral 
											  <* ws <* 
											  (charP #":")
											  <* ws)							   

		val close_braces = withErrorMsg "Expected '}' to close Object."
										(charP #"}")

		val pair = liftA2 make_pair key self
										
		val obj = withErrorMsg "Invalid Object." 
								(ws *> charP #"{" 
									*> ws *> (sepBy sep_comma pair))
	in
		(
		  obj
		  <* ws <*
		  close_braces
		) <$> JsonObject
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
		val generic_error = withErrorMsg "Expected TRUE, FALSE, NUMBER, STRING, '[' or '{'" empty
		fun p input =
			(ws *> ((jsonObject p) <||> (jsonArray p) <||> non_recursive <||> generic_error) <* ws) input
	in
		p
	end

(* Ensure the parser consumes the entire input *)
fun all_input p =
	(fn state : parser_state =>
		case p state of
			FAILURE x => FAILURE x
		  | SUCCESS (result, state' : parser_state) => 
			if String.size (#input state') = 0 
			then SUCCESS (result, state')
			else FAILURE ("Unexpected content", state'))
		
(* Function exposed by the sig.
 *
 * f : string -> JsonValue *)
fun parse_json s = 
	case run_parser (all_input recursive) s of
		FAILURE _ => JsonObject []
	  | SUCCESS (res, state) => res


(* Convert escaped characters in a string literal to it's original form.
 *
 * f : string -> string *)
fun escape_string s =
    let
        fun escape #"\"" = "\\\""
          | escape #"\\" = "\\\\"
          | escape #"\b" = "\\b"
          | escape #"\f" = "\\f" 
          | escape #"\n" = "\\n"
          | escape #"\r" = "\\r"
          | escape #"\t" = "\\t"
          | escape c = str c
                
        val chars = explode s
    in
        String.concat (map escape chars)
    end

(* Transform a JsonValue to String and Prettify it.
 *
 * f : JsonValue * int -> string *)
fun to_string_prettify (json_v, level) =
    let
		(* Helper function to repeat a tab character for indentation 
		 *
		 * f : string -> string *)
		fun indent n = String.concat (List.tabulate (n, fn _ => "\t"))

        val ind = indent level
        val ind_inner = indent (level + 1)
    in
        case json_v of
            JsonObject fields =>
                if null fields then "{}"
                else "{\n"
                    ^ String.concatWith ",\n" (List.map (fn (k, v) =>
                        ind_inner ^ "\"" ^ k ^ "\": " ^ to_string_prettify (v, level + 1)
                    ) fields)
                    ^ "\n" ^ ind ^ "}"

          | JsonArray elems =>
                if null elems then "[]"
                else "[\n"
                    ^ String.concatWith ",\n" (List.map (fn e =>
                        ind_inner ^ to_string_prettify (e, level + 1)
                    ) elems)
                    ^ "\n" ^ ind ^ "]"

          | JsonString s => "\"" ^ escape_string s ^ "\""
          | JsonNumber n =>
                String.map (fn c => if c = #"~" then #"-" else c) (Real.toString n)
          | JsonBool true => "true"
          | JsonBool false => "false"
          | JsonNull => "null"
    end

(* Returns a Prettified json string 
 * 
 * f : jsonValue -> string *)
fun prettify json_v = to_string_prettify (json_v, 0)


(* Transform a JsonValue to String and Prettify it.
 *
 * f : JsonValue * int -> string *)
fun minify (JsonObject fields) =
    "{" ^ String.concatWith "," (List.map (fn (k, v) => "\"" ^ k ^ "\":" ^ minify v) fields) ^ "}"
  | minify (JsonArray elems) =
    "[" ^ String.concatWith "," (List.map minify elems) ^ "]"
  | minify (JsonString s) = "\"" ^ (escape_string s) ^ "\""
  | minify (JsonNumber n) = Real.toString n
  | minify (JsonBool true) = "true"
  | minify (JsonBool false) = "false"
  | minify JsonNull = "null"

(* Reads a file and returns it's content.
 *
 * f : string -> TextIO.vector *)
fun read_file path =
    let
        val instream = TextIO.openIn path
        val content = TextIO.inputAll instream
        val _ = TextIO.closeIn instream
    in
        content
    end

(* Write a String to a File.
 *
 * f : string -> string -> unit *)
fun write_to_file text path =
	let
        val outstream = TextIO.openOut path
    in
        TextIO.output (outstream, text);
        TextIO.closeOut outstream
    end

end
end
