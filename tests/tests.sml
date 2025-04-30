(* Tests for Option-Extended, Parser, and JSON Parser *)

structure OptionTests =
struct
    open Test
    open Option_Ext

	(* Infix Operators Orders *)
	(* FUNCTOR_SIG Operators *)
	infix 2 <$> <$ $>
		
	(* APPLICATIVE_SIG Operators *)
	infix 1 <*> *> <*

	(* MONAD_SIG Operators *)
	infix 1 >=> >>=

	(* ALTERNATIVE_SIG Operators *)
	infix 1 <|>

    (* Tests for FUNCTOR_SIG functions *)
    val fmap_tests = [
        ("fmap with SOME", fn () => 
            assertEqual(fmap (fn x => x * 2) (SOME 5), SOME 10)),
        ("fmap with NONE", fn () => 
            assertEqual(fmap (fn x => x * 2) NONE, NONE)),
        ("<$> with SOME", fn () => 
            assertEqual((SOME 5) <$> (fn x => x * 2), SOME 10)),
        ("<$> with NONE", fn () => 
            assertEqual(NONE <$> (fn x => x * 2), NONE)),
        ("rplc_left with SOME", fn () => 
            assertEqual(rplc_left "new" (SOME 5), SOME "new")),
        ("rplc_left with NONE", fn () => 
            assertEqual(rplc_left "new" NONE, NONE)),
        ("<$ with SOME", fn () => 
            assertEqual("new" <$ (SOME 5), SOME "new")),
        ("<$ with NONE", fn () => 
            assertEqual("new" <$ NONE, NONE)),
        ("rplc_right with SOME", fn () => 
            assertEqual(rplc_right (SOME 5) "new", SOME "new")),
        ("rplc_right with NONE", fn () => 
            assertEqual(rplc_right NONE "new", NONE)),
        ("$> with SOME", fn () => 
            assertEqual((SOME 5) $> "new", SOME "new")),
        ("$> with NONE", fn () => 
            assertEqual(NONE $> "new", NONE))
    ]

    (* Tests for APPLICATIVE_SIG functions *)
    val applicative_tests = [
        ("pure", fn () => 
            assertEqual(pure 42, SOME 42)),
        ("apply with SOME", fn () => 
            assertEqual(apply (SOME (fn x => x * 2)) (SOME 5), SOME 10)),
        ("apply with NONE function", fn () => 
            assertEqual(apply NONE (SOME 5), NONE)),
        ("apply with NONE value", fn () => 
            assertEqual(apply (SOME (fn x => x * 2)) NONE, NONE)),
        ("<*> with SOME", fn () => 
            assertEqual((SOME (fn x => x * 2)) <*> (SOME 5), SOME 10)),
        ("<*> with NONE function", fn () => 
            assertEqual(NONE <*> (SOME 5), NONE)),
        ("<*> with NONE value", fn () => 
            assertEqual((SOME (fn x => x * 2)) <*> NONE, NONE)),
        ("leftsq with both SOME", fn () => 
            assertEqual(leftsq (SOME 5) (SOME 10), SOME 5)),
        ("leftsq with left NONE", fn () => 
            assertEqual(leftsq NONE (SOME 10), NONE)),
        ("leftsq with right NONE", fn () => 
            assertEqual(leftsq (SOME 5) NONE, NONE)),
        ("<* with both SOME", fn () => 
            assertEqual((SOME 5) <* (SOME 10), SOME 5)),
        ("<* with left NONE", fn () => 
            assertEqual(NONE <* (SOME 10), NONE)),
        ("<* with right NONE", fn () => 
            assertEqual((SOME 5) <* NONE, NONE)),
        ("rightsq with both SOME", fn () => 
            assertEqual(rightsq (SOME 5) (SOME 10), SOME 10)),
        ("rightsq with left NONE", fn () => 
            assertEqual(rightsq NONE (SOME 10), NONE)),
        ("rightsq with right NONE", fn () => 
            assertEqual(rightsq (SOME 5) NONE, NONE)),
        ("*> with both SOME", fn () => 
            assertEqual((SOME 5) *> (SOME 10), SOME 10)),
        ("*> with left NONE", fn () => 
            assertEqual(NONE *> (SOME 10), NONE)),
        ("*> with right NONE", fn () => 
            assertEqual((SOME 5) *> NONE, NONE)),
        ("liftA2 with both SOME", fn () => 
            assertEqual(liftA2 (fn x => fn y => x + y) (SOME 5) (SOME 10), SOME 15)),
        ("liftA2 with first NONE", fn () => 
            assertEqual(liftA2 (fn x => fn y => x + y) NONE (SOME 10), NONE)),
        ("liftA2 with second NONE", fn () => 
            assertEqual(liftA2 (fn x => fn y => x + y) (SOME 5) NONE, NONE)),
        ("sequenceA with all SOME", fn () => 
            assertEqual(sequenceA [SOME 1, SOME 2, SOME 3], SOME [1, 2, 3])),
        ("sequenceA with some NONE", fn () => 
            assertEqual(sequenceA [SOME 1, NONE, SOME 3], NONE)),
        ("sequenceA with empty list", fn () => 
            assertEqual(sequenceA [], SOME []))
    ]

    (* Tests for ALTERNATIVE_SIG functions *)
    val alternative_tests = [
        ("empty", fn () => 
            assertEqual(empty, NONE)),
        ("<|> with first SOME", fn () => 
            assertEqual((SOME 5) <|> (SOME 10), SOME 5)),
        ("<|> with first NONE", fn () => 
            assertEqual(NONE <|> (SOME 10), SOME 10)),
        ("<|> with both NONE", fn () => 
            assertEqual(NONE <|> NONE, NONE))
    ]

    (* Tests for MONAD_SIG functions *)
    val monad_tests = [
        ("bind with SOME", fn () => 
            assertEqual(bind (SOME 5) (fn x => SOME (x * 2)), SOME 10)),
        ("bind with NONE", fn () => 
            assertEqual(bind NONE (fn x => SOME (x * 2)), NONE)),
        (">>= with SOME", fn () => 
            assertEqual((SOME 5) >>= (fn x => SOME (x * 2)), SOME 10)),
        (">>= with NONE", fn () => 
            assertEqual(NONE >>= (fn x => SOME (x * 2)), NONE)),
        ("fish_bind with SOME result", fn () => 
            assertEqual(fish_bind (fn x => SOME (x * 2)) (fn x => SOME (x + 1)) 5, SOME 11)),
        ("fish_bind with first function returning NONE", fn () => 
            assertEqual(fish_bind (fn _ => NONE) (fn x => SOME (x + 1)) 5, NONE)),
        ("fish_bind with second function returning NONE", fn () => 
            assertEqual(fish_bind (fn x => SOME (x * 2)) (fn _ => NONE) 5, NONE))
    ]

    val option_tests_suite = SuiteNode("Option_Ext Tests", [
        SuiteLeaf("Functor Tests", fmap_tests),
        SuiteLeaf("Applicative Tests", applicative_tests),
        SuiteLeaf("Alternative Tests", alternative_tests),
        SuiteLeaf("Monad Tests", monad_tests)
    ])
end

structure ParserTests =
struct
    open Test
    open Parser

    (* Infix Operators Orders *)
    (* FUNCTOR_SIG Operators *)
    infix 2 <$> <$ $>

    (* APPLICATIVE_SIG Operators *)
    infix 1 <*> *> <*

    (* ALTERNATIVE_SIG Operators *)
    infix 1 <|> <||>

    (* Helper function to test parser success *)
    fun check_parser parser input expected_result expected_rest =
        case run_parser parser input of
            SUCCESS (result, state) => 
                assertEqual ((result, String.size (#input state) = String.size expected_rest), 
                            (expected_result, true))
          | FAILURE (msg, _) => 
                (print ("Parser failed with message: " ^ msg ^ "\n");
                 assertEqual(true, false))

    (* Helper function to test parser failure *)
    fun check_parser_failure parser input =
        case run_parser parser input of
            SUCCESS (_, _) => assertEqual(false, true)
          | FAILURE (_, _) => assertEqual(true, true)

    val char_parser_tests = [
        ("charP matches single char", fn () => 
            check_parser (charP #"a") "abc" #"a" "bc"),
        ("charP fails on non-matching char", fn () => 
            check_parser_failure (charP #"x") "abc"),
        ("charP fails on empty string", fn () => 
            check_parser_failure (charP #"a") ""),
        ("charP updates line count on newline", fn () =>
            check_parser (charP #"\n") "\nabc" #"\n" "abc")
    ]

    val string_parser_tests = [
        ("stringP matches exact string", fn () => 
            check_parser (stringP "hello") "hello world" (explode "hello") " world"),
        ("stringP fails on partial match", fn () => 
            check_parser_failure (stringP "hello") "hell"),
        ("stringP fails on non-matching string", fn () => 
            check_parser_failure (stringP "hello") "world"),
        ("stringP with empty string", fn () =>
            check_parser (stringP "") "abc" [] "abc")
    ]

    val not_null_tests = [
        ("not_null succeeds with non-empty list", fn () => 
            check_parser (not_null (stringP "hello")) "hello world" (explode "hello") " world"),
        ("not_null fails with empty list", fn () => 
            check_parser_failure (not_null (stringP "")) "hello")
    ]

    val span_parser_tests = [
        ("spanP with digits", fn () => 
            check_parser (spanP Char.isDigit) "123abc" (explode "123") "abc"),
        ("spanP with letters", fn () => 
            check_parser (spanP Char.isAlpha) "abcDEF123" (explode "abcDEF") "123"),
        ("spanP with no matches returns empty list", fn () => 
            check_parser (spanP Char.isDigit) "abc123" [] "abc123"),
        ("spanP with empty input", fn () =>
            check_parser (spanP Char.isDigit) "" [] "")
    ]

    val nat_parser_tests = [
        ("natP parses positive integer", fn () => 
            check_parser natP "123abc" 123 "abc"),
        ("natP parses zero", fn () => 
            check_parser natP "0abc" 0 "abc"),
        ("natP fails on non-digit", fn () => 
            check_parser_failure natP "abc123"),
        ("natP fails on empty string", fn () =>
            check_parser_failure natP "")
    ]


	val int_parser_tests = [
        ("intP parses positive integer", fn () => 
            check_parser intP "123abc" 123 "abc"),
        ("intP parses negative integer", fn () => 
            check_parser intP "-456def" ~456 "def"),
        ("intP parses zero", fn () => 
            check_parser intP "0xyz" 0 "xyz"),
        ("intP fails on non-digit", fn () => 
            check_parser_failure intP "abc123"),
        ("intP fails on empty string", fn () =>
            check_parser_failure intP ""),
        ("intP handles sign without digits", fn () =>
            check_parser_failure intP "-abc")
    ]
						   
	(* Helper function to check real parser success with tolerance *)
    fun check_parser_real parser input expected_result expected_rest =
        let
            val epsilon = 0.0000001  (* Tolerance for floating-point comparison *)
            
            fun real_equal (r1, r2) =
                Real.abs (r1 - r2) < epsilon
        in
            case run_parser parser input of
                SUCCESS (result, state) => 
                    assertEqual ((real_equal(result, expected_result), 
                                 String.size (#input state) = String.size expected_rest), 
                                (true, true))
              | FAILURE (msg, _) => 
                    (print ("Parser failed with message: " ^ msg ^ "\n");
                     assertEqual(true, false))
        end

    (* Helper function to test real parser failure *)
    fun check_parser_failure_real parser input =
        case run_parser parser input of
            SUCCESS (_, _) => assertEqual(false, true)
          | FAILURE (_, _) => assertEqual(true, true)

    val double_parser_tests = [
        ("doubleP parses simple real number", fn () => 
            check_parser_real doubleP "123.45abc" 123.45 "abc"),
        ("doubleP parses negative real number", fn () => 
            check_parser_real doubleP "-67.89xyz" ~67.89 "xyz"),
        ("doubleP parses real without decimal part", fn () => 
            check_parser_real doubleP "42xyz" 42.0 "xyz"),
        ("doubleP parses real with just decimal part", fn () => 
            check_parser_real doubleP "0.5abc" 0.5 "abc"),
        ("doubleP parses number with explicit positive sign", fn () => 
            check_parser_real doubleP "+123.45xyz" 123.45 "xyz"),
        ("doubleP parses scientific notation with positive exponent", fn () => 
            check_parser_real doubleP "1.23e2abc" 123.0 "abc"),
        ("doubleP parses scientific notation with negative exponent", fn () => 
            check_parser_real doubleP "4.5e-2xyz" 0.045 "xyz"),
        ("doubleP parses scientific notation with uppercase E", fn () => 
            check_parser_real doubleP "6.7E3def" 6700.0 "def"),
        ("doubleP parses scientific notation without decimal point", fn () => 
            check_parser_real doubleP "8E4ghi" 80000.0 "ghi"),
        ("doubleP fails on empty string", fn () =>
            check_parser_failure_real doubleP ""),
        ("doubleP fails with just decimal point", fn () =>
            check_parser_failure_real doubleP ".123"),
        ("doubleP fails with just a sign", fn () =>
            check_parser_failure_real doubleP "-abc")
    ]						   

    val hex_parser_tests = [
        ("hexP parses simple hex number", fn () => 
            check_parser hexP "1A3def" 1719791 ""),
        ("hexP parses hex number with 0x prefix", fn () => 
            check_parser hexP "0x1F4abc" 2050748 ""),
        ("hexP parses hex number with 0X prefix", fn () => 
            check_parser hexP "0XFF5xyz" 4085 "xyz"),
        ("hexP parses negative hex number", fn () => 
            check_parser hexP "-AEghi" ~174 "ghi"),
        ("hexP parses positive hex number with sign", fn () => 
            check_parser hexP "+FFjkl" 255 "jkl"),
        ("hexP handles lowercase hex digits", fn () => 
            check_parser hexP "abcdef123" 46118400291 ""),
        ("hexP handles uppercase hex digits", fn () => 
            check_parser hexP "ABCDEF123" 46118400291 ""),
        ("hexP fails on empty string", fn () =>
            check_parser_failure hexP ""),
        ("hexP fails with just 0x without digits", fn () =>
            check_parser_failure hexP "0xghi")
    ]

    val bin_parser_tests = [
        ("binP parses binary number", fn () => 
            check_parser binP "1010abc" 10 "abc"),
        ("binP parses negative binary number", fn () => 
            check_parser binP "-1100xyz" ~12 "xyz"),
        ("binP parses positive binary number with sign", fn () => 
            check_parser binP "+101def" 5 "def"),
        ("binP fails with non-binary digits", fn () =>
            check_parser_failure binP "210"),
        ("binP fails on empty string", fn () =>
            check_parser_failure binP "")
    ]

    val sep_by_tests = [
        ("sepBy with comma-separated numbers - multiple items", fn () =>
            check_parser (sepBy (charP #",") natP) "1,2,3abc" [1,2,3] "abc"),
        ("sepBy with comma-separated numbers - single item", fn () =>
            check_parser (sepBy (charP #",") natP) "42xyz" [42] "xyz"),
        ("sepBy with no matches", fn () =>
            check_parser (sepBy (charP #",") natP) "abc" [] "abc"),
        ("sepBy with empty string", fn () =>
            check_parser (sepBy (charP #",") natP) "" [] ""),
        ("sepBy with trailing separator", fn () =>
            check_parser_failure
				(sepBy (charP #",") natP) "1,2,3,"),
        ("sepBy with complex separators", fn () =>
            check_parser (sepBy (stringP ", ") natP) "1, 2, 3xyz" [1,2,3] "xyz")
    ]

    val all_input_tests = [
        ("all_input succeeds with exact match", fn () =>
            check_parser (all_input natP) "123" 123 ""),
        ("all_input fails with remaining input", fn () =>
            check_parser_failure (all_input natP) "123abc"),
        ("all_input propagates parser errors", fn () =>
            check_parser_failure (all_input natP) "abc")
    ]


    (* Functor Tests *)
    val functor_tests = [
        ("fmap transforms parser result", fn () => 
            check_parser (fmap (fn c => Char.toUpper c) (charP #"a")) "abc" #"A" "bc"),
        ("<$> transforms parser result", fn () => 
            check_parser ((charP #"a") <$> (fn c => Char.toUpper c)) "abc" #"A" "bc"),
        ("rplc_left replaces parser result", fn () => 
            check_parser (rplc_left #"X" (charP #"a")) "abc" #"X" "bc"),
        ("<$ replaces parser result", fn () => 
            check_parser (#"X" <$ (charP #"a")) "abc" #"X" "bc"),
        ("rplc_right replaces parser result", fn () => 
            check_parser (rplc_right (charP #"a") #"X") "abc" #"X" "bc"),
        ("$> replaces parser result", fn () => 
            check_parser ((charP #"a") $> #"X") "abc" #"X" "bc")
    ]

    (* Applicative Tests *)
    val applicative_tests = [
        ("pure creates constant parser", fn () => 
            check_parser (pure 42) "abc" 42 "abc"),
        ("apply combines parsers", fn () => 
            check_parser (apply (pure (fn x => x + 1)) natP) "123abc" 124 "abc"),
        ("<*> combines parsers", fn () => 
            check_parser ((pure (fn x => x + 1)) <*> natP) "123abc" 124 "abc"),
        ("leftsq keeps left value", fn () => 
            check_parser (leftsq (charP #"a") (charP #"b")) "abc" #"a" "c"),
        ("<* keeps left value", fn () => 
            check_parser ((charP #"a") <* (charP #"b")) "abc" #"a" "c"),
        ("rightsq keeps right value", fn () => 
            check_parser (rightsq (charP #"a") (charP #"b")) "abc" #"b" "c"),
        ("*> keeps right value", fn () => 
            check_parser ((charP #"a") *> (charP #"b")) "abc" #"b" "c"),
        ("liftA2 applies binary function", fn () => 
            let
                fun join c1 c2 = String.str(c1) ^ String.str(c2)
            in
                check_parser (liftA2 join (charP #"a") (charP #"b")) "abc" "ab" "c"
            end),
        ("sequenceA with char parsers", fn () =>
            check_parser (sequenceA [charP #"a", charP #"b", charP #"c"]) "abcdef" [#"a", #"b", #"c"] "def")
    ]

    (* Alternative Tests *)
    val alternative_tests = [
        ("empty always fails", fn () => 
            check_parser_failure empty "abc"),
        ("<|> takes first success", fn () => 
            check_parser ((charP #"x") <|> (charP #"a")) "abc" #"a" "bc"),
        ("<|> skips failures", fn () => 
            check_parser ((charP #"x") <|> (charP #"y") <|> (charP #"a")) "abc" #"a" "bc"),
        ("<||> preserves error message", fn () =>
            check_parser ((charP #"a") <||> (charP #"b")) "abc" #"a" "bc")
    ]

    (* Many/Some Tests *)
    val many_some_tests = [
        ("many matches zero or more", fn () => 
            check_parser (many (charP #"a")) "aaabcd" (explode "aaa") "bcd"),
        ("many succeeds with zero matches", fn () => 
            check_parser (many (charP #"x")) "abcd" [] "abcd"),
        ("some matches one or more", fn () => 
            check_parser (some (charP #"a")) "aaabcd" (explode "aaa") "bcd"),
        ("some fails with zero matches", fn () =>
            check_parser_failure (some (charP #"x")) "abcd")
    ]

    val parser_tests_suite = SuiteNode("Parser Tests", [
        SuiteLeaf("Character Parser Tests", char_parser_tests),
        SuiteLeaf("String Parser Tests", string_parser_tests),
        SuiteLeaf("Not Null Tests", not_null_tests),
        SuiteLeaf("Span Parser Tests", span_parser_tests),
        SuiteLeaf("Natural Number Parser Tests", nat_parser_tests),
		SuiteLeaf("Integer Parser Tests", int_parser_tests),
        SuiteLeaf("Double Parser Tests", double_parser_tests),
        SuiteLeaf("Hexadecimal Parser Tests", hex_parser_tests),
        SuiteLeaf("Binary Parser Tests", bin_parser_tests),
        SuiteLeaf("SepBy Parser Tests", sep_by_tests),
        SuiteLeaf("All Input Parser Tests", all_input_tests),
        SuiteLeaf("Functor Tests", functor_tests),
        SuiteLeaf("Applicative Tests", applicative_tests),
        SuiteLeaf("Alternative Tests", alternative_tests),
        SuiteLeaf("Many/Some Tests", many_some_tests)
    ])
end

structure JsonParserTests =
struct
    open Test
    open JsonParser

    (* Helper function to check parse_json results *)
    fun check_json_parser input expected_json =
        let
            val result = parse_json input
        in
            assertEqual(minify result, minify expected_json)
        end

    (* Helper function to test json parser failure - returns empty object on failure *)
    fun check_json_parser_failure input =
        let
            val result = parse_json input
        in
            assertEqual(minify result, minify (JsonObject []))
        end

    (* Tests for parse_json function *)
    val parse_json_null_tests = [
        ("Parse null value", fn () => 
            check_json_parser "null" JsonNull),
        ("Parse null with whitespace", fn () =>
            check_json_parser "  null  " JsonNull),
        ("Fail on invalid null value", fn () =>
            check_json_parser_failure "nul")
    ]

    val parse_json_bool_tests = [
        ("Parse true value", fn () => 
            check_json_parser "true" (JsonBool true)),
        ("Parse false value", fn () => 
            check_json_parser "false" (JsonBool false)),
        ("Parse true with whitespace", fn () =>
            check_json_parser "  true  " (JsonBool true)),
        ("Fail on invalid boolean value", fn () =>
            check_json_parser_failure "tru")
    ]

    val parse_json_number_tests = [
        ("Parse simple integer", fn () => 
            check_json_parser "42" (JsonNumber 42.0)),
        ("Parse zero", fn () => 
            check_json_parser "0" (JsonNumber 0.0)),
        ("Parse negative number", fn () => 
            check_json_parser "-123" (JsonNumber ~123.0)),
        ("Parse decimal number", fn () => 
            check_json_parser "3.14" (JsonNumber 3.14)),
        ("Parse scientific notation", fn () => 
            check_json_parser "1.23e-4" (JsonNumber 0.000123)),
        ("Parse number with whitespace", fn () =>
            check_json_parser "  123  " (JsonNumber 123.0))
    ]

    val parse_json_string_tests = [
        ("Parse simple string", fn () => 
            check_json_parser "\"hello\"" (JsonString "hello")),
        ("Parse empty string", fn () => 
            check_json_parser "\"\"" (JsonString "")),
        ("Parse string with spaces", fn () => 
            check_json_parser "\"hello world\"" (JsonString "hello world")),
        ("Parse string with escaped characters", fn () => 
            check_json_parser "\"hello\\nworld\"" (JsonString "hello\nworld")),
        ("Parse string with all escape sequences", fn () => 
            check_json_parser "\"\\\"\\\\\\b\\f\\n\\r\\t\"" (JsonString "\"\\\b\f\n\r\t")),
        ("Parse string with unicode escape", fn () => 
            check_json_parser "\"\\u0048\\u0065\\u006C\\u006C\\u006F\"" (JsonString "Hello")),
        ("Parse string with whitespace around", fn () =>
            check_json_parser "  \"test\"  " (JsonString "test")),
        ("Fail on unclosed string", fn () =>
            check_json_parser_failure "\"unclosed")
    ]

    val parse_json_array_tests = [
        ("Parse empty array", fn () => 
            check_json_parser "[]" (JsonArray [])),
        ("Parse array of numbers", fn () => 
            check_json_parser "[1, 2, 3]" (JsonArray [JsonNumber 1.0, JsonNumber 2.0, JsonNumber 3.0])),
        ("Parse array of mixed types", fn () => 
            check_json_parser "[1, \"hello\", true]" 
                (JsonArray [JsonNumber 1.0, JsonString "hello", JsonBool true])),
        ("Parse nested array", fn () => 
            check_json_parser "[[1, 2], [3, 4]]" 
                (JsonArray [
                    JsonArray [JsonNumber 1.0, JsonNumber 2.0], 
                    JsonArray [JsonNumber 3.0, JsonNumber 4.0]
                ])),
        ("Parse array with whitespace", fn () =>
            check_json_parser "  [ 1 , 2 , 3 ]  " 
                (JsonArray [JsonNumber 1.0, JsonNumber 2.0, JsonNumber 3.0])),
        ("Fail on unclosed array", fn () =>
            check_json_parser_failure "[1, 2, 3"),
        ("Fail on missing comma", fn () =>
            check_json_parser_failure "[1 2, 3]")
    ]

    val parse_json_object_tests = [
        ("Parse empty object", fn () => 
            check_json_parser "{}" (JsonObject [])),
        ("Parse simple object", fn () => 
            check_json_parser "{\"name\": \"John\", \"age\": 30}" 
                (JsonObject [("name", JsonString "John"), ("age", JsonNumber 30.0)])),
        ("Parse nested object", fn () => 
            check_json_parser "{\"person\": {\"name\": \"John\", \"age\": 30}}" 
                (JsonObject [
                    ("person", JsonObject [
                        ("name", JsonString "John"), 
                        ("age", JsonNumber 30.0)
                    ])
                ])),
        ("Parse object with array", fn () => 
            check_json_parser "{\"numbers\": [1, 2, 3]}" 
                (JsonObject [
                    ("numbers", JsonArray [JsonNumber 1.0, JsonNumber 2.0, JsonNumber 3.0])
                ])),
        ("Parse object with whitespace", fn () =>
            check_json_parser "  { \"name\" : \"John\" }  " 
                (JsonObject [("name", JsonString "John")])),
        ("Fail on unclosed object", fn () =>
            check_json_parser_failure "{\"name\": \"John\""),
        ("Fail on missing colon", fn () =>
            check_json_parser_failure "{\"name\" \"John\"}"),
        ("Fail on missing comma", fn () =>
            check_json_parser_failure "{\"name\": \"John\" \"age\": 30}")
    ]

    val parse_json_complex_tests = [
        ("Parse complex nested structure", fn () =>
            check_json_parser 
                "{\"person\": {\"name\": \"John\", \"age\": 30, \"isActive\": true, \"hobbies\": [\"reading\", \"coding\"], \"address\": {\"city\": \"New York\", \"zip\": 10001}}}" 
                (JsonObject [
                    ("person", JsonObject [
                        ("name", JsonString "John"),
                        ("age", JsonNumber 30.0),
                        ("isActive", JsonBool true),
                        ("hobbies", JsonArray [JsonString "reading", JsonString "coding"]),
                        ("address", JsonObject [
                            ("city", JsonString "New York"),
                            ("zip", JsonNumber 10001.0)
                        ])
                    ])
                ])
        ),
        ("Parse array of objects", fn () =>
            check_json_parser 
                "[{\"id\": 1, \"name\": \"Alice\"}, {\"id\": 2, \"name\": \"Bob\"}]"
                (JsonArray [
                    JsonObject [("id", JsonNumber 1.0), ("name", JsonString "Alice")],
                    JsonObject [("id", JsonNumber 2.0), ("name", JsonString "Bob")]
                ])
        )
    ]

    (* Tests for prettify function *)
    val prettify_tests = [
        ("Prettify null", fn () =>
            assertEqual(prettify JsonNull, "null")),
        ("Prettify boolean", fn () =>
            assertEqual(prettify (JsonBool true), "true")),
        ("Prettify number", fn () =>
            assertEqual(prettify (JsonNumber 42.0), "42")),
        ("Prettify string", fn () =>
            assertEqual(prettify (JsonString "hello"), "\"hello\"")),
        ("Prettify empty array", fn () =>
            assertEqual(prettify (JsonArray []), "[]")),
        ("Prettify simple array", fn () =>
            assertEqual(prettify (JsonArray [JsonNumber 1.0, JsonNumber 2.0]), 
                        "[\n\t1,\n\t2\n]")),
        ("Prettify empty object", fn () =>
            assertEqual(prettify (JsonObject []), "{}")),
        ("Prettify simple object", fn () =>
            assertEqual(prettify (JsonObject [("name", JsonString "John")]), 
                        "{\n\t\"name\": \"John\"\n}")),
        ("Prettify nested structure", fn () =>
            assertEqual(prettify (JsonObject [
                        ("person", JsonObject [
                            ("name", JsonString "John"),
                            ("hobbies", JsonArray [JsonString "reading", JsonString "coding"])
                        ])
                    ]),
                    "{\n\t\"person\": {\n\t\t\"name\": \"John\",\n\t\t\"hobbies\": [\n\t\t\t\"reading\",\n\t\t\t\"coding\"\n\t\t]\n\t}\n}")
        )
    ]

    (* Tests for minify function *)
    val minify_tests = [
        ("Minify null", fn () =>
            assertEqual(minify JsonNull, "null")),
        ("Minify boolean", fn () =>
            assertEqual(minify (JsonBool true), "true")),
        ("Minify number", fn () =>
            assertEqual(minify (JsonNumber 42.0), "42")),
        ("Minify string", fn () =>
            assertEqual(minify (JsonString "hello"), "\"hello\"")),
        ("Minify string with escaped chars", fn () =>
            assertEqual(minify (JsonString "hello\nworld"), "\"hello\\nworld\"")),
        ("Minify empty array", fn () =>
            assertEqual(minify (JsonArray []), "[]")),
        ("Minify simple array", fn () =>
            assertEqual(minify (JsonArray [JsonNumber 1.0, JsonNumber 2.0]), "[1,2]")),
        ("Minify empty object", fn () =>
            assertEqual(minify (JsonObject []), "{}")),
        ("Minify simple object", fn () =>
            assertEqual(minify (JsonObject [("name", JsonString "John")]), "{\"name\":\"John\"}")),
        ("Minify nested structure", fn () =>
            assertEqual(minify (JsonObject [
                        ("person", JsonObject [
                            ("name", JsonString "John"),
                            ("hobbies", JsonArray [JsonString "reading", JsonString "coding"])
                        ])
                    ]),
                    "{\"person\":{\"name\":\"John\",\"hobbies\":[\"reading\",\"coding\"]}}")
        )
    ]

    (* Tests for string escaping *)
    val escape_string_tests = [
        ("Escape double quotes", fn () =>
            check_json_parser "\"hello \\\"world\\\"\"" (JsonString "hello \"world\"")),
        ("Escape backslash", fn () =>
            check_json_parser "\"C:\\\\Program Files\\\\App\"" (JsonString "C:\\Program Files\\App")),
        ("Escape forward slash", fn () =>
            check_json_parser "\"\\/path\\/to\\/file\"" (JsonString "/path/to/file")),
        ("Escape control characters", fn () =>
            check_json_parser "\"\\b\\f\\n\\r\\t\"" (JsonString "\b\f\n\r\t"))
    ]

    (* Full test suite for JsonParser *)
    val json_parser_tests_suite = SuiteNode("JsonParser Tests", [
        SuiteLeaf("parse_json - Null Tests", parse_json_null_tests),
        SuiteLeaf("parse_json - Boolean Tests", parse_json_bool_tests),
        SuiteLeaf("parse_json - Number Tests", parse_json_number_tests),
        SuiteLeaf("parse_json - String Tests", parse_json_string_tests),
        SuiteLeaf("parse_json - Array Tests", parse_json_array_tests),
        SuiteLeaf("parse_json - Object Tests", parse_json_object_tests),
        SuiteLeaf("parse_json - Complex Tests", parse_json_complex_tests),
        SuiteLeaf("prettify Tests", prettify_tests),
        SuiteLeaf("minify Tests", minify_tests),
        SuiteLeaf("String Escaping Tests", escape_string_tests)
    ])
end


(* Main test runner *)
fun run_all_tests () =
    Test.run_suite(
        Test.SuiteNode("All Tests", [
            OptionTests.option_tests_suite,
            ParserTests.parser_tests_suite,
            JsonParserTests.json_parser_tests_suite
        ])
    )

val _ = run_all_tests()
