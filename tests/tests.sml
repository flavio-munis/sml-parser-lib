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
            assertEqual(empty (), NONE)),
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

	(* MONAD_SIG Operators *)
	infix 1 >=> >>=

	(* ALTERNATIVE_SIG Operators *)
	infix 1 <|>

    (* Helper function to run parser and check result *)
    fun check_parser parser input expected =
        case run_parser parser input of
            NONE => assertEqual(false, true) (* Always fail if NONE *)
          | SOME (result, rest) => assertEqual((result, rest), expected)

    fun check_parser_none parser input =
        assertEqual(run_parser parser input, NONE)

    val char_parser_tests = [
        ("charP matches single char", fn () => 
            check_parser (charP #"a") "abc" (#"a", "bc")),
        ("charP fails on non-matching char", fn () => 
            check_parser_none (charP #"x") "abc"),
        ("charP fails on empty string", fn () => 
            check_parser_none (charP #"a") "")
    ]

    val string_parser_tests = [
        ("stringP matches exact string", fn () => 
            check_parser (stringP "hello") "hello world" (explode "hello", " world")),
        ("stringP fails on partial match", fn () => 
            check_parser_none (stringP "hello") "hell"),
        ("stringP fails on non-matching string", fn () => 
            check_parser_none (stringP "hello") "world")
    ]

    val not_null_tests = [
        ("not_null succeeds with non-empty list", fn () => 
            check_parser (not_null (stringP "hello")) "hello world" (explode "hello", " world")),
        ("not_null fails with empty list", fn () => 
            check_parser_none (not_null (stringP "")) "hello")
    ]

    val span_parser_tests = [
        ("spanP with digits", fn () => 
            check_parser (spanP Char.isDigit) "123abc" (explode "123", "abc")),
        ("spanP with letters", fn () => 
            check_parser (spanP Char.isAlpha) "abcDEF123" (explode "abcDEF", "123")),
        ("spanP with no matches", fn () => 
            check_parser (spanP Char.isDigit) "abc123" ([], "abc123"))
    ]

    val nat_parser_tests = [
        ("natP parses positive integer", fn () => 
            check_parser natP "123abc" (123, "abc")),
        ("natP fails on non-digit", fn () => 
            check_parser_none natP "abc123")
    ]

    (* Functor Tests *)
    val fmap_tests = [
        ("fmap transforms parser result", fn () => 
            check_parser (fmap (fn c => Char.toUpper c) (charP #"a")) "abc" (#"A", "bc")),
        ("<$> transforms parser result", fn () => 
            check_parser ((charP #"a") <$> (fn c => Char.toUpper c)) "abc" (#"A", "bc")),
        ("rplc_left replaces parser result", fn () => 
            check_parser (rplc_left #"X" (charP #"a")) "abc" (#"X", "bc")),
        ("<$ replaces parser result", fn () => 
            check_parser (#"X" <$ (charP #"a")) "abc" (#"X", "bc")),
        ("rplc_right replaces parser result", fn () => 
            check_parser (rplc_right (charP #"a") #"X") "abc" (#"X", "bc")),
        ("$> replaces parser result", fn () => 
            check_parser ((charP #"a") $> #"X") "abc" (#"X", "bc"))
    ]

    (* Applicative Tests *)
    val applicative_tests = [
        ("pure creates constant parser", fn () => 
            check_parser (pure 42) "abc" (42, "abc")),
        ("apply combines parsers", fn () => 
            check_parser (apply (pure (fn x => x + 1)) natP) "123abc" (124, "abc")),
        ("<*> combines parsers", fn () => 
            check_parser ((pure (fn x => x + 1)) <*> natP) "123abc" (124, "abc")),
        ("leftsq keeps left value", fn () => 
            check_parser (leftsq (charP #"a") (charP #"b")) "abc" (#"a", "c")),
        ("<* keeps left value", fn () => 
            check_parser ((charP #"a") <* (charP #"b")) "abc" (#"a", "c")),
        ("rightsq keeps right value", fn () => 
            check_parser (rightsq (charP #"a") (charP #"b")) "abc" (#"b", "c")),
        ("*> keeps right value", fn () => 
            check_parser ((charP #"a") *> (charP #"b")) "abc" (#"b", "c")),
        ("liftA2 applies binary function", fn () => 
            let
                fun join c1 c2 = String.str(c1) ^ String.str(c2)
            in
                check_parser (liftA2 join (charP #"a") (charP #"b")) "abc" ("ab", "c")
            end)
    ]

    (* Alternative Tests *)
    val alternative_tests = [
        ("empty always fails", fn () => 
            check_parser_none (empty ()) "abc"),
        ("<|> takes first success", fn () => 
            check_parser ((charP #"x") <|> (charP #"a")) "abc" (#"a", "bc")),
        ("<|> skips failures", fn () => 
            check_parser ((charP #"x") <|> (charP #"y") <|> (charP #"a")) "abc" (#"a", "bc"))
    ]

    (* Many/Some Tests *)
    val many_some_tests = [
        ("many matches zero or more", fn () => 
            check_parser (many (charP #"a")) "aaabcd" (explode "aaa", "bcd")),
        ("many succeeds with zero matches", fn () => 
            check_parser (many (charP #"x")) "abcd" ([], "abcd")),
        ("some matches one or more", fn () => 
            check_parser (some (charP #"a")) "aaabcd" (explode "aaa", "bcd"))
        (* "some fails with zero matches" - can't easily test failures with some *)
    ]

    val parser_tests_suite = SuiteNode("Parser Tests", [
        SuiteLeaf("Character Parser Tests", char_parser_tests),
        SuiteLeaf("String Parser Tests", string_parser_tests),
        SuiteLeaf("Not Null Tests", not_null_tests),
        SuiteLeaf("Span Parser Tests", span_parser_tests),
        SuiteLeaf("Natural Number Parser Tests", nat_parser_tests),
        SuiteLeaf("Functor Tests", fmap_tests),
        SuiteLeaf("Applicative Tests", applicative_tests),
        SuiteLeaf("Alternative Tests", alternative_tests),
        SuiteLeaf("Many/Some Tests", many_some_tests)
    ])
end

structure JsonParserTests =
struct
    open Test
    open Parser
    open JsonParser

    (* Helper function to run parser and check result *)
    fun check_json_parser input expected_json =
        case run_parser parseJson input of
            NONE => assertEqual(false, true) (* Always fail *)
          | SOME (result, rest) => (
              assertEqual(rest, ""); (* Should consume all input *)
              assertEqual(jsonToString result, jsonToString expected_json)
            )

    val json_null_tests = [
        ("Parse null value", fn () => 
            check_json_parser "null" JsonNull)
    ]

    val json_bool_tests = [
        ("Parse true value", fn () => 
            check_json_parser "true" (JsonBool true)),
        ("Parse false value", fn () => 
            check_json_parser "false" (JsonBool false))
    ]

    val json_number_tests = [
        ("Parse simple number", fn () => 
            check_json_parser "42" (JsonNumber 42)),
        ("Parse zero", fn () => 
            check_json_parser "0" (JsonNumber 0)),
        ("Parse large number", fn () => 
            check_json_parser "999999" (JsonNumber 999999))
    ]

    val json_string_tests = [
        ("Parse simple string", fn () => 
            check_json_parser "\"hello\"" (JsonString "hello")),
        ("Parse empty string", fn () => 
            check_json_parser "\"\"" (JsonString "")),
        ("Parse string with spaces", fn () => 
            check_json_parser "\"hello world\"" (JsonString "hello world"))
    ]

    val json_array_tests = [
        ("Parse empty array", fn () => 
            check_json_parser "[]" (JsonArray [])),
        ("Parse array of numbers", fn () => 
            check_json_parser "[1, 2, 3]" (JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3])),
        ("Parse array of mixed types", fn () => 
            check_json_parser "[1, \"hello\", true]" 
                (JsonArray [JsonNumber 1, JsonString "hello", JsonBool true])),
        ("Parse nested array", fn () => 
            check_json_parser "[[1, 2], [3, 4]]" 
                (JsonArray [
                    JsonArray [JsonNumber 1, JsonNumber 2], 
                    JsonArray [JsonNumber 3, JsonNumber 4]
                ]))
    ]

    val json_object_tests = [
        ("Parse empty object", fn () => 
            check_json_parser "{}" (JsonObject [])),
        ("Parse simple object", fn () => 
            check_json_parser "{\"name\": \"John\", \"age\": 30}" 
                (JsonObject [("name", JsonString "John"), ("age", JsonNumber 30)])),
        ("Parse nested object", fn () => 
            check_json_parser "{\"person\": {\"name\": \"John\", \"age\": 30}}" 
                (JsonObject [
                    ("person", JsonObject [
                        ("name", JsonString "John"), 
                        ("age", JsonNumber 30)
                    ])
                ])),
        ("Parse object with array", fn () => 
            check_json_parser "{\"numbers\": [1, 2, 3]}" 
                (JsonObject [
                    ("numbers", JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3])
                ]))
    ]

    val whitespace_tests = [
        ("Parse with leading whitespace", fn () => 
            check_json_parser "   {\"name\": \"John\"}" 
                (JsonObject [("name", JsonString "John")])),
        ("Parse with trailing whitespace", fn () => 
            check_json_parser "{\"name\": \"John\"}   " 
                (JsonObject [("name", JsonString "John")])),
        ("Parse with whitespace between elements", fn () => 
            check_json_parser "{ \"name\" :  \"John\" ,  \"age\" : 30 }" 
                (JsonObject [("name", JsonString "John"), ("age", JsonNumber 30)]))
    ]

    (* ToString Tests *)
    val json_to_string_tests = [
        ("Convert null to string", fn () => 
            assertEqual(jsonToString JsonNull, "null")),
        ("Convert boolean to string", fn () => 
            assertEqual(jsonToString (JsonBool true), "true")),
        ("Convert number to string", fn () => 
            assertEqual(jsonToString (JsonNumber 42), "42")),
        ("Convert string to string", fn () => 
            assertEqual(jsonToString (JsonString "hello"), "\"hello\"")),
        ("Convert array to string", fn () => 
            assertEqual(jsonToString (JsonArray [JsonNumber 1, JsonNumber 2]), "[1, 2]")),
        ("Convert object to string", fn () => 
            assertEqual(jsonToString (JsonObject [("name", JsonString "John")]), "{\"name\": \"John\"}"))
    ]

    val json_parser_tests_suite = SuiteNode("JSON Parser Tests", [
        SuiteLeaf("JSON Null Tests", json_null_tests),
        SuiteLeaf("JSON Boolean Tests", json_bool_tests),
        SuiteLeaf("JSON Number Tests", json_number_tests),
        SuiteLeaf("JSON String Tests", json_string_tests),
        SuiteLeaf("JSON Array Tests", json_array_tests),
        SuiteLeaf("JSON Object Tests", json_object_tests),
        SuiteLeaf("Whitespace Handling Tests", whitespace_tests),
        SuiteLeaf("JSON ToString Tests", json_to_string_tests)
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
