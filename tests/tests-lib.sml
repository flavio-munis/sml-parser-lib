signature TESTS =
sig
	type test = string * (unit -> unit)

	(* Test Suite Tree *)
	datatype TestSuite = SuiteLeaf of string * test list
					   | SuiteNode of string * TestSuite list

	val	run_suite          : TestSuite -> unit
	
	val assertEqual           : ''a * ''a -> unit       (* Use For Any Type *)
	
	(* Takes a equality function for type 'a *)
	val assertEqualf          : 'a * 'a * ('a -> 'a -> bool) -> unit
	
	val assertEqual_string    : string * string -> unit (* Print Values *)
	val assertEqual_int       : int * int -> unit       (* Print Values *)
											  
	val assertNotEqual        : ''a * ''a -> unit       (* Use For Any Type *)
	val assertNotEqual_string : string * string -> unit (* Print Values *)
	val assertNotEqual_int    : int * int -> unit       (* Print Values *)

	val assertTrue            : bool -> unit
	val assertFalse           : bool -> unit
end

structure Test :> TESTS =
struct

exception TestFail of string

datatype TestStat = SUCCESS
				  | FAIL of string
				  | ERROR

type test = string * (unit -> unit)
datatype TestSuite = SuiteLeaf of string * test list
				   | SuiteNode of string * TestSuite list


(* Takes two values of polyequal type 'a and checks if threy are equal.
 *
 * f : ''a * ''a -> unit *)
fun assertEqual (actual, expected) =
	if actual = expected
	then ()
	else raise TestFail (": Values not Equal!")

(* Takes two values 'a and a equality function.
 *
 * f : 'a * 'a * ('a -> 'a -> bool) -> unit *)
fun assertEqualf (actual, expected, eq) =
	if eq actual expected
	then ()
	else raise TestFail (": Values not Equal!")

(* Takes two strings and checks if they are equal.
 *
 * f : string * string -> unit *)
fun assertEqual_string (actual, expected) =
	if actual = expected
	then ()
	else raise TestFail (": Expected \"" ^ expected ^ 
						 "\" but got \"" ^ actual ^ "\"!")

(* Takes two ints and checks if they are equal.
 *
 * f : int * int -> unit *)
fun assertEqual_int (actual, expected) =
	if actual = expected
	then ()
	else raise TestFail (": Expected \"" ^ (Int.toString expected) ^ 
						 "\" but got \"" ^ (Int.toString actual) ^ "\"!")

(* Takes two values of polyequal type 'a and checks if threy are different.
 *
 * f : ''a * ''a -> unit *)
fun assertNotEqual (actual, expected) =
	if actual <> expected
	then ()
	else raise TestFail (": Values are Equal!")

(* Takes two strings and checks if they are different.
 *
 * f : string * string -> unit *)
fun assertNotEqual_string (actual, expected) =
	if actual <> expected
	then ()
	else raise TestFail (": \"" ^ expected ^ 
						 "\" = \"" ^ actual ^ "\"!")

(* Takes two ints and checks if they are differemt.
 *
 * f : int * int -> unit *)
fun assertNotEqual_int (actual, expected) =
	if actual <> expected
	then ()
	else raise TestFail (": \"" ^ (Int.toString expected) ^ 
						 "\" = \"" ^ (Int.toString actual) ^ "\"!")


(* Checks if a value is true.
 *
 * f : bool -> unit *)
fun assertTrue a = assertEqual (a, true)


(* Checks if a value is false.
 *
 * f : bool -> unit *)
fun assertFalse a = assertEqual (a, false)


(* Run a test and print it's results
 *
 * f : test -> (string * TestStat) *)
fun run_test (name, f) =
	let
		val result = 
			(f (); SUCCESS)
				handle TestFail s => FAIL s
					 | _ => ERROR  
	in
		 (name, result)
	end
	
(* Run a test suite and print it's results.
 *
 * f : (int * string * Test lits) -> (int * int * int) * string *)
fun run_suiteL (depth, s, ts) =
	let

		(* apply run_test to every element *)
		val results = List.map run_test ts

		fun acc_stats ((name, stat), (total, passes, error)) = 	
				(* Accumulate the test stats *)
			case stat of
				SUCCESS => (total + 1, passes + 1, error)
			  | ERROR => (total + 1, passes, error + 1)
			  | _ => (total + 1, passes, error)


		fun render_res (name, stat) =
			case stat of
				SUCCESS => "[PASS] " ^ name ^ "\n"
			  | FAIL s => "[FAIL] " ^ name ^ " " ^ s ^ "\n"
			  | ERROR => "[ERR] " ^ name ^ " - Unexpect Error " ^ "\n"
						 
		val (total, passes, error) = foldr acc_stats
										   (0,0,0)
										   results
						
		(* Create prefix based on depth *)
        val prefix = String.implode(List.tabulate(depth, fn _ => #"="))

		(* Test SuiteLeaf Result *)
		val output = String.concat (List.map render_res results)
		val summary = (prefix ^ " " ^ s ^ " - " ^ 
					   (Int.toString passes) ^ "/" ^
					   (Int.toString total) ^ " Passes (" ^
					   (Int.toString error) ^ " Error(s))" ^ "\n")
	
	in
		((total, passes, error), summary ^ output ^ "\n")
	end
	

(* Traverse Teste Suite Tree and returns it's stats.
 *
 * f : TestSuite -> (int * int * int) * string *)
fun run_suiteT depth (SuiteLeaf (s, ts)) = run_suiteL (depth - 1, s, ts)
  | run_suiteT depth (SuiteNode (s, st)) = 
	let
		(* Run the Next Nesting Level of TestSuite *)
		fun run child =
			let
				val (stat, out) = run_suiteT (depth + 1) child
			in
				(stat, out)
			end

		(* Run all nested suites*)
		val results = List.map run st
						  
		(* Sum the result of all previous nested suites *)
		fun add_stats ((t1, p1, e1), (t2, p2, e2)) = 
			(t1 + t2, p1 + p2, e1 + e2)

		val (total, passes, error) = 
			foldl (fn ((stats, _), acc) => add_stats (stats, acc))
				  (0, 0, 0)
				  results

		(* Create prefix based on depth *)
        val prefix = String.implode(List.tabulate(depth, fn _ => #"="))

		(* Format the Output *)
		val outputs = map #2 results
		val summary = (prefix ^ "= " ^ s ^ " - " ^ 
					   (Int.toString passes) ^ "/" ^
					   (Int.toString total) ^ " Passes (" ^
					   (Int.toString error) ^ " Error(s))" ^ "\n\n")
	in
		((total, passes, error), summary ^ (String.concat outputs))
	end

(* Wrapper for traversing a suite tree and print It's Summary.
 *
 * f : TestSuite -> unit *)
fun run_suite suite = 
	let
		val (stats, output) = run_suiteT 1 suite
		val (total, passes, error) = stats
	in
		print output;
		if total = passes
		then print ("✅ All Test(s) Passed (" ^ (Int.toString total) ^ " Total)\n")
		else print ("❌ " ^ (Int.toString (total - passes)) ^ " Fail(s), " ^
				   (Int.toString error) ^ " Error(s) ("
				    ^ (Int.toString total) ^ " Total)\n")
	end									 

end

