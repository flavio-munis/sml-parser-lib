signature TYPE_SIG =
sig
	type 'a t
end


signature FUNCTOR_SIG =
sig
	include TYPE_SIG

	val <$>        : 'a t * ('a -> 'b) -> 'b t
	val fmap       : ('a -> 'b) -> 'a t -> 'b t

	val <$         : 'a * 'b t -> 'a t
	val rplc_left  : 'a -> 'b t -> 'a t

	val $>         : 'a t * 'b -> 'b t
	val rplc_right : 'a t -> 'b -> 'b t
end


signature APPLICATIVE_SIG =
sig
	include TYPE_SIG

	val <*>       : ('a -> 'b) t * 'a t -> 'b t
	val apply     : ('a -> 'b) t -> 'a t -> 'b t
	
	val <*        : 'a t * 'b t -> 'a t
	val leftsq    : 'a t -> 'b t -> 'a t
	
	val *>        : 'a t * 'b t -> 'b t
	val rightsq   : 'a t -> 'b t -> 'b t
	
	val liftA2    : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

	val pure      : 'a -> 'a t
	val sequenceA : 'a t list -> ('a list) t
	val some      : 'a t -> 'a list t
	val many      : 'a t -> 'a list t
end

signature MONAD_SIG =
sig
	include TYPE_SIG

	val >=>       : ('a -> 'b t) * ('b -> 'c t) -> 'a -> 'c t
	val fish_bind : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t

	val >>=       : 'a t * ('a -> 'b t) -> 'b t
	val bind      : 'a t -> ('a -> 'b t) -> 'b t
end


signature ALTERNATIVE_SIG =
sig
	include TYPE_SIG
	
	val empty : 'a t
	val <|>   : 'a t * 'a t -> 'a t
	val <||>  : 'a t * 'a t -> 'a t (* Better Choice Between 'a t's*)
end


signature UTILS_SIG =
sig
	(* Curried :: operator *)
	val append    : 'a -> 'a list -> 'a list
	val replicate : int -> 'a -> 'a list
end

structure Utils :> UTILS_SIG =
struct

(* Curried version of :: operator.
 *
 * f : 'a -> 'a list -> 'a list *)
fun append x y = x::y

(* Creates a list of length n of a 'a.
 *
 * f : int -> 'a -> 'a list*)
fun replicate n p =
	if n < 1
	then []
	else
		if n = 1
		then [p]
		else p::(replicate (n- 1) p)

end

(*
(* Infix Operators Orders *)
(* FUNCTOR_SIG Operators *)
infix 2 <$> <$ $>

(* APPLICATIVE_SIG Operators *)
infix 1 <*> *> <*

(* MONAD_SIG Operators *)
infix 1 >=> >>=

(* ALTERNATIVE_SIG Operators *)
infix 1 <|>
*)
