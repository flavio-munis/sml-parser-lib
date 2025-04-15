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
	include FUNCTOR_SIG

	val <*>       : ('a -> 'b) t * 'a t -> 'b t
	val apply     : ('a -> 'b) t -> 'a t -> 'b t
	
	val <*        : 'a t * 'b t -> 'a t
	val leftsq    : 'a t -> 'b t -> 'a t
	
	val *>        : 'a t * 'b t -> 'b t
	val rightsq   : 'a t -> 'b t -> 'b t
	
	val pure      : 'a -> 'a t
	val sequenceA : 'a t list -> ('a list) t
end

signature MONAD_SIG =
sig
	include APPLICATIVE_SIG

	val >=>       : ('a -> 'b t) * ('b -> 'c t) -> 'a -> 'c t
	val fish_bind : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t

	val >>=       : 'a t * ('a -> 'b t) -> 'b t
	val bind      : 'a t -> ('a -> 'b t) -> 'b t
end
