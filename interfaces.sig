signature TYPE_SIG =
sig
	type 'a t
end


signature FUNCTOR_SIG =
sig
	include TYPE_SIG

	val <$   : 'a * 'b t -> 'a t
	val $>   : 'a t * 'b -> 'b t
	val <$>  : 'a t * ('a -> 'b) -> 'b t
	val fmap : ('a -> 'b) -> 'a t -> 'b t
end


signature APPLICATIVE_SIG =
sig
	include FUNCTOR_SIG

	val <*>  : ('a -> 'b) t -> 'a t -> 'b t
	val pure : 'a -> 'a t
end
