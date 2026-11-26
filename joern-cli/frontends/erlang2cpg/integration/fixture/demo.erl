-module(demo).
-export([route/1, dispatch/3, through/2, notify/2, wait/0]).
route({ok, X}) when is_integer(X) -> helper:keep(X);
route(_) -> helper:keep(0).
dispatch(M, F, X) -> M:F(X).
through(A, B) ->
Chosen = helper:keep(A, B),
helper:observe(Chosen).
notify(Pid, Value) -> Pid ! {value, Value}.
wait() ->
receive {value, V} -> helper:observe(V)
after 0 -> idle
end.
