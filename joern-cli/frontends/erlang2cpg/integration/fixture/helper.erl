-module(helper).
-export([keep/1, keep/2, observe/1]).
keep(X) -> X.
keep(_Ignored, Y) -> Y.
observe(X) -> X.
