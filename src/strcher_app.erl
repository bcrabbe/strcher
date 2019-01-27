-module(strcher_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    strcher_sup:start_link().

stop(_State) ->
    ok.
