-module(folsom_unix_app).
-behaviour(application).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    folsom_unix_sup:start_link().

stop(_State) ->
    ok.
