-module(folsom_unix).
-author('cybergrind <cybergrind@gmail.com>').

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2]).
-export([inc/1, inc/2, start/0]).


-define(SERVER, ?MODULE).
-record(emon, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).

init([]) ->
  process_flag(trap_exit, true),
  folsom_metrics:new_gauge(<<"running">>),
  folsom_metrics:notify(<<"running">>, <<"1">>),
  State = #emon{},
  {ok, State}.

handle_call(Req, _From, State) ->
  lager:error("Unhandled call ~p~n", [Req]),
  {reply, State}.

make_resp(Val) when is_binary(Val) ->
  Val;
make_resp(Val) when is_integer(Val) ->
  list_to_binary(integer_to_list(Val));
make_resp([_, {one, Val} | _]) ->
  list_to_binary(integer_to_list(Val));
make_resp(Val) when is_list(Val) ->
  list_to_binary(Val);
make_resp({error, _, nonexistent_metric}) ->
  <<"0">>;
make_resp(_) ->
  <<"0">>.

handle_cast({req, Req, Sock, BackPid}, State) ->
  Val = folsom_metrics:get_metric_value(Req),
  Resp = make_resp(Val),
  BackPid ! { resp, Resp, Sock},
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Req, State) ->
  lager:error("Unhandled cast: ~p~n", [Req]),
  {noreply, State}.

handle_info(Info, State) ->
  lager:error("Unhandled info: ~p~n", [Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(shutdown, _State) ->
  ok;
terminate({shutdown, _Reason}, _State) ->
  ok;
terminate(_Reason, _State) ->
  ok.


inc(What) ->
  inc(What, 1).

inc(What, Num) ->
  gen_server:cast(folsom_unix, {inc, What, Num}).

start() ->
  application:start(folsom_unix).
