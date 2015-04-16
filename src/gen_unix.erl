-module(gen_unix).
-author('cybergrind <cybergrind@gmail.com>').

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).
-export([accept_loop/3]).

-include_lib("procket/include/procket.hrl").
-record(st, {sock, poll}).
-vsn("1.1.1").

start_link() ->
   gen_server:start_link(?MODULE, [], []).

get_path() ->
  Path = application:get_env(folsom_unix, monitoring_socket,
                             <<"/tmp/monitoring.sock">>),
  file:delete(Path),
  Len = byte_size(Path),
  {<<(procket:sockaddr_common(?PF_LOCAL, Len))/binary,
     Path/binary,
     0:((procket:unix_path_max()-Len)*8)>>,
   Path}.

accept_loop(LSock, BackPid, Poll) ->
  inert:poll(Poll, LSock, []),
  case  procket:accept(LSock) of
    {ok, RSock} ->
      BackPid ! {ready, RSock};
    {error, Rsn} ->
      lager:error("folsom_unix, accept_loop error ~p", [Rsn]),
      timer:sleep(250) end,
  ?MODULE:accept_loop(LSock, BackPid, Poll).

init([]) ->
  process_flag(trap_exit, true),
  folsom_metrics:new_gauge(<<"running">>),
  folsom_metrics:notify(<<"running">>, <<"1">>),
  {SockPath, SockName} = get_path(),
  {ok, Socket} = procket:socket(?PF_LOCAL, ?SOCK_STREAM, 0),
  {ok, Poll} = inert:start(),
  ok = procket:bind(Socket, SockPath),
  ok = procket:listen(Socket, ?BACKLOG),
  State = #st{sock=Socket, poll=Poll},
  SelfPid = self(),
  spawn_link(gen_unix, accept_loop, [Socket, SelfPid, Poll]),
  file:change_mode(SockName, 8#00666),
  {ok, State}.

handle_call(Req, _From, State) ->
  lager:error("folsom_unix, unhandled call ~p~n", [Req]),
  {reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Req, State) ->
  lager:error("folsom_unix, Unhandled cast: ~p~n", [Req]),
  {noreply, State}.

handle_info({ready, Sock}, #st{poll=Poll}=State) ->
    inert:poll(Poll, Sock, [{timeout, 250}]),
    case procket:recv(Sock, 1024) of
        {ok, Req} ->
            Val = try
                      case binary:split(Req, <<" ">>, [global]) of
                          [Name] -> folsom_metrics:get_metric_value(Name);
                          [Name, <<"stat">>, Param] ->
                              Stat = lists:map(fun({K, V}) -> {list_to_binary(atom_to_list(K)), V} end,
                                               folsom_metrics:get_histogram_statistics(Name)),
                              proplists:get_value(Param, Stat, 0.0);
                          _ -> lager:error("folsom_unix, invalid request ~p", [Req]),
                               0
                      end
                  catch
                      _:E -> lager:error("folsom_unix, invalid request ~p ~p ~p",
                                         [Req, E, erlang:get_stacktrace()]),
                             0
                  end,
            Resp = make_resp(Val),
            lager:debug("folsom_unix, req~p resp:~p", [Req, Resp]),
            procket:write(Sock, Resp),
            procket:close(Sock);
        {error,eagain} ->
            self() ! {ready, Sock};
        Error ->
            lager:error("folsom_unix, error during read from unix socket: ~p", [Error]),
            procket:write(Sock, <<"-1">>) end,
    {noreply, State};

handle_info(Info, State) ->
    lager:error("folsom_unix, unhandled info: ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  catch folsom_unix_sup:check_childs(),
  {ok, State}.

terminate(_Reason, #st{sock=Sock, poll=Poll}=_State) ->
  procket:close(Sock),
  inert:stop(Poll),
  ok.

make_resp(Val) when is_binary(Val) ->
  Val;
make_resp(Val) when is_integer(Val) ->
  list_to_binary(integer_to_list(Val));
make_resp(Val) when is_float(Val) ->
  erlang:list_to_binary(float_to_list(Val, [{decimals, 5}]));
make_resp([_, {one, Val} | _]) when is_float(Val) ->
  make_resp(Val);
make_resp([_, {one, Val} | _ ]) when is_integer(Val) ->
  list_to_binary(integer_to_list(Val));
make_resp(Val) when is_list(Val) ->
  list_to_binary(Val);
make_resp({error, Metric, nonexistent_metric}) ->
  lager:error("folsom_unix, call to nonexistend_metric ~p", [Metric]),
  <<"0">>;
make_resp(_) ->
  <<"0">>.
