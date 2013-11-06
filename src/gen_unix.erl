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

start_link() ->
   gen_server:start_link(?MODULE, [], []).

get_path() ->
  Path = application:get_env(folsom_unix, monitoring_socket,
                             <<"/tmp/monitoring.sock">>),
  file:delete(Path),
  Len = byte_size(Path),
  <<(procket:sockaddr_common(?PF_LOCAL, Len))/binary,
    Path/binary,
    0:((procket:unix_path_max()-Len)*8)>>.

accept_loop(LSock, BackPid, Poll) ->
  inert:poll(Poll, LSock, []),
  case  procket:accept(LSock) of
    {ok, RSock} ->
      BackPid ! {ready, RSock};
    {error, Rsn} ->
      lager:error("Got error ~p", [Rsn]),
      timer:sleep(250) end,
  ?MODULE:accept_loop(LSock, BackPid, Poll).

init([]) ->
  process_flag(trap_exit, true),
  SockPath = get_path(),
  {ok, Socket} = procket:socket(?PF_LOCAL, ?SOCK_STREAM, 0),
  {ok, Poll} = inert:start(),
  ok = procket:bind(Socket, SockPath),
  ok = procket:listen(Socket, ?BACKLOG),
  State = #st{sock=Socket, poll=Poll},
  SelfPid = self(),
  spawn_link(gen_unix, accept_loop, [Socket, SelfPid, Poll]),
  {ok, State}.

handle_call(Req, _From, State) ->
  lager:debug("Unhandled call ~p~n", [Req]),
  {reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Req, State) ->
  lager:debug("Unhandled cast: ~p~n", [Req]),
  {noreply, State}.

handle_info({ready, Sock}, #st{poll=Poll}=State) ->
  inert:poll(Poll, Sock, [{timeout, 250}]),
  case procket:recv(Sock, 1024) of
    {ok, Req} ->
      lager:debug("Read req ~p", [Req]),
      gen_server:cast(folsom_unix, {req, Req, Sock, self()});
    {error,eagain} ->
      lager:debug("Got eagain. Try read again"),
      self() ! {ready, Sock};
    Error ->
      lager:error("During read from unix socket: ~p", [Error]),
      procket:write(Sock, <<"-1">>) end,
  {noreply, State};
handle_info({resp, Resp, Sock}, State) ->
  procket:write(Sock, Resp),
  procket:close(Sock),
  {noreply, State};
handle_info(Info, State) ->
  lager:debug("Unhandled info: ~p~n", [Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, #st{sock=Sock, poll=Poll}=_State) ->
  procket:close(Sock),
  inert:stop(Poll),
  ok.
