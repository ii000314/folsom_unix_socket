-module(folsom_unix_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, check_childs/0]).


-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  GUnix = ?CHILD(gen_unix, worker),
  {ok, { {one_for_one, 5, 10}, [GUnix]} }.

check_childs() ->
  [check_for_terminate(Id) || {Id, _, _, _} <- supervisor:which_children(?MODULE)].

check_for_terminate(folsom_unix) ->
  supervisor:terminate_child(?MODULE, folsom_unix),
  supervisor:delete_child(?MODULE, folsom_unix),
  ok;
check_for_terminate(_) ->
  ok.

