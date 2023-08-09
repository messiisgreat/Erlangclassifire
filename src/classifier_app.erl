-module(classifier_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

-spec start() -> ok | {error, term()}.
start() ->
  application:start(classifier).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  classifier_sup:start_link().

stop(_State) ->
  ok.