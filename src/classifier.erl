-module(classifier).

-behaviour(gen_server).

-include("defaults.hrl").

-record(state, {
  token_probabilities = dict:new() :: dict(),
  neg_tokens = dict:new() :: dict(),
  pos_tokens = dict:new() :: dict()
  }).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([state/0, train/1, tokens/0, classify/1, classify/2, update_probabilities/0, false_positive/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE,[],[]).


%% train/1 receives data to expand the corpus.
%% The token probabilities are not updated in this function.
%% Data can be
%%  Dir: 
%%    Path to some dir that contains two folders called pos and neg where there're files with texts to be tokenized;
%%  {Tag, InfoType, Info}:
%%    If Info is a string InfoType would be text and Info will be tokenized and stored in the Tag(pos or neg) side;
%%    If Info is a string list InfoType would be text_list and each value in Info will be tokenized and stored in the Tag(pos or neg) side;
-spec train(string() | {pos | neg, text | text_list, string() | [string()]} ) -> ok.
train(Data) ->
  gen_server:cast(?MODULE, {train, Data}).

-spec false_positive(string()) -> ok.
false_positive(Text) -> 
  gen_server:cast(?MODULE, {false_positive, Text}).

-spec classify(binary() | string()) -> acceptable | unacceptable | {acceptable | unacceptable, float()}.
classify(Text) ->
  classify(Text, []).

-spec classify(binary() | string(), [proplists:property()]) -> acceptable | unacceptable | {acceptable | unacceptable, float()}.
classify(Text, Options) when is_binary(Text) ->
  classify(binary_to_list(Text), Options);
classify(Text, Options) ->
  gen_server:call(?MODULE, {classify, Text, Options}).

-spec update_probabilities() -> ok.
update_probabilities() ->
  gen_server:cast(?MODULE, update_probabilities).


%% Debug functions

-spec state() -> #state{}.
state() ->
  gen_server:call(?MODULE, state).

-spec tokens() -> list().
tokens() ->
  gen_server:call(?MODULE, tokens).

%% ----------- %%

-spec init([]) -> {ok,#state{}}.
init([]) ->
  Timeout = classifier_utils:get_env(update_probabilities_timeout),
  timer:send_interval(Timeout, self(), update_probabilities),
  {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}} | {noreply, #state{}}.
handle_call(state, _From, State) ->
  {reply, State, State};

handle_call(tokens, _From, State = #state{token_probabilities=TokenProbabilities}) ->
  {reply, dict:to_list(TokenProbabilities), State};

handle_call({classify, Text, Options}, _From, State = #state{token_probabilities=TokenProbabilities, pos_tokens=PosTokens, neg_tokens=NegTokens}) ->
  Tokens = classifier_utils:get_text_tokenized(Text),

  Probalities = 
    lists:foldl(fun(Token, Accum) ->
      %% Get the prob for this Token
      Prob = 
        case dict:find(Token, TokenProbabilities) of
          {ok, Value} -> Value;
          error -> classifier_utils:get_env(default_probability)
        end,

      %% Keep the most significative tokens
      % case length(Accum) == round(length(Tokens)/2) of
      case length(Accum) == classifier_utils:get_env(max_text_tokens) of
        false -> [Prob | Accum] ;
        true ->
          lists:sublist(
          lists:sort(fun(A, B) ->
            abs(A - 0.5) >= abs(B - 0.5)
          end, [Prob | Accum]), classifier_utils:get_env(max_text_tokens))
          % end, [Prob | Accum]), round(length(Tokens)/2))
      end
    end, [], Tokens),

  {NegMultiplication, PosMultiplication} = 
    lists:foldl(fun(P, {Neg, Pos}) ->
      {Neg*P, Pos*(1-P)}
    end,{1,1}, Probalities),

  TextProbability = NegMultiplication / (NegMultiplication + PosMultiplication),
  
  %% Get the Result and the Updated Token Lists
  TextStatus =
    case TextProbability < classifier_utils:get_env(threshold_probability) of
      true -> acceptable;
      false -> unacceptable
    end, 

  {NewPosTokens, NewNegTokens} =
    case {TextStatus, proplists:get_value(train, Options, true)} of
      {_, false} -> {PosTokens, NegTokens};
      {acceptable, true} -> {classifier_utils:add_token_appearances(Tokens, PosTokens), NegTokens};
      {unacceptable, true} -> {PosTokens, classifier_utils:add_token_appearances(Tokens, NegTokens)}
    end,

  Reply = 
    case proplists:get_value(include_probability, Options, false) of
      true -> {TextStatus, TextProbability};
      false -> TextStatus
    end, 

  {reply, Reply, State#state{pos_tokens=NewPosTokens, neg_tokens=NewNegTokens}};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({train, {Tag, text, Text}}, State = #state{pos_tokens=PosTokens, neg_tokens=NegTokens}) ->
  NewTokens = classifier_utils:get_text_tokenized(Text),
  {NewPosTokens, NewNegTokens} =
    case Tag of
      pos -> {classifier_utils:add_token_appearances(NewTokens, PosTokens), NegTokens};
      neg -> {PosTokens, classifier_utils:add_token_appearances(NewTokens, NegTokens)}
    end,
  {noreply, State#state{pos_tokens=NewPosTokens, neg_tokens=NewNegTokens}};

handle_cast({train, {pos, text_list, TextList}}, State = #state{pos_tokens=PosTokens}) ->
  NewPosTokens = classifier_utils:add_token_appearances(classifier_utils:get_text_tokenized(string:join(TextList, " ")), PosTokens),
  {noreply, State#state{pos_tokens=NewPosTokens}};
handle_cast({train, {neg, text_list, TextList}}, State = #state{neg_tokens=NegTokens}) ->
  NewNegTokens = classifier_utils:add_token_appearances(classifier_utils:get_text_tokenized(string:join(TextList, " ")), NegTokens),
  {noreply, State#state{neg_tokens=NewNegTokens}};

handle_cast({train, Dir}, State = #state{pos_tokens=PosTokens, neg_tokens=NegTokens}) ->
  Files = classifier_utils:get_files(Dir),
  NewPosTokens = classifier_utils:add_token_appearances_from_files(pos, Files, PosTokens),
  NewNegTokens = classifier_utils:add_token_appearances_from_files(neg, Files, NegTokens),
  {noreply, State#state{pos_tokens=NewPosTokens, neg_tokens=NewNegTokens}};

handle_cast(update_probabilities, State = #state{pos_tokens=PosTokens, neg_tokens=NegTokens}) ->
  {noreply, State#state{token_probabilities=classifier_utils:calculate_probabilities(PosTokens, NegTokens)}};

handle_cast({false_positive, Text}, State = #state{pos_tokens=PosTokens, neg_tokens=NegTokens}) ->
  Tokens = classifier_utils:get_text_tokenized(Text),
  NewPosTokens = classifier_utils:add_token_appearances(Tokens, PosTokens),
  NewNegTokens = classifier_utils:delete_token_appearances(Tokens, NegTokens),
  {noreply, State#state{pos_tokens=NewPosTokens, neg_tokens=NewNegTokens}};

handle_cast(Term, State) ->
  io:format("bad term ~p",[Term]),
  {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(update_probabilities, State = #state{pos_tokens=PosTokens, neg_tokens=NegTokens}) ->
  {noreply, State#state{token_probabilities=classifier_utils:calculate_probabilities(PosTokens, NegTokens)}};

handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.