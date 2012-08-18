%%% This module contains functions to manage and intersect time thresholds.
%%%
%%% Copyright 2012 Marcelo Gornstein <marcelog@gmail.com>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
-module(ecal_th).
-export([new/3, intersect/2]).
-include_lib("ecal_th.hrl").

-export_type([
  threshold_type/0,
  threshold_start/0,
  threshold_end/0,
  threshold_length/0,
  threshold/0
]).
-type threshold_type():: absolute|second.
-type threshold_start():: ecal_time:timespec().
-type threshold_end():: ecal_time:timespec().
-type threshold_length():: integer().
-type threshold():: {
  threshold_type(), threshold_start(), threshold_end(), threshold_length()
}.


new(_Type, _Start, 0) ->
  erlang:error(threshold_cant_be_0_length);

new(Type, Start, Length) when Start >= 0 ->
  #threshold{
    type=Type,
    tstart=Start,
    tend=Start + Length,
    len=Length
};

new(_Type, _Start, _Length) ->
  erlang:error(threshold_start_cant_be_negative).

%%%-----------------------------------------------------------------------------
%%% Case 1: Does not intersect
%%%          11111
%%%               22222
%%%-----------------------------------------------------------------------------
intersect(
  #threshold{tend=End1}, #threshold{tstart=Start2}
) when End1 =< Start2 ->
  false;

%%%-----------------------------------------------------------------------------
%%% Case 2: Does not intersect
%%%          11111
%%%     22222
%%%-----------------------------------------------------------------------------
intersect(
  #threshold{tstart=Start1}, #threshold{tend=End2}
) when Start1 >= End2 ->
  false;

%%%-----------------------------------------------------------------------------
%%% Case 3: Matches
%%%          11111
%%%           222
%%%-----------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------
%%% Case 4: Matches
%%%          11111
%%%        222222222
%%%-----------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------
%%% Case 5: Matches
%%%          11111
%%%          2222222
%%%-----------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------
%%% Case 6: Matches
%%%          11111
%%%      22222
%%%-----------------------------------------------------------------------------
intersect(
  #threshold{tstart=Start1, tend=End1}, #threshold{tstart=Start2, tend=End2}
) ->
  Start = choose_start(Start1, Start2),
  End = choose_end(End1, End2),
  Length = End - Start,
  new(absolute, Start, Length).

choose_end(End1, End2) when End1 =< End2 ->
  End1;

choose_end(End1, End2) when End2 < End1 ->
  End2.

choose_start(Start1, Start2) when Start1 >= Start2 ->
  Start1;

choose_start(Start1, Start2) when Start2 > Start1 ->
  Start2.