%%% @doc This module contains functions to manage and intersect time thresholds.
%%%
%%% Copyright 2012 Marcelo Gornstein &lt;marcelog@gmail.com&gt;
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
%%% @end
%%% @copyright Marcelo Gornstein <marcelog@gmail.com>
%%% @author Marcelo Gornstein <marcelog@gmail.com>
-module(ecal_th).
-include_lib("ecal_time.hrl").
-include_lib("ecal_th.hrl").

-define(TH_ABS, absolute).
-define(TH_SOD, sec_of_day).
-define(TH_DOW, day_of_week).
-include_lib("eunit/include/eunit.hrl").

%%% Types
-export_type([
  threshold_type/0,
  threshold_start/0,
  threshold_end/0,
  threshold_length/0,
  threshold/0
]).
-type threshold_type():: ?TH_ABS|?TH_SOD|?TH_DOW.
-type threshold_start():: ecal_time:timespec().
-type threshold_end():: ecal_time:timespec().
-type threshold_length():: integer().
-type threshold():: {
  threshold_type(), threshold_start(), threshold_end(), threshold_length()
}.

%%% Main API
-export([new/3, new_second_of_day/2, new_absolute/2, intersect/2]).

%% @doc Returns an absolute time threshold, starting at the given timespec, and
%% lasting n seconds.
-spec new_absolute(
  Start::ecal_time:timespec(), Length::integer()
) -> threshold().
new_absolute(Start, Length) ->
 new(absolute, Start, Length).

%% @doc This one repeats every day, from second 0 to 86399 inclusive.
-spec new_second_of_day(Start::integer(), Length::integer()) -> threshold().
new_second_of_day(Start, Length) ->
  #threshold{
    type=sec_of_day,
    tstart=Start,
    tend=Start + Length,
    len=Length
}.

%%% Code Starts here.
%% @doc Constructs a new time threshold.
-spec new(
  Type::threshold_type(), Start::ecal_time:timespec(), Length::integer()
) -> threshold().
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

%% @doc Intersects 2 thresholds, returning false if they dont intersect, or
%% a new threshold if they do, with their intersection information.
-spec intersect(Th1::threshold(), Th2::threshold()) -> false|threshold().
intersect(
  #threshold{type=?TH_ABS,tend=End1}, #threshold{type=?TH_ABS,tstart=Start2}
) when End1 =< Start2 ->
  false;

%%%-----------------------------------------------------------------------------
%%% Case 2: Does not intersect
%%%          11111
%%%     22222
%%%-----------------------------------------------------------------------------
intersect(
  #threshold{type=?TH_ABS,tstart=Start1}, #threshold{type=?TH_ABS,tend=End2}
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
  #threshold{type=?TH_ABS,tstart=Start1, tend=End1},
  #threshold{type=?TH_ABS,tstart=Start2, tend=End2}
) ->
  Start = choose_start(Start1, Start2),
  End = choose_end(End1, End2),
  Length = End - Start,
  [new(absolute, Start, Length)];

intersect(
  #threshold{type=?TH_SOD,tstart=Start1, len=Length1},
  #threshold{type=?TH_ABS,tstart=Start2, len=Length2}=Th2
) ->
  DaysFound = ecal_time:days_in(Length2),
  {Candidates, _Timespec} = lists:foldl(
    fun(_Day, {Matches, BeginDay}) ->
      RealTh1 = new_absolute(ecal_time:plus_seconds(BeginDay, Start1), Length1),
      Matches2 = case intersect(RealTh1, Th2) of
        false -> Matches;
        [Result] -> [Result|Matches]
      end,
      {Matches2, ecal_time:plus_days(BeginDay, 1)}
    end,
    {[], ecal_time:beginning_of_day(Start2)} ,
    lists:seq(0, DaysFound)
  ),
  Candidates.

%% @doc Chooses the correct end between one of the 2 given intersecting
%% thresholds.
-spec choose_end(
  End1::ecal_time:timespec(), End2::ecal_time:timespec()
) -> ecal_time:timespec().
choose_end(End1, End2) when End1 =< End2 ->
  End1;

choose_end(End1, End2) when End2 < End1 ->
  End2.

%% @doc Chooses the correct start between one of the 2 given intersecting
%% thresholds.
-spec choose_start(
  Start1::ecal_time:timespec(), Start2::ecal_time:timespec()
) -> ecal_time:timespec().
choose_start(Start1, Start2) when Start1 >= Start2 ->
  Start1;

choose_start(Start1, Start2) when Start2 > Start1 ->
  Start2.
