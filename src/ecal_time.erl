%%% @doc General calendar functions.
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
-module(ecal_time).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%%% Main API
-export([now/0, is_leapyear/1, datetime_to_secs/1]).
-export([day_of_time/1]).
-export([second_of_day/1, minute_of_day/1, hour_of_day/1]).
-export([beginning_of_hour/1, end_of_hour/1]).
-export([beginning_of_day/1, end_of_day/1]).
-export([beginning_of_minute/1, end_of_minute/1]).
-export([plus_seconds/2, minus_seconds/2]).
-export([plus_minutes/2, minus_minutes/2]).
-export([plus_hours/2, minus_hours/2]).
-export([plus_days/2, minus_days/2, yesterday/1, tomorrow/1]).
-export([plus_year/1, plus_leap_year/1, minus_year/1, minus_leap_year/1]).

%%% Types
-type timespec():: integer().
-export_type([timespec/0]).

%%% Constants
-define(SECONDS_IN_MINUTE, 60).
-define(MINUTES_IN_HOUR, 60).
-define(HOURS_IN_DAY, 24).
-define(DAYS_IN_WEEK, 7).
-define(DAYS_IN_YEAR, 365).
-define(DAYS_IN_LEAP_YEAR, 366).
-define(DAYS_IN_SHORT_MONTH, 30).
-define(DAYS_IN_LONG_MONTH, 31).
-define(DAYS_IN_JAN, ?DAYS_IN_LONG_MONTH).
-define(DAYS_IN_FEB, 28).
-define(DAYS_IN_LEAP_FEB, 29).
-define(DAYS_IN_MAR, ?DAYS_IN_LONG_MONTH).
-define(DAYS_IN_APR, ?DAYS_IN_SHORT_MONTH).
-define(DAYS_IN_MAY, ?DAYS_IN_LONG_MONTH).
-define(DAYS_IN_JUN, ?DAYS_IN_SHORT_MONTH).
-define(DAYS_IN_JUL, ?DAYS_IN_LONG_MONTH).
-define(DAYS_IN_AUG, ?DAYS_IN_LONG_MONTH).
-define(DAYS_IN_SEP, ?DAYS_IN_SHORT_MONTH).
-define(DAYS_IN_OCT, ?DAYS_IN_LONG_MONTH).
-define(DAYS_IN_NOV, ?DAYS_IN_SHORT_MONTH).
-define(DAYS_IN_DEC, ?DAYS_IN_LONG_MONTH).
-define(SECONDS_IN_HOUR, (?MINUTES_IN_HOUR * ?SECONDS_IN_MINUTE)).
-define(SECONDS_IN_DAY, (?HOURS_IN_DAY * ?SECONDS_IN_HOUR)).
-define(SECONDS_IN_WEEK, (?DAYS_IN_WEEK * ?SECONDS_IN_DAY)).
-define(SECONDS_IN_YEAR, (?DAYS_IN_YEAR * ?SECONDS_IN_DAY)).
-define(SECONDS_IN_LEAP_YEAR, (?SECONDS_IN_DAY * ?DAYS_IN_LEAP_YEAR)).

%%% Code Starts here.

%% @doc Returns the number of seconds since year 1 up to now.
-spec now() -> timespec().
now() ->
  now_to_secs(erlang:now()).

%% @doc Returns the number of seconds that the given date represents (normally
%% the output of erlang:now/0.
-spec now_to_secs(Timestamp::erlang:timestamp()) -> timespec().
now_to_secs({MegaSecs, Secs, _MicroSecs}) ->
  MegaSecs * 1000000 + Secs.

%% @doc Returns the number of seconds since year 1 that the given datetime
%% represents.
-spec datetime_to_secs({
  {Year::integer(), Month::integer(), Day::integer()},
  {Hour::integer(), Minute::integer(), Second::integer()}
}) -> timespec().
datetime_to_secs({{_Year, _Month, _Day}, {_Hour, _Minute, _Second}}=DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime).

%% @doc Adds a complete leap year to the given timespec.
-spec plus_leap_year(Timespec::timespec()) -> timespec().
plus_leap_year(Timespec) ->
  plus_days(Timespec, ?DAYS_IN_LEAP_YEAR).

%% @doc Substracts a complete leap year to the given timespec.
-spec minus_leap_year(Timespec::timespec()) -> timespec().
minus_leap_year(Timespec) ->
  minus_days(Timespec, ?DAYS_IN_LEAP_YEAR).

%% @doc Adds a complete year to the given timespec.
-spec plus_year(Timespec::timespec()) -> timespec().
plus_year(Timespec) ->
  plus_days(Timespec, ?DAYS_IN_YEAR).

%% @doc Substracts a complete year to the given timespec.
-spec minus_year(Timespec::timespec()) -> timespec().
minus_year(Timespec) ->
  minus_days(Timespec, ?DAYS_IN_YEAR).

%% @doc Returns the number of complete days in the given timestamp.
-spec day_of_time(Timespec::timespec()) -> integer().
day_of_time(Timespec) ->
  Timespec div ?SECONDS_IN_DAY.

%% @doc Returns the current second of the current day of the given timestamp
%% (0 - 86400).
-spec second_of_day(Timespec::timespec()) -> integer().
second_of_day(Timespec) ->
  minus_seconds(Timespec, day_of_time(Timespec) * ?SECONDS_IN_DAY).

%% @doc Returns the current minute of the current day of the given timestamp
%% (0 - 1440).
-spec minute_of_day(Timespec::timespec()) -> integer().
minute_of_day(Timespec) ->
  second_of_day(Timespec) div ?SECONDS_IN_MINUTE.

%% @doc Returns the current hour of the current day of the given timestamp
%% (0 - 23).
-spec hour_of_day(Timespec::timespec()) -> integer().
hour_of_day(Timespec) ->
  second_of_day(Timespec) div ?SECONDS_IN_HOUR.

%% @doc Returns a timespec equal to the first second of the next
%% day according to the given timespec (forward in time).
-spec tomorrow(Timespec::timespec()) -> timespec().
tomorrow(Timespec) ->
  plus_days(beginning_of_day(Timespec), 1).

%% @doc Returns a timespec equal to the first second of the previous
%% day according to the given timespec (backwards in time).
-spec yesterday(Timespec::timespec()) -> timespec().
yesterday(Timespec) ->
  minus_days(beginning_of_day(Timespec), 1).

%% @doc Returns a timespec equal to the first second of the current ongoing
%% day according to the given timespec (backwards in time).
-spec beginning_of_day(Timespec::timespec()) -> timespec().
beginning_of_day(Timespec) ->
  minus_seconds(Timespec, second_of_day(Timespec)).

%% @doc Returns a timespec equal to the last second of the current ongoing
%% day according to the given timespec (forward in time).
-spec end_of_day(Timespec::timespec()) -> timespec().
end_of_day(Timespec) ->
  minus_seconds(plus_days(beginning_of_day(Timespec), 1), 1).

%% @doc Returns a timespec equal to the first second of the current ongoing
%% minute according to the given timespec (backwards in time).
-spec beginning_of_minute(Timespec::timespec()) -> timespec().
beginning_of_minute(Timespec) ->
  SecDay = second_of_day(Timespec),
  ExtraSeconds = SecDay rem ?SECONDS_IN_MINUTE,
  minus_seconds(Timespec, ExtraSeconds).

%% @doc Returns a timespec equal to the last second of the current ongoing
%% minute according to the given timespec (forward in time).
-spec end_of_minute(Timespec::timespec()) -> timespec().
end_of_minute(Timespec) ->
  minus_seconds(plus_minutes(beginning_of_minute(Timespec), 1), 1).

%% @doc Returns a timespec equal to the first second of the current ongoing hour
%% according to the given timespec (backwards in time).
-spec beginning_of_hour(Timespec::timespec()) -> timespec().
beginning_of_hour(Timespec) ->
  SecDay = second_of_day(Timespec),
  ExtraSeconds = SecDay rem ?SECONDS_IN_HOUR,
  minus_seconds(Timespec, ExtraSeconds).

%% @doc Returns a timespec equal to the last second of the current ongoing hour
%% according to the given timespec (forward in time).
-spec end_of_hour(Timespec::timespec()) -> timespec().
end_of_hour(Timespec) ->
  minus_seconds(plus_hours(beginning_of_hour(Timespec), 1), 1).

%% @doc Adds the given number of days to the specified time.
-spec plus_days(Timespec::timespec(), Hours::integer()) -> timespec().
plus_days(Timespec, Days) ->
  plus_seconds(Timespec, ?SECONDS_IN_DAY * Days).

%% @doc Substracts the given number of days to the specified time.
-spec minus_days(Timespec::timespec(), Hours::integer()) -> timespec().
minus_days(Timespec, Days) ->
  plus_days(Timespec, -Days).

%% @doc Adds the given number of hours to the specified time.
-spec plus_hours(Timespec::timespec(), Hours::integer()) -> timespec().
plus_hours(Timespec, Hours) ->
  plus_seconds(Timespec, ?SECONDS_IN_HOUR * Hours).

%% @doc Substracts the given number of hours to the specified time.
-spec minus_hours(Timespec::timespec(), Hours::integer()) -> timespec().
minus_hours(Timespec, Hours) ->
  plus_hours(Timespec, -Hours).

%% @doc Adds the given number of minutes to the specified time.
-spec plus_minutes(Timespec::timespec(), Minutes::integer()) -> timespec().
plus_minutes(Timespec, Minutes) ->
  plus_seconds(Timespec, ?SECONDS_IN_MINUTE * Minutes).

%% @doc Substracts the given number of minutes to the specified time.
-spec minus_minutes(Timespec::timespec(), Minutes::integer()) -> timespec().
minus_minutes(Timespec, Minutes) ->
  plus_minutes(Timespec, -Minutes).

%% @doc Adds the given number of seconds to the specified time.
-spec plus_seconds(Timespec::timespec(), Seconds::integer()) -> timespec().
plus_seconds(Timespec, Seconds) ->
  Timespec + Seconds.

%% @doc Substracts the given number of seconds to the specified time.
-spec minus_seconds(Timespec::timespec(), Seconds::integer()) -> timespec().
minus_seconds(Timespec, Seconds) ->
  plus_seconds(Timespec, -Seconds).

%% @doc Returns true if the given year is a leap year.
-spec is_leapyear(Year::integer()) -> boolean().
is_leapyear(Year) when Year rem 400 =:= 0 ->
  true;

is_leapyear(Year) when Year rem 100 =:= 0 ->
  false;

is_leapyear(Year) when Year rem 4 =:= 0 ->
  true;

is_leapyear(_Year) ->
  false.

