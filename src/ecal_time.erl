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

-include_lib("ecal_time.hrl").

%%% Main API
-export([now/0, is_leapyear/1, datetime_to_secs/1]).
-export([day_of_time/1]).
-export([day_of_month/1]).
-export([day_of_week/1]).
-export([days_in/1]).
-export([year_of_time/1]).
-export([month_of_time/1]).
-export([plus_month/3]).
-export([minus_month/3]).
-export([plus_years/2, minus_years/2, inc_year/2, dec_year/2]).
-export([timespec_for_year/1]).
-export([second_of_day/1, minute_of_day/1, hour_of_day/1]).
-export([beginning_of_year/1, end_of_year/1]).
-export([beginning_of_hour/1, end_of_hour/1]).
-export([beginning_of_day/1, end_of_day/1]).
-export([beginning_of_minute/1, end_of_minute/1]).
-export([beginning_of_month/1, end_of_month/1]).
-export([beginning_of_week/1, end_of_week/1]).
-export([plus_seconds/2, minus_seconds/2]).
-export([plus_minutes/2, minus_minutes/2]).
-export([plus_hours/2, minus_hours/2]).
-export([plus_days/2, minus_days/2, yesterday/1, tomorrow/1]).
-export([plus_weeks/2, minus_weeks/2]).
-export([plus_year/1, plus_leap_year/1, minus_year/1, minus_leap_year/1]).

%%% Types
-type timespec():: integer().
-type month():: ?MONTH_JAN
  |?MONTH_FEB|?MONTH_MAR|?MONTH_APR|?MONTH_MAY|?MONTH_JUN
  |?MONTH_JUL|?MONTH_AUG|?MONTH_SEP|?MONTH_OCT|?MONTH_NOV
  |?MONTH_DEC|?MONTH_FEB_LEAP.
-type day():: ?DAY_SUN|?DAY_MON|?DAY_TUE|?DAY_WED|?DAY_THU|?DAY_FRI|?DAY_SAT.
-export_type([timespec/0]).
-export_type([month/0]).
-export_type([day/0]).

%%% Code Starts here.

%% @doc Returns the number of seconds since year 1 up to now.
-spec now() -> timespec().
now() ->
  now_to_secs(os:timestamp()).

%% @doc Returns the number of seconds that the given date represents (normally
%% the output of os:timestamp/0.
-spec now_to_secs(Timestamp::erlang:timestamp()) -> timespec().
now_to_secs({MegaSecs, Secs, _MicroSecs}) ->
  MegaSecs * 1000000 + Secs.

%% @doc Returns the number of seconds since year 1 that the given datetime
%% represents.
-spec datetime_to_secs(DateTime::calendar:datetime()) -> timespec().
datetime_to_secs({{_Year, _Month, _Day}, {_Hour, _Minute, _Second}}=DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime).

%% @doc Returns the timespec for the start, and the numeric month, of the
%% current ongoing year according to the given timestamp, starting at
%% 0 (january) and up to 11 (december).
-spec month_of_time(Timespec::timespec()) -> {timespec(), month()}.
month_of_time(Timespec) ->
  {Beginning, Year} = year_of_time(Timespec),
  IsLeap = is_leapyear(Year),
  month_of_time(Timespec, Beginning, IsLeap, ?MONTH_JAN).

month_of_time(Timespec, AccTimespec, _YearIsLeap, Month)
  when Timespec =:= AccTimespec ->
  {AccTimespec, Month};

month_of_time(Timespec, AccTimespec, YearIsLeap, Month)
  when Timespec < AccTimespec ->
  {minus_month(AccTimespec, YearIsLeap, Month - 1), Month - 1};

month_of_time(Timespec, AccTimespec, _YearIsLeap, ?MONTH_DEC)
  when Timespec > AccTimespec ->
  {AccTimespec, ?MONTH_DEC};

month_of_time(Timespec, AccTimespec, YearIsLeap, Month)
  when Timespec > AccTimespec ->
  month_of_time(
    Timespec, plus_month(AccTimespec, YearIsLeap, Month), YearIsLeap, Month + 1
  ).

%% @doc Returns the 00:00:00 hours of the 1st day of the current month
%% according to the given timestamp.
-spec beginning_of_month(Timespec::timespec()) -> timespec().
beginning_of_month(Timespec) ->
  {Beginning, _Month} = month_of_time(Timespec),
  Beginning.

%% @doc Returns the 23:59:59 hours of the last day of the current month
%% according to the given timestamp.
-spec end_of_month(Timespec::timespec()) -> timespec().
end_of_month(Timespec) ->
  {_YearBeginning, Year} = year_of_time(Timespec),
  {Beginning, Month} = month_of_time(Timespec),
  minus_seconds(plus_month(Beginning, is_leapyear(Year), Month), 1).

%% @doc Returns the day of the month, starting at 0.
-spec day_of_month(Timespec::timespec()) -> integer().
day_of_month(Timespec) ->
  MonthBeginning = beginning_of_month(Timespec),
  ((Timespec - MonthBeginning) div ?SECONDS_IN_DAY).

%% @doc Adds the given month to timespec.
-spec plus_month(
  Timespec::timespec(), YearIsLeap::boolean(), Month::month()
) -> timespec().
plus_month(Timespec, YearIsLeap, Month) ->
  case Month of
    ?MONTH_JAN -> plus_days(Timespec, ?DAYS_IN_JAN);
    ?MONTH_FEB -> case YearIsLeap of
      true -> plus_days(Timespec, ?DAYS_IN_LEAP_FEB);
      false -> plus_days(Timespec, ?DAYS_IN_FEB)
    end;
    ?MONTH_MAR -> plus_days(Timespec, ?DAYS_IN_MAR);
    ?MONTH_APR -> plus_days(Timespec, ?DAYS_IN_APR);
    ?MONTH_MAY -> plus_days(Timespec, ?DAYS_IN_MAY);
    ?MONTH_JUN -> plus_days(Timespec, ?DAYS_IN_JUN);
    ?MONTH_JUL -> plus_days(Timespec, ?DAYS_IN_JUL);
    ?MONTH_AUG -> plus_days(Timespec, ?DAYS_IN_AUG);
    ?MONTH_SEP -> plus_days(Timespec, ?DAYS_IN_SEP);
    ?MONTH_OCT -> plus_days(Timespec, ?DAYS_IN_OCT);
    ?MONTH_NOV -> plus_days(Timespec, ?DAYS_IN_NOV);
    ?MONTH_DEC -> plus_days(Timespec, ?DAYS_IN_DEC);
    ?MONTH_FEB_LEAP -> plus_days(Timespec, ?DAYS_IN_LEAP_FEB)
  end.

%% @doc Substracts the given month to timespec.
-spec minus_month(
  Timespec::timespec(), YearIsLeap::boolean(), Month::month()
) -> timespec().
minus_month(Timespec, YearIsLeap, Month) ->
  case Month of
    ?MONTH_JAN -> minus_days(Timespec, ?DAYS_IN_JAN);
    ?MONTH_FEB -> case YearIsLeap of
      true -> minus_days(Timespec, ?DAYS_IN_LEAP_FEB);
      false -> minus_days(Timespec, ?DAYS_IN_FEB)
    end;
    ?MONTH_MAR -> minus_days(Timespec, ?DAYS_IN_MAR);
    ?MONTH_APR -> minus_days(Timespec, ?DAYS_IN_APR);
    ?MONTH_MAY -> minus_days(Timespec, ?DAYS_IN_MAY);
    ?MONTH_JUN -> minus_days(Timespec, ?DAYS_IN_JUN);
    ?MONTH_JUL -> minus_days(Timespec, ?DAYS_IN_JUL);
    ?MONTH_AUG -> minus_days(Timespec, ?DAYS_IN_AUG);
    ?MONTH_SEP -> minus_days(Timespec, ?DAYS_IN_SEP);
    ?MONTH_OCT -> minus_days(Timespec, ?DAYS_IN_OCT);
    ?MONTH_NOV -> minus_days(Timespec, ?DAYS_IN_NOV);
    ?MONTH_DEC -> minus_days(Timespec, ?DAYS_IN_DEC);
    ?MONTH_FEB_LEAP -> minus_days(Timespec, ?DAYS_IN_LEAP_FEB)
  end.

%% @doc Returns a timespec equal to the first second of the current ongoing
%% year according to the given timespec (backwards in time).
-spec beginning_of_year(Timespec::timespec()) -> timespec().
beginning_of_year(Timespec) ->
  {Beginning, _Year} = year_of_time(Timespec),
  Beginning.

%% @doc Returns a timespec equal to the last second of the current ongoing
%% year according to the given timespec (forward in time).
-spec end_of_year(Timespec::timespec()) -> timespec().
end_of_year(Timespec) ->
  {Beginning, Year} = year_of_time(Timespec),
  minus_seconds(inc_year(Year, Beginning), 1).

%% @doc Returns the number of the current day of the week according to the
%% timespec, starting at saturday(0) up to friday(6).
-spec day_of_week(Timespec::timespec()) -> day().
day_of_week(Timespec) ->
  (Timespec rem ?SECONDS_IN_WEEK) div ?SECONDS_IN_DAY.

%% @doc Returns the number of complete days found in Length (given in seconds).
-spec days_in(Length::integer()) -> integer().
days_in(Length) ->
  Length div ?SECONDS_IN_DAY.

%% @doc Returns the 00:00:00hrs of the first day of the week (saturday).
-spec beginning_of_week(Timespec::timespec()) -> timespec().
beginning_of_week(Timespec) ->
  Day = day_of_week(Timespec),
  BeginDay = beginning_of_day(Timespec),
  minus_days(BeginDay, Day).

%% @doc Returns the 23:59:59hrs of the last day of the week (friday).
-spec end_of_week(Timespec::timespec()) -> timespec().
end_of_week(Timespec) ->
  minus_seconds(plus_weeks(beginning_of_week(Timespec), 1), 1).

%% @doc Returns the number of the year for the given Timespec.
-spec year_of_time(Timespec::timespec()) -> integer().
year_of_time(Timespec) ->
  year_of_time(Timespec, 0, 0).

year_of_time(Timespec, CandidateTs, Year) when Timespec > CandidateTs ->
  year_of_time(Timespec, inc_year(Year, CandidateTs), Year + 1);

year_of_time(Timespec, CandidateTs, Year) when Timespec =:= CandidateTs ->
  {CandidateTs, Year};

year_of_time(Timespec, CandidateTs, Year) when Timespec < CandidateTs ->
  {dec_year(Year, CandidateTs), Year - 1}. 

%% @doc Returns the Jan 1st, 00:00:00 for the given year.
-spec timespec_for_year(Year::integer()) -> timespec().
timespec_for_year(Year) ->
  lists:foldl(
    fun(X, Acc) ->
      inc_year(X, Acc)
    end,
    inc_year(0, 0),
    lists:seq(1, Year - ?GREGORIAN_YEAR_0)
  ).

%% @doc Adds the given number of years to the specified time.
-spec plus_years(Timespec::timespec(), Years::integer()) -> timespec().
plus_years(Timespec, Years) ->
  {_Beginning, BaseYear} = year_of_time(Timespec),
  lists:foldl(
    fun(X, Acc) ->
      inc_year(X, Acc)
    end,
    Timespec,
    lists:seq(BaseYear, BaseYear + Years - ?GREGORIAN_YEAR_0)
  ).

%% @doc Substracts the given number of years to the specified time.
-spec minus_years(Timespec::timespec(), Years::integer()) -> timespec().
minus_years(Timespec, Years) ->
  {_Beginning, BaseYear} = year_of_time(Timespec),
  lists:foldl(
    fun(X, Acc) ->
      dec_year(X, Acc)
    end,
    Timespec,
    lists:seq(BaseYear, BaseYear - Years + ?GREGORIAN_YEAR_0, -1)
  ).

%% @doc Increments a year, based on if Year is a leap year.
-spec inc_year(Year::integer(), Timespec::timespec()) -> timespec().
inc_year(Year, Timespec) ->
  case is_leapyear(Year) of
    true -> plus_leap_year(Timespec);
    false -> plus_year(Timespec)
  end.

%% @doc Decrements a year, based on if Year is a leap year. This one will
%% actually substract the PREVIOUS year TO the GIVEN year. So the timespec
%% resulting from substracting a year to 1999, is 1999 minus the length of
%% 1998, so, 1/1/1998.
-spec dec_year(Year::integer(), Timespec::timespec()) -> timespec().
dec_year(Year, Timespec) ->
  case is_leapyear(Year - 1) of
    true -> minus_leap_year(Timespec);
    false -> minus_year(Timespec)
  end.

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

%% Adds the given number of weeks to Timespec.
-spec plus_weeks(Timespec::timespec(), Weeks::integer()) -> timespec().
plus_weeks(Timespec, Weeks) ->
  plus_seconds(Timespec, Weeks * ?SECONDS_IN_WEEK).

%% Substracts the given number of weeks from Timespec.
-spec minus_weeks(Timespec::timespec(), Weeks::integer()) -> timespec().
minus_weeks(Timespec, Weeks) ->
  minus_seconds(Timespec, Weeks * ?SECONDS_IN_WEEK).

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

