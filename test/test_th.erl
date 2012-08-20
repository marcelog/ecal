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
-module(test_th).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-include_lib("eunit/include/eunit.hrl").
-include_lib("ecal_th.hrl").

start() ->
  [].

stop(_SetupData) ->
  ok.

cannot_get_threshold_with_0_length(_SetupData) ->
  [?_assertError(
    threshold_cant_be_0_length, ecal_th:new(absolute, 0, 0)
  )].

cannot_get_threshold_with_negative_start(_SetupData) ->
  [?_assertError(
    threshold_start_cant_be_negative, ecal_th:new(absolute, -1, 1)
  )].

can_get_threshold(_SetupData) ->
  Th1 = ecal_th:new(absolute, 1, 2),
  [?_assertEqual(#threshold{type=absolute, tstart=1, tend=3, len=2}, Th1)].

can_case1(_SetupData) ->
  Dt1 = ecal_time:datetime_to_secs({{2012, 1, 1}, {0, 0, 0}}),
  Dt2 = ecal_time:datetime_to_secs({{2012, 1, 2}, {0, 0, 0}}),
  Th1 = ecal_th:new(absolute, Dt1, 86400),
  Th2 = ecal_th:new(absolute, Dt2, 86400),
  [?_assertEqual(false, ecal_th:intersect(Th1, Th2))].

can_case2(_SetupData) ->
  Dt2 = ecal_time:datetime_to_secs({{2012, 1, 1}, {0, 0, 0}}),
  Dt1 = ecal_time:datetime_to_secs({{2012, 1, 2}, {0, 0, 0}}),
  Th1 = ecal_th:new(absolute, Dt1, 86400),
  Th2 = ecal_th:new(absolute, Dt2, 86400),
  [?_assertEqual(false, ecal_th:intersect(Th1, Th2))].

can_case3(_SetupData) ->
  Dt1 = ecal_time:datetime_to_secs({{2012, 1, 1}, {0, 0, 0}}),
  Dt2 = ecal_time:datetime_to_secs({{2012, 1, 1}, {8, 0, 0}}),
  Th1 = ecal_th:new(absolute, Dt1, 86400),
  Th2 = ecal_th:new(absolute, Dt2, 28800),
  ResultDt = ecal_time:datetime_to_secs({{2012, 1, 1}, {8, 0, 0}}),
  Result = ecal_th:new(absolute, ResultDt, 28800),
  [?_assertEqual([Result], ecal_th:intersect(Th1, Th2))].

can_case4(_SetupData) ->
  Dt2 = ecal_time:datetime_to_secs({{2012, 1, 1}, {0, 0, 0}}),
  Dt1 = ecal_time:datetime_to_secs({{2012, 1, 1}, {8, 0, 0}}),
  Th1 = ecal_th:new(absolute, Dt1, 28800),
  Th2 = ecal_th:new(absolute, Dt2, 86400),
  ResultDt = ecal_time:datetime_to_secs({{2012, 1, 1}, {8, 0, 0}}),
  Result = ecal_th:new(absolute, ResultDt, 28800),
  [?_assertEqual([Result], ecal_th:intersect(Th1, Th2))].

can_case5(_SetupData) ->
  Dt1 = ecal_time:datetime_to_secs({{2012, 1, 1}, {0, 0, 0}}),
  Dt2 = ecal_time:datetime_to_secs({{2012, 1, 1}, {0, 0, 0}}),
  Th1 = ecal_th:new(absolute, Dt1, 28800),
  Th2 = ecal_th:new(absolute, Dt2, 86400),
  ResultDt = ecal_time:datetime_to_secs({{2012, 1, 1}, {0, 0, 0}}),
  Result = ecal_th:new(absolute, ResultDt, 28800),
  [?_assertEqual([Result], ecal_th:intersect(Th1, Th2))].

can_case6(_SetupData) ->
  Dt1 = ecal_time:datetime_to_secs({{2012, 1, 1}, {16, 0, 0}}),
  Dt2 = ecal_time:datetime_to_secs({{2012, 1, 1}, {0, 0, 0}}),
  Th1 = ecal_th:new(absolute, Dt1, 28800),
  Th2 = ecal_th:new(absolute, Dt2, 86400),
  ResultDt = ecal_time:datetime_to_secs({{2012, 1, 1}, {16, 0, 0}}),
  Result = ecal_th:new(absolute, ResultDt, 28800),
  [?_assertEqual([Result], ecal_th:intersect(Th1, Th2))].

can_intersect_seconds_of_the_day(_SetupData) ->
  SoD = ecal_th:new_second_of_day(28800, 18000),
  %% Test1
  Th1 = ecal_th:new_absolute(
    ecal_time:datetime_to_secs({{2012, 1, 1}, {0, 0, 0}}),
    86400
  ),
  %% Test2
  Th2 = ecal_th:new_absolute(
    ecal_time:datetime_to_secs({{2012, 1, 1}, {0, 0, 0}}),
    604800
  ),
  %% Test3
  Th3 = ecal_th:new_absolute(
    ecal_time:datetime_to_secs({{2012, 1, 1}, {7, 59, 0}}),
    120
  ),
  [
    ?_assertEqual(
      [ecal_th:new_absolute(
        ecal_time:datetime_to_secs({{2012, 1, 1}, {8, 0, 0}}), 18000
      )], ecal_th:intersect(SoD, Th1)
    ),
    ?_assertEqual([
      ecal_th:new_absolute(
        ecal_time:datetime_to_secs({{2012, 1, 7}, {8, 0, 0}}), 18000
      ),
      ecal_th:new_absolute(
        ecal_time:datetime_to_secs({{2012, 1, 6}, {8, 0, 0}}), 18000
      ),
      ecal_th:new_absolute(
        ecal_time:datetime_to_secs({{2012, 1, 5}, {8, 0, 0}}), 18000
      ),
      ecal_th:new_absolute(
        ecal_time:datetime_to_secs({{2012, 1, 4}, {8, 0, 0}}), 18000
      ),
      ecal_th:new_absolute(
        ecal_time:datetime_to_secs({{2012, 1, 3}, {8, 0, 0}}), 18000
      ),
      ecal_th:new_absolute(
        ecal_time:datetime_to_secs({{2012, 1, 2}, {8, 0, 0}}), 18000
      ),
      ecal_th:new_absolute(
        ecal_time:datetime_to_secs({{2012, 1, 1}, {8, 0, 0}}), 18000
      )
    ], ecal_th:intersect(SoD, Th2)
    ),
    ?_assertEqual(
      [ecal_th:new_absolute(
        ecal_time:datetime_to_secs({{2012, 1, 1}, {8, 0, 0}}), 60
      )], ecal_th:intersect(SoD, Th3)
    )
  ].

simple_test_() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun(SetupData) ->
      {inparallel, [
        cannot_get_threshold_with_0_length(SetupData),
        cannot_get_threshold_with_negative_start(SetupData),
        can_get_threshold(SetupData),
        can_case1(SetupData),
        can_case2(SetupData),
        can_case3(SetupData),
        can_case4(SetupData),
        can_case5(SetupData),
        can_case6(SetupData),
        can_intersect_seconds_of_the_day(SetupData)
      ]}
    end
  }.