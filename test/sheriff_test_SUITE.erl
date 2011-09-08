-module(sheriff_test_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]). %% ct.
-export([test/1]). %% Tests.

-compile({parse_transform, sheriff}).

-type a() :: atom()|list()|tuple().
-type b() :: [1|a()].
-type c(A) :: [{A}|d()].
-type d() :: [{a()}|list()].
-type f() :: [integer()|{f()}].
-type g() :: external_type:e(b()).
-type h() :: -5..5|{tuple(tuple())}.
-type i() :: {i()}.

%% ct.

all() ->
[{group, sheriff}].

groups() ->
[{sheriff, [], [test]}].

%% Tests.

test(Config) ->
	true = sheriff:check([2,{5},2.2,at,"p"],a()),
	true = sheriff:check({[2,{5},2.2,at,"p"]},a()),
	true = sheriff:check(at,a()),
	false = sheriff:check(2.3,a()),

	true = sheriff:check(1,b()),
	true = sheriff:check({[2,{5},2.2,at,"p"]},b()),
	false = sheriff:check(2.3,b()),

	true = sheriff:check([{1}],c(integer())),
	true = sheriff:check([{{atom}}],c(a())),
	false = sheriff:check([{2.3}],c(a())),

	true = sheriff:check([{[a]}],d()),
	true = sheriff:check([[azerty],[qsd]],d()),
	false = sheriff:check(1.2,d()),

	true = sheriff:check({aze,5,-5,2.1},external_type:e(tuple())),
	true = sheriff:check([3,2],external_type:e(a())),
	true = sheriff:check(5,external_type:e(tuple())),
	false = sheriff:check(8,external_type:e(a())),

	true = sheriff:check([5,5,2,-7,10000000],f()),
	true = sheriff:check([{[5,2,4]}],f()),
	false = sheriff:check([atom,atom2],f()),

	true = sheriff:check(5,g()),
	true = sheriff:check({5},g()),
	false = sheriff:check(10,g()),

	true = sheriff:check(-3,h()),
	true = sheriff:check({{a},{a}},h()),
	false = sheriff:check({1},h()),

	true = sheriff:check({2.3,[1,2]},i()),
	false = sheriff:check([{2.3}],i())
.
