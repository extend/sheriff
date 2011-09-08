-module(sheriff_test_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]). %% ct.
-export([test_a/1,test_b/1,test_c/1,test_d/1,test_e/1,test_f/1
		,test_g/1,test_h/1,test_i/1]). %% Tests.

-compile({parse_transform, sheriff}).

-type a() :: atom()|list()|tuple().
-type b() :: [1|a()].
-type c(A) :: [{A}|d()].
-type d() :: [{a()}|list()].
-type f() :: [integer()|{f()}].
-type g() :: external_type:e(b()).
-type h() :: -5..5|{tuple(tuple())}.
-type i() :: [i()|atom()].

%% ct.

all() ->
[{group, sheriff}].

groups() ->
[{sheriff, [], [test_a,test_b,test_c,test_d,test_e,test_f,
				test_g,test_h,test_i]}].

%% Tests.

test_a(_Config) ->
	true = sheriff:check([2,{5},2.2,at,"p"],a()),
	true = sheriff:check({[2,{5},2.2,at,"p"]},a()),
	true = sheriff:check(at,a()),
	false = sheriff:check(2.3,a()).

test_b(_Config) ->
	true = sheriff:check([1],b()),
	true = sheriff:check([{[2,{5},2.2,at,"p"]}],b()),
	false = sheriff:check(2.3,b()).

test_c(_Config) ->
	true = sheriff:check([{1}],c(integer())),
	true = sheriff:check([[{atom}],[[],[a,"b"]]],c(integer())),
	true = sheriff:check([{{2.3}}],c(a())),
	false = sheriff:check([{2.3}],c(a())).

test_d(_Config) ->
	true = sheriff:check([{[a]}],d()),
	true = sheriff:check([[azerty],[qsd]],d()),
	false = sheriff:check(1.2,d()).

test_e(_Config) ->
	true = sheriff:check([{5},{a}],external_type:e([tuple()])),
	true = sheriff:check([1,a,"a"],external_type:e(list())),
	true = sheriff:check([3,2],external_type:e(a())),
	true = sheriff:check(5,external_type:e(tuple())),
	false = sheriff:check(8,external_type:e(a())).

test_f(_Config) ->
	true = sheriff:check([5,5,2,-7,10000000],f()),
	true = sheriff:check([{[5,2,4]}],f()),
	false = sheriff:check([atom,atom2],f()).

test_g(_Config) ->
	true = sheriff:check(5,g()),
	true = sheriff:check([1,1,1,1,1],g()),
	false = sheriff:check(10,g()).

test_h(_Config) ->
	true = sheriff:check(-3,h()),
	true = sheriff:check({{{a,1,"a"}}},h()),
	false = sheriff:check({1},h()).

test_i(_Config) ->
	true = sheriff:check([atom,atom,[atom]],i()),
	false = sheriff:check([[2.3],atom],i()).
