-module(sheriff_test_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]). %% ct.
-export([test_a/1,test_b/1,test_c/1,test_d/1,test_e/1,test_f/1,test_g/1,test_h/1
		,test_i/1]).%,test_j/1,test_k/1,test_l/1,test_n/1,test_o/1,test_q/1,test_r/1
		%,test_s/1,test_t/1]). %% Tests.

-compile({parse_transform, sheriff}).




%% ct.

all() ->
[{group, sheriff}].

groups() ->
[{sheriff, [], [test_a,test_b,test_c,test_d,test_e,test_f,test_g,test_h,test_i]}].
				%,test_j,test_k,test_l,test_n,test_o,test_q,test_r,test_s,test_t]}].

%% Tests.

-type a() :: atom()|list()|tuple().
test_a(_Config) ->
	true = sheriff:check([2,{5},2.2,at,"p"],a()),
	true = sheriff:check({[2,{5},2.2,at,"p"]},a()),
	true = sheriff:check(at,a()),
	false = sheriff:check(2.3,a()).

-type b() :: [1|a()].
test_b(_Config) ->
	true = sheriff:check([1],b()),
	true = sheriff:check([{[2,{5},2.2,at,"p"]}],b()),
	false = sheriff:check(2.3,b()).

-type c(A) :: [{A}|d()].
test_c(_Config) ->
	true = sheriff:check([{1}],c(integer())),
	true = sheriff:check([[{atom}],[[],[a,"b"]]],c(integer())),
	true = sheriff:check([{{2.3}}],c(a())),
	false = sheriff:check([{2.3}],c(a())).


-type d() :: [{a()}|list()].
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


-type f() :: [integer()|{f()}].
test_f(_Config) ->
	true = sheriff:check([5,5,2,-7,10000000],f()),
	true = sheriff:check([{[5,2,4]}],f()),
	false = sheriff:check([atom,atom2],f()).

-type g() :: external_type:e(b()).
test_g(_Config) ->
	true = sheriff:check(5,g()),
	true = sheriff:check([1,1,1,1,1],g()),
	false = sheriff:check(10,g()).

-type h() :: -5..5|{tuple(tuple())}.
test_h(_Config) ->
	true = sheriff:check(-3,h()),
	true = sheriff:check({{{a,1,"a"}}},h()),
	false = sheriff:check({1},h()).

-type i() :: [i()|atom()].
test_i(_Config) ->
	true = sheriff:check([atom,atom,[atom]],i()),
	false = sheriff:check([[2.3],atom],i()).

%-type j() :: {any()}.
%test_j(_Config) ->
%	true = sheriff:check({atom},j()),
%	false = sheriff:check({1,"a"},j()).

%-type k() :: pid()|maybe_improper_list().
%test_k(_Config) ->
%	%% @todo pid()
%	true = sheriff:check([a,1],k()),
%	true = sheriff:check([a|1],k()).

%-type l() :: port()|{number()}|maybe_improper_list(float()).
%test_l(_Config) ->
%	%% @todo port()
%	true = sheriff:check({1},l()),
%	true = sheriff:check({1.2},l()),
%	true = sheriff:check([1.2|4],l()).

%-type m() :: reference().
%% @todo function test_m

%-type n() :: binary()|pos_integer()|string().
%test_n(_Config) ->
%	true = sheriff:check(<<1>>,n()),
%	true = sheriff:check(1,n()),
%	true = sheriff:check("popopo",n()),
%	false = sheriff:check(atom,n()).

%-type o() :: <<_:2>>|<<_:8, _:_*3>>|<<_:_*10>>.
%test_o(_Config) ->
%	true = sheriff:check(<<454:2>>,o()),
%	true = sheriff:check(<<454:8,341:9>>,o()),
%	true = sheriff:check(<<454:20>>,o()),
%	false = sheriff:check(<<454:5>>,o()).

%-type p() :: [{float(),fun(() -> list())}]|fun((term()) -> non_neg_integer()).
%% @todo function test_p

%-type q() :: [boolean()]|[byte()]|nonempty_string().
%test_q(_Config) ->
%	true = sheriff:check([true,'true'],q()),
%	true = sheriff:check([0,45,255,54,2],q()),
%	true = sheriff:check("azerty",q()),
%	false = sheriff:check([0,45,255,54,2000],q()).

%-type r() :: [char()]|[neg_integer()].
%test_r(_Config) ->
%	true = sheriff:check([53,"t"],r()),
%	true = sheriff:check([-8000,-1],r()),
%	false = sheriff:check(["t",-5],r()).

%-type s() :: iolist()|module()|mfa().
%test_s(_Config) ->
%	true = sheriff:check([21,"e","*",<<32>>],s()),
%	true = sheriff:check(atom,s()),
%	true = sheriff:check({atom,atom,123},s()),
%	false = sheriff:check({atom,atom,-1},s()).

%-type t() :: external_type:e(node()|[timeout()]|no_return()|[20..95]).
%test_t(_Config) ->
%	true = sheriff:check(atom,t()),
%	true = sheriff:check(['infinity',211],t()),
	%% @todo check no_return()
%	true = sheriff:check([26,85,95,21],t()),
%	false = sheriff:check("a",t()).