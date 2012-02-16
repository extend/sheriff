-module(sheriff_SUITE).
-compile({parse_transform, sheriff}).
-export([all/0, groups/0]).
-export([
	t_all/1,
	t_custom_a/1, t_custom_b/1, t_custom_c/1, t_custom_d/1,
	t_custom_e/1, t_custom_f/1, t_custom_g/1, t_custom_h/1,
	t_custom_i/1, t_custom_j/1, t_custom_k/1, t_custom_l/1,
	t_custom_m/1, t_custom_n/1, t_custom_o/1, t_custom_p/1,
	t_custom_q/1, t_custom_r/1, t_custom_s/1,
	t_external_a/1, t_external_b/1, t_external_c/1,
	t_external_d/1, t_external_e/1,
	t_string_a/1]).

-include_lib("common_test/include/ct.hrl").

%% Export the types just to get rid of warnings.
-export_type([
	my_type/0, all/0,
	a/0, b/0, c/0, d/0, e/0, f/0, g/0, h/0,
	i/0, j/0, k/0, l/0, m/0, n/0, o/0, p/0,
	q/0, r/0, s/0,
	external_b/0, external_c/0, external_d/0,
	external_e/0]).

-record(my_record, {
	id :: integer(),
	value = 2 :: integer(),
	bad_default = undefined :: integer()
}).

%% These two types are just used to check we can compile any type check.
-type my_type() :: 1 | 3 | 5 | 7 | 9.
-type all() ::
	%% Literal values and notations.
	_ | %% any()
	an_atom |
	<<>> |
	<< _:3 >> |
	<< _:_*5 >> |
	<< _:3, _:_*5 >> |
	-42 |
	42 |
	[] |
	[integer()] |
	[byte(), ...] |
	-10..-5 |
	5..100 |
	{} |
	{atom(), integer(), float()} |

	%% Built-in types.
	any() |
	arity() | %% 0..255
	atom() |
	binary() | %% << _:0, _:_*8 >>
	binary(3, 5) | %% << _:3, _:_*5 >>
	bitstring() |
	boolean() | %% true | false
	byte() | %% 0..255
	char() | %% 0..16#10ffff
	float() |
	fun() |
	fun(() -> integer()) |
	fun((...) -> integer()) |
	fun((atom(), tuple()) -> integer()) |
	integer() |
%	iodata() | %% @todo
%	iolist() | %% @todo
	list() | %% [any()]
	list(integer()) | %% [integer()]
	maybe_improper_list() |
%	maybe_improper_list(atom(), integer()) | %% @todo
	mfa() | %% {atom(), atom(), arity()}
	module() | %% atom() only, because tuple() is deprecated
	neg_integer() | %% ..-1
	nil() | %% []
	no_return() | %% none()
	node() | %% atom()
	none() |
	nonempty_list() | %% [any(), ...]
	nonempty_list(byte()) | %% [byte(), ...]
%	nonempty_improper_list(atom(), integer()) | %% @todo
%	nonempty_maybe_improper_list() | %% @todo
%	nonempty_maybe_improper_list(atom(), integer()) | %% @todo
	nonempty_string() | %% [byte(), ...]
	non_neg_integer() | %% 0..
	number() |
	pid() |
	port() |
	pos_integer() | %% 1..
	range(5, 100) | %% 5..100
	reference() |
	string() | %% [byte()]
	term() | %% any()
	timeout() | %% 0.. | infinity
	tuple() |
	tuple(atom()) | %% {atom()}
	tuple(atom(), integer()) | %% {atom(), integer()}
	tuple(atom(), integer(), float()) | %% {atom(), integer(), float()}

	%% Variables.
	%% A | B | C

	%% User-defined records and types.
	#my_record{} |
	my_type().

all() ->
	[{group, check}].

groups() ->
	[{check, [], [
		t_all,
		t_custom_a, t_custom_b, t_custom_c, t_custom_d,
		t_custom_e, t_custom_f, t_custom_g, t_custom_h,
		t_custom_i, t_custom_j, t_custom_k, t_custom_l,
		t_custom_m, t_custom_n, t_custom_o, t_custom_p,
		t_custom_q, t_custom_r, t_custom_s,
		t_external_a, t_external_b, t_external_c,
		t_external_d, t_external_e,
		t_string_a
	]}].

t_all(_) ->
	true = sheriff:check([{"a", b}], all).

-type a() :: atom() | list() | tuple().
t_custom_a(_) ->
	true = sheriff:check([2, {5}, 2.2, at, "p"], a),
	true = sheriff:check({[2, {5}, 2.2, at, "p"]}, a),
	true = sheriff:check(at, a),
	false = sheriff:check(2.3, a).

-type b() :: [1 | a()].
t_custom_b(_) ->
	true = sheriff:check([1], b),
	true = sheriff:check([{[2, {5}, 2.2, at, "p"]}], b),
	false = sheriff:check(2.3, b).

-type c() :: [{a()} | list()].
t_custom_c(_) ->
	true = sheriff:check([{[a]}], c),
	true = sheriff:check([[azerty], [qsd]], c),
	false = sheriff:check(1.2, c).

-type d() :: [integer() | {d()}].
t_custom_d(_) ->
	true = sheriff:check([5, 5, 2, -7, 10000000], d),
	true = sheriff:check([{[5, 2, 4]}], d),
	false = sheriff:check([atom, atom2], d).

-type e() :: -5..5 | {tuple(tuple())}.
t_custom_e(_) ->
	true = sheriff:check(-3, e),
	true = sheriff:check({{{a, 1, "a"}}}, e),
	false = sheriff:check({1}, e).

-type f() :: [f() | atom()].
t_custom_f(_) ->
	true = sheriff:check([atom, atom, [atom]], f),
	false = sheriff:check([atom, atom, [atom, [[[[[[[1]]]]]]]]], f),
	false = sheriff:check([[2.3], atom], f).

-type g() :: {any()}.
t_custom_g(_) ->
	true = sheriff:check({atom}, g),
	false = sheriff:check({1, "a"}, g).

-type h() :: binary() | pos_integer() | string().
t_custom_h(_) ->
	true = sheriff:check(<<1>>, h),
	true = sheriff:check(1, h),
	true = sheriff:check("popopo", h),
	false = sheriff:check(atom, h).

-type i() :: [byte(), ...].
t_custom_i(_) ->
	true = sheriff:check([0, 45, 255, 54, 2], i),
	true = sheriff:check("azerty", i),
	false = sheriff:check("", i),
	false = sheriff:check([0, 45, 255, 54, 2000], i).

-type j() :: [char()] | [neg_integer()].
t_custom_j(_) ->
	true = sheriff:check("azerty" ++ [2000], j),
	true = sheriff:check([-8000, -1], j),
	false = sheriff:check(["t", -5], j).

-type list_of(A) :: list(A).
-type k() :: list_of(integer()).
t_custom_k(_) ->
	true = sheriff:check([1, 2, 3], k),
	true = sheriff:check([42, 80085, 999999999999999], k),
	true = sheriff:check("it works!", k),
	false = sheriff:check(atom, k),
	false = sheriff:check([atom], k),
	false = sheriff:check(["it shouldn't"], k).

-type l_base(A) :: [{A, boolean()}|c()].
-type l() :: l_base(integer()).
t_custom_l(_) ->
	true = sheriff:check([[{atom}]], l),
	true = sheriff:check([[{atom}], [[], [a, "b"]], {42, false}], l),
	false = sheriff:check(42, l).

-type m() :: pid() | reference() | none.
t_custom_m(_) ->
	true = sheriff:check(self(), m),
	true = sheriff:check(make_ref(), m),
	true = sheriff:check(none, m),
	false = sheriff:check(other, m),
	false = sheriff:check({self(), none}, m).

-type n() :: fun((integer(), atom()) -> tuple()).
t_custom_n(_) ->
	true = sheriff:check(fun(A, B) -> {A, B} end, n),
	false = sheriff:check(fun() -> ok end, n),
	false = sheriff:check(atom, n).

-type o() :: list_of(a()).
t_custom_o(_) ->
	true = sheriff:check([atom, [list, 42], {tuple}], o),
	true = sheriff:check([a, b, c, d, e], o),
	false = sheriff:check(atom, o),
	false = sheriff:check([atom, [list], {tuple}, 42], o).

-type p() :: <<>>.
t_custom_p(_) ->
	true = sheriff:check(<<>>, p),
	false = sheriff:check(<< 0:2 >>, p),
	false = sheriff:check(<< 0:8 >>, p),
	false = sheriff:check(atom, p).

-type q() :: << _:4, _:_*8 >>.
t_custom_q(_) ->
	true = sheriff:check(<< 1:4 >>, q),
	true = sheriff:check(<< 1:4, 0:8, 9:8 >>, q),
	false = sheriff:check(<< 1:4, 0:7 >>, q),
	false = sheriff:check(<< 1:3, 0:8 >>, q),
	false = sheriff:check(atom, q).

-type r() :: list(integer()).
t_custom_r(_) ->
	true = sheriff:check([2, 3], r),
	true = sheriff:check([2|[3]], r),
	true = sheriff:check([2, 3|[]], r),
	false = sheriff:check([2|3], r).

-type s() :: maybe_improper_list().
t_custom_s(_) ->
	true = sheriff:check([2, 3], s),
	true = sheriff:check([2|[3]], s),
	true = sheriff:check([2, 3|[]], s),
	true = sheriff:check([2|3], s),
	false = sheriff:check(atom, s).

%% Note that external_type is the name of the module, not a keyword.
t_external_a(_) ->
	true = sheriff:check(test, {external_type, a}),
	false = sheriff:check(1.2, {external_type, a}).

-type external_b() :: external_type:b(atom(), integer()).
t_external_b(_) ->
	true = sheriff:check(test, external_b),
	true = sheriff:check({test, 42}, external_b),
	false = sheriff:check({42, test}, external_b).

-type external_c_base(A, B) :: [{A, B}].
-type external_c() :: external_c_base(external_type:a(), boolean()).
t_external_c(_) ->
	true = sheriff:check([{test, true}], external_c),
	false = sheriff:check(test, external_c),
	false = sheriff:check([{4.2, true}], external_c).

-type external_d() :: external_type:b(external_type:a(), integer()).
t_external_d(_) ->
	true = sheriff:check({[list], 42}, external_d),
	true = sheriff:check({atom, 42}, external_d),
	true = sheriff:check(atom, external_d),
	false = sheriff:check({4.2, 42}, external_d).

-type external_e() :: external_type:b(a(), integer()).
t_external_e(_) ->
	true = sheriff:check({[list], 42}, external_e),
	true = sheriff:check({atom, 42}, external_e),
	true = sheriff:check(atom, external_e),
	false = sheriff:check({4.2, 42}, external_e).

t_string_a(_) ->
	true = sheriff:check([a, b, c], "list(atom())"),
	true = sheriff:check(test, "external_type:a()"),
	true = sheriff:check([1, 2, 3], "list_of(integer())"),
	false = sheriff:check({a, b}, "{atom(), integer()}").
