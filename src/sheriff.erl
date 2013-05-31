%% Copyright (c) 2012, Loïc Hoguin <essen@ninenines.eu>
%%
%% Based on the awesome original work and research
%% by William Dang and Hamza Mahmood.
%%
%% Copyright (c) 2011, William Dang <malliwi@hotmail.com>,
%%                     Hamza Mahmood <zar_roc@hotmail.fr>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(sheriff).
-compile({parse_transform, parse_trans_codegen}).

-export([check/2, parse_transform/2, build_type/3]).

%% @doc Return whether a value matches an Erlang type.
%%
%% This function takes two arguments: the value, and the type it will
%% be matched against. Currently the type must be given either as
%% an atom Type (for a local type of arity 0) or as a {Module, Type}
%% tuple (for an external type of arity 0). Built-in types or types
%% of other arities aren't supported as an argument at this point.
%% To work around this limitation you need to define your own base
%% type that will be a super-type of other built-in or user types.
%%
%% This function is actually a placeholder and will never be called,
%% as Sheriff will replace all the sheriff:check/2 calls by a call
%% to the generated validation function for the given type.
-spec check(any(), atom() | {atom(), atom()}) -> boolean().
check(<<"sheriff world">>, world) -> true;
check(_, _) -> false.

parse_transform(Forms, _Options) ->
	%% @todo We only need to go through the top-level stuff.
	{_, Types} = parse_trans:depth_first(
		fun retrieve_types/4, [], Forms, []),
	Module = parse_trans:get_module(Forms),
	Funcs = gen_check_funcs(Types, Module),
	Forms2 = insert_funcs(Forms, Funcs, Types),
	{Forms3, _} = parse_trans:depth_first(
		fun replace_calls/4, Module, Forms2, []),
	parse_trans:revert(Forms3).

retrieve_types(attribute, Form, _, Acc) ->
	case erl_syntax_lib:analyze_attribute(Form) of
		{type, {type, Type}} ->
			{Form, [Type|Acc]};
		_ ->
			{Form, Acc}
	end;
retrieve_types(_, Form, _, Acc) ->
	{Form, Acc}.

gen_check_funcs(Types, Module) ->
	gen_check_funcs(Types, Module, []).
gen_check_funcs([], Module, Acc) ->
	Func = codegen:gen_function('sherif_$_type_$_generic_$', 
		fun(Val, Type) when is_atom(Type) ->
			TypeFunc = list_to_atom("sheriff_$_type_$_" ++ atom_to_list(Type)),
			{'$var', Module}:TypeFunc(Val);
		(Val, Type) when is_list(Type) ->
			{ModulePart, TypePart} = try
				[ModulePart2, TypePart2] = string:tokens(Type, ":"),
				[TypePart3, ")"] = string:tokens(TypePart2, "("),
				{ModulePart2, TypePart3}
			catch error:{badmatch, _} ->
				error(badarg)
			end,
			ModuleAtom = list_to_atom(ModulePart),
			TypeAtom = list_to_atom(TypePart),
			'sherif_$_type_$_generic_$'(Val, {ModuleAtom, TypeAtom});
		(Val, {ModuleAtom, TypeAtom}) ->
			TypeFunc = list_to_atom("sheriff_$_type_$_" ++ atom_to_list(TypeAtom)),
			ModuleAtom:TypeFunc(Val)
		end),
	[Func | Acc];
gen_check_funcs([{{record, Name}, Tree, []}|Tail], Module, Acc) ->
	FuncName = record_to_func_name(Name),
	Value = {var, 0, 'Sheriff_check_value'},
	[Exprs] = build_record_exprs(Tree, Module, Value),
	Func = codegen:gen_function(FuncName, fun(Sheriff_check_value) ->
		{'$form', Exprs}
	end),
	gen_check_funcs(Tail, Module, [Func|Acc]);
%% Special cases for types aliasing any() or term() to avoid an
%% unnecessary warning with the variable being unused.
gen_check_funcs([{Name, {type, L, TypeName, []}, Args}|Tail], Module, Acc)
		when TypeName =:= any; TypeName =:= term ->
	FuncName = type_to_func_name(Name),
	FuncArity = 1 + length(Args),
	Value = {var, 0, '_Sheriff_check_value'},
	Func = {function, 0, FuncName, FuncArity, [
		{clause, 0, [Value|Args], [], [{atom, L, true}]}
	]},
	gen_check_funcs(Tail, Module, [Func|Acc]);
gen_check_funcs([{Name, Tree, Args}|Tail], Module, Acc) ->
	FuncName = type_to_func_name(Name),
	FuncArity = 1 + length(Args),
	Value = {var, 0, 'Sheriff_check_value'},
	Exprs = build_exprs([Tree], Module, Value),
	Func = {function, 0, FuncName, FuncArity, [
		{clause, 0, [Value|Args], [], Exprs}
	]},
	gen_check_funcs(Tail, Module, [Func|Acc]).

build_exprs(Types, Module, Value) ->
	build_exprs(Types, Module, Value, []).
build_exprs([], _, _, Acc) ->
	lists:reverse(Acc);
build_exprs([Type|Tail], Module, Value, Acc) ->
	Expr = build_type(Type, Module, Value),
	build_exprs(Tail, Module, Value, [Expr|Acc]).

build_record_exprs(Fields, Module, Value) ->
	[build_intersection(build_record_fields(Fields, Module, Value, 2, []))].

build_record_fields([], _, _, _, Acc) ->
	lists:reverse(Acc);
%% Ignore untyped record fields.
build_record_fields([Field|Tail], Module, Value, Pos, Acc)
		when element(1, Field) =:= record_field ->
	build_record_fields(Tail, Module, Value, Pos + 1, Acc);
build_record_fields([Field|Tail], Module, Value, Pos, Acc) ->
	Expr = build_record_field(Field, Module, Value, Pos),
	build_record_fields(Tail, Module, Value, Pos + 1, [Expr|Acc]).

build_record_field({typed_record_field, _, Type}, Module, Value, Pos) ->
	[Elem] = codegen:exprs(fun() ->
		element({'$var', Pos}, {'$form', Value})
	end),
	build_type(Type, Module, Elem).

%% Extract type information from annotated types.
build_type({ann_type, _, [{var, _, _}, Type]}, Module, Value) ->
	build_type(Type, Module, Value);
build_type(Expr = {atom, _, _}, _, Value) ->
	build_identity(Expr, Value);
build_type(Expr = {integer, _, _}, _, Value) ->
	build_identity(Expr, Value);
build_type(Expr = {op, _, '-', {integer, _, _}}, _, Value) ->
	build_identity(Expr, Value);
build_type({remote_type, _, [{atom, _, Module}, {atom, _, Type}, Args]},
		_, Value) ->
	FuncName = type_to_func_name(Type),
	[Exprs] = codegen:exprs(fun() ->
		apply({'$var', Module}, {'$var', FuncName},
			[{'$form', Value}] ++ {'$var', Args})
	end),
	Exprs;
build_type({type, L, any, []}, _, _) ->
	{atom, L, true};
build_type({type, L, arity, []}, Module, Value) ->
	build_type({type, L, range, [{integer, L, 0}, {integer, L, 255}]},
		Module, Value);
build_type({type, _, atom, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_atom({'$form', Value})
	end),
	Exprs;
build_type({type, L, binary, []}, Module, Value) ->
	build_type({type, L, binary, [{integer, L, 0}, {integer, L, 8}]},
		Module, Value);
%% This one is <<>> specifically.
build_type({type, L, binary, [{integer, _, 0}, {integer, _, 0}]},
		_, Value) ->
	build_identity({bin, L, []}, Value);
build_type({type, _, binary, [{integer, _, Size}, {integer, _, 0}]},
		_, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_bitstring({'$form', Value}) andalso
		bit_size({'$form', Value}) =:= {'$var', Size}
	end),
	Exprs;
build_type({type, _, binary, [{integer, _, MinSize}, {integer, _, Div}]},
		_, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_bitstring({'$form', Value}) andalso
		(bit_size({'$form', Value}) - {'$var', MinSize})
			rem {'$var', Div} =:= 0
	end),
	Exprs;
build_type({type, _, bitstring, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_bitstring({'$form', Value})
	end),
	Exprs;
build_type({type, _, boolean, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_boolean({'$form', Value})
	end),
	Exprs;
build_type({type, L, byte, []}, Module, Value) ->
	build_type({type, L, range, [{integer, L, 0}, {integer, L, 255}]},
		Module, Value);
build_type({type, L, char, []}, Module, Value) ->
	build_type({type, L, range, [{integer, L, 0}, {integer, L, 16#10ffff}]},
		Module, Value);
build_type({type, _, float, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_float({'$form', Value})
	end),
	Exprs;
%% We can only check the function arity when provided.
build_type({type, _, 'fun', [{type, _, product, Product}, _]}, _, Value) ->
	Arity = length(Product),
	[Exprs] = codegen:exprs(fun() ->
		is_function({'$form', Value}, {'$var', Arity})
	end),
	Exprs;
build_type({type, _, 'fun', _}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_function({'$form', Value})
	end),
	Exprs;
build_type({type, _, integer, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_integer({'$form', Value})
	end),
	Exprs;
%% @todo {type, _, iolist, []}
build_type({type, _, list, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_list({'$form', Value}) andalso
		({'$form', Value} =:= [] orelse is_list(tl({'$form', Value})))
	end),
	Exprs;
build_type({type, L, list, Types}, Module, Value) ->
	LCValue = {var, L, 'L'},
	InExprs = build_union(build_exprs(Types, Module, LCValue)),
	[Exprs] = codegen:exprs(fun() ->
		is_list({'$form', Value}) andalso
		({'$form', Value} =:= [] orelse is_list(tl({'$form', Value}))) andalso
		true =/= lists:member(false,
			[{'$form', InExprs} || {'$form', LCValue} <- {'$form', Value}])
	end),
	Exprs;
build_type({type, _, maybe_improper_list, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_list({'$form', Value})
	end),
	Exprs;
%% @todo
%build_type({type, L, maybe_improper_list,
%		[{integer, _, Type1}, {integer, _, Type2}]}, Module, Value) ->
%% @todo Same as the two above with nonempty_maybe_improper_list.
build_type({type, L, mfa, []}, Module, Value) ->
	build_tuple([{type, L, atom, []}, {type, L, atom, []},
		{type, L, byte, []}], Module, Value);
build_type({type, L, module, []}, Module, Value) ->
	build_type({type, L, atom, []}, Module, Value);
build_type({type, _, neg_integer, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_integer({'$form', Value}) andalso {'$form', Value} < 0
	end),
	Exprs;
build_type({type, _, nil, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		{'$form', Value} =:= []
	end),
	Exprs;
build_type({type, L, no_return, []}, Module, Value) ->
	build_type({type, L, none, []}, Module, Value);
build_type({type, L, node, []}, Module, Value) ->
	build_type({type, L, atom, []}, Module, Value);
build_type({type, _, nonempty_list, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_list({'$form', Value}) andalso
		{'$form', Value} =/= [] andalso
		is_list(tl({'$form', Value}))
	end),
	Exprs;
build_type({type, L, nonempty_list, Types}, Module, Value) ->
	LCValue = {var, L, 'L'},
	InExprs = build_union(build_exprs(Types, Module, LCValue)),
	[Exprs] = codegen:exprs(fun() ->
		is_list({'$form', Value}) andalso
		{'$form', Value} =/= [] andalso
		is_list(tl({'$form', Value})) andalso
		true =/= lists:member(false,
			[{'$form', InExprs} || {'$form', LCValue} <- {'$form', Value}])
	end),
	Exprs;
build_type({type, L, nonempty_string, []}, Module, Value) ->
	StrExpr = build_type({type, L, string, []}, Module, Value),
	[Exprs] = codegen:exprs(fun() ->
		{'$form', StrExpr} andalso {'$form', Value} =/= []
	end),
	Exprs;
build_type({type, _, non_neg_integer, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_integer({'$form', Value}) andalso 0 =< {'$form', Value}
	end),
	Exprs;
build_type({type, L, none, []}, _, _) ->
	{atom, L, false};
build_type({type, _, number, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_integer({'$form', Value}) orelse is_float({'$form', Value})
	end),
	Exprs;
build_type({type, _, pid, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_pid({'$form', Value})
	end),
	Exprs;
build_type({type, _, port, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_port({'$form', Value})
	end),
	Exprs;
build_type({type, _, pos_integer, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_integer({'$form', Value}) andalso 0 < {'$form', Value}
	end),
	Exprs;
build_type({type, _, range, [From, To]}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		{'$form', From} =< {'$form', Value} andalso
		{'$form', Value} =< {'$form', To}
	end),
	Exprs;
build_type({type, _, record, [{atom, _, Record}]}, _, Value) ->
	FuncName = record_to_func_name(Record),
	[Exprs] = codegen:exprs(fun() ->
		is_record({'$form', Value}, {'$var', Record}) andalso
		{'$var', FuncName}({'$form', Value})
	end),
	Exprs;
build_type({type, _, reference, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_reference({'$form', Value})
	end),
	Exprs;
build_type({type, L, string, []}, Module, Value) ->
	build_type({type, L, list, [{type, L, char, []}]}, Module, Value);
build_type({type, L, term, []}, Module, Value) ->
	build_type({type, L, any, []}, Module, Value);
build_type({type, _, timeout, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		{'$form', Value} =:= infinity orelse
		is_integer({'$form', Value}) andalso 0 =< {'$form', Value}
	end),
	Exprs;
build_type({type, _, tuple, any}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		is_tuple({'$form', Value})
	end),
	Exprs;
build_type({type, _, tuple, []}, _, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		{'$form', Value} =:= {}
	end),
	Exprs;
build_type({type, _, tuple, Types}, Module, Value) ->
	build_tuple(Types, Module, Value);
build_type({type, _, union, Types}, Module, Value) ->
	build_union(build_exprs(Types, Module, Value));
build_type({type, _, Custom, Args}, Module, Value) ->
	FuncName = type_to_func_name(Custom),
	[Exprs] = codegen:exprs(fun() ->
		apply({'$var', Module}, {'$var', FuncName},
			[{'$form', Value}] ++ {'$var', Args})
	end),
	Exprs;
build_type({var, L, '_'}, Module, Value) ->
	build_type({type, L, any, []}, Module, Value);
%% For type parameters, we dynamically obtain the validation AST
%% for the parameter, then eval it and return the result.
build_type(Expr = {var, _, _}, Module, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		%% Hide the variable Result in a fun to avoid conflicts
		%% when a type has more than one parameter.
		(fun() ->
			{value, Result, []} = erl_eval:exprs(
				[sheriff:build_type(
					{'$form', Expr},
					{'$var', Module},
					erl_parse:abstract({'$form', Value}))],
				[]),
			Result
		end)()
	end),
	Exprs.

build_identity(Expr, Value) ->
	[Exprs] = codegen:exprs(fun() ->
		{'$form', Expr} =:= {'$form', Value}
	end),
	Exprs.

build_tuple(Types = [Type|Tail], Module, Value) ->
	Size = length(Types),
	First = build_tuple_element(Type, Module, Value, 1),
	Elems = build_tuple_elements(Tail, Module, Value, 2, First),
	[Exprs] = codegen:exprs(fun() ->
		is_tuple({'$form', Value}) andalso
		size({'$form', Value}) =:= {'$var', Size} andalso
		{'$form', Elems}
	end),
	Exprs.

build_tuple_elements([], _, _, _, Expr) ->
	Expr;
build_tuple_elements([Type|Tail], Module, Value, N, Expr2) ->
	Expr1 = build_tuple_element(Type, Module, Value, N),
	[Elems] = codegen:exprs(fun() ->
		{'$form', Expr1} andalso {'$form', Expr2}
	end),
	build_tuple_elements(Tail, Module, Value, N + 1, Elems).

build_tuple_element(Type, Module, Value, N) ->
	[Elem] = codegen:exprs(fun() ->
		element({'$var', N}, {'$form', Value})
	end),
	build_type(Type, Module, Elem).

build_intersection([Expr|Tail]) ->
	build_intersection(Tail, Expr).
build_intersection([], Expr) ->
	Expr;
build_intersection([Expr1|Tail], Expr2) ->
	[Intersection] = codegen:exprs(fun() ->
		{'$form', Expr1} andalso {'$form', Expr2}
	end),
	build_intersection(Tail, Intersection).

build_union([Expr|Tail]) ->
	build_union(Tail, Expr).
build_union([], Expr) ->
	Expr;
build_union([Expr1|Tail], Expr2) ->
	[Union] = codegen:exprs(fun() ->
		{'$form', Expr1} orelse {'$form', Expr2}
	end),
	build_union(Tail, Union).

insert_funcs(Forms, Funcs, Types) ->
	Forms2 = parse_trans:do_insert_forms(below, Funcs, Forms,
		parse_trans:initial_context(Forms, [])),
	lists:foldl(fun({Type, _, Args}, FormsAcc) ->
		case Type of
			{record, _} ->
				FormsAcc;
			Type ->
				FuncName = type_to_func_name(Type),
				FuncArity = 1 + length(Args),
				parse_trans:export_function(FuncName, FuncArity, FormsAcc)
		end
	end, Forms2, Types).

replace_calls(application, Form, _Ctx, ThisModule) ->
	case erl_syntax_lib:analyze_application(Form) of
		{sheriff, {check, 2}} ->
			Pos = erl_syntax:get_pos(Form),
			Args = erl_syntax:application_arguments(Form),
			Vars = [hd(Args)],
			[CheckVar, TypeVar] = parse_trans:revert(Args),
			Form2 = case TypeVar of
				{var, _, _} ->
					erl_syntax:application(
						erl_syntax:atom('sherif_$_type_$_generic_$'), 
						Args); 
				{string, _, String} ->
					{ok, Ts, _} = erl_scan:string(
						"-type sheriff_string_arg() :: " ++ String ++ "."),
					{ok, {attribute, _, type, {sheriff_string_arg, Type, []}}}
						= erl_parse:parse_form(Ts),
					build_type(Type, ThisModule, CheckVar);
				{tuple, _, [{atom, _, Module}, {atom, _, Type}]} ->
					FuncName = type_to_func_name(Type),
					erl_syntax:application(erl_syntax:atom(Module),
						erl_syntax:atom(FuncName), Vars);
				{atom, _, Type} ->
					FuncName = type_to_func_name(Type),
					erl_syntax:application(erl_syntax:atom(FuncName), Vars)
			end,
			Form3 = erl_syntax:set_pos(Form2, Pos),
			{Form3, ThisModule};
		_ ->
			{Form, ThisModule}
	end;
replace_calls(_, Form, _Ctx, ThisModule) ->
	{Form, ThisModule}.

type_to_func_name(Type) when is_atom(Type) ->
	list_to_atom("sheriff_$_type_$_" ++ atom_to_list(Type)).

record_to_func_name(Record) when is_atom(Record) ->
	list_to_atom("sheriff_$_record_$_" ++ atom_to_list(Record)).
