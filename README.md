Sheriff
=======

Sheriff is a parse transform that allows developers to check
values against their type as defined through typespecs.

Sheriff generates validation functions for all the types in
the module being compiled and then replaces all the sheriff:check/2
calls with calls to these validation functions.

Sheriff should be used where Dialyzer cannot do anything: when
receiving external data, for example when reading from a file,
a socket or receiving a process message.

Currently Sheriff should support all types excluding iolist/0
and maybe_improper_list/2. The main limitation is that it can
only work with modules compiled with the Sheriff parse transform.

Also note that Sheriff does not work with opaque types. If you
try to check an opaque type, the file won't compile. If you try
to check a type which include an opaque type, a runtime error
will be produced.

Usage
-----

To compile a module with the Sheriff parse transform, simply add
the following line at the top of your module:

``` erlang
-compile({parse_transform, sheriff}).
```

This compilation option can also be defined project-wide using
rebar.config or the Erlang compiler.

Type Check
----------

To check that a value matches a type, you need to first define
a type, then call sheriff:check/2 with the value and the type
as arguments.

``` erlang
-type colors() :: blue | red | green | yellow.

paint(Color, Object) ->
	case sheriff:check(Color, colors) of
		true ->
			do_paint(Color, Object);
		false ->
			{error, badarg}
	end.
```

Many times you will probably want to let it crash, though.

``` erlang
-type colors() :: blue | red | green | yellow.

paint(Color, Object) ->
	true = sheriff:check(Color, colors),
	do_paint(Color, Object).
```

You can check records. All the typed record values will be
checked, along with making sure the value is a record of the
expected type. To check for recordness, you must first define a
type specifically for the record.

``` erlang
-type paintable_object() :: #paintable_object{}.

paint(Color, Object) ->
	true = sheriff:check(Color, colors),
	true = sheriff:check(Object, paintable_object),
    do_paint(Color, Object).
```

You can also check against a remote type.

``` erlang
paint(Color, Object) ->
	true = sheriff:check(Color, {picasso_module, colors}),
	do_paint(Color, Object).
```

You can finally use the inline notation. You can specify any
built-in, local or remote type in a string and pass it to
sheriff:check/2.

``` erlang
paint(Color, Object) ->
	true = sheriff:check(Color, "picasso_module:colors()"),
	do_paint(Color, Object).

erase(Pixels, Object) ->
	true = sheriff:check(Pixels, "list({integer(), integer()})"),
	do_erase(Pixels, Object).
```

Note that when passing atoms or tuples for the type to check
against, Sheriff does not currently accept built-in types as
arguments, only local or remote types. Also note that all types
must be of arity 0, as sheriff:check/2 can only accept type
names as argument at this time. This is a limitation only on
the function call, not on the type specifications. You can use
the inline notation to overcome it.

``` erlang
%% This type cannot be passed to sheriff:check/2.
-type a(A, B) :: [{A, B}].

%% These types can be passed to sheriff:check/2.
-type b() :: a(atom(), integer()).
-type c() :: list(integer()).
-type d() :: picasso_module:colors().
```

Thanks
------

Sheriff is available through the initial work and research
by the students William Dang and Hamza Mahmood.
