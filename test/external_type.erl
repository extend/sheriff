-module(external_type).
-compile({parse_transform, sheriff}).

-export_type([a/0]).
-type a() :: atom() | list() | tuple().

-export_type([b/2]).
-type b(A, B) :: atom() | {A, B}.
