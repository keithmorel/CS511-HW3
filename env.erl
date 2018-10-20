-module(env).
-compile(export_all).
-include("types.hrl").


-spec new()-> envType().
new() ->
    {}.

-spec add(envType(),atom(),valType())-> envType().
add(Env,Key,Value) ->
    append(Key, Value, Env).

-spec lookup(envType(),atom())-> valType().
lookup(Env,Key) -> 
   fetch(Key, Env);

