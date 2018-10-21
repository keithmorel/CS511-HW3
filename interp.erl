-module(interp).
-export([scanAndParse/1,runFile/1,runStr/1]).
-include("types.hrl").

loop(InFile,Acc) ->
    case io:request(InFile,{get_until,prompt,lexer,token,[1]}) of
        {ok,Token,_EndLine} ->
            loop(InFile,Acc ++ [Token]);
        {error,token} ->
            exit(scanning_error);
        {eof,_} ->
            Acc
    end.

scanAndParse(FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop(InFile,[]),
    file:close(InFile),
    {Result, AST} = parser:parse(Acc),
    case Result of
        ok -> AST;
        _ -> io:format("Parse error~n")
    end.


-spec runFile(string()) -> valType().
runFile(FileName) ->
    valueOf(scanAndParse(FileName),env:new()).

scanAndParseString(String) ->
    {_ResultL, TKs, _L} = lexer:string(String),
    parser:parse(TKs).

-spec runStr(string()) -> valType().
runStr(String) ->
    {Result, AST} = scanAndParseString(String),
    case Result  of
        ok -> valueOf(AST,env:new());
        _ -> io:format("Parse error~n")
    end.


-spec numVal2Num(numValType()) -> integer().
numVal2Num({num, N}) ->
    N.

-spec boolVal2Bool(boolValType()) -> boolean().
boolVal2Bool({bool, B}) ->
    B.

-spec valueOf(expType(),envType()) -> valType().
valueOf(Exp,Env) ->
    case Exp of
        % A number
        {numExp, {num, _, Number}} ->
            Number;
        % An id
        {idExp, {id, _, Id}} ->
            env:lookup(Env, Id);

        % diff
        {diffExp, Exp1, Exp2} ->
            valueOf(Exp1,Env) - valueOf(Exp2,Env);

        % Plus
        {plusExp, Exp1, Exp2} ->
            valueOf(Exp1, Env) + valueOf(Exp2, Env);

        % App
        {appExp, {idExp, {id, _, Id}}, Value} ->
            case valueOf(env:lookup(Env, Id),Env) of
                {plusExp, Exp1, Value} ->
                    io:format("ok");
                {plusExp, Value, Exp2} ->
                    io:format("ok");
                {diffExp, Exp1, Exp2} ->
                    io:format("ok");
                {procExp, {id, _, Id1}, InnerExp} ->
                    valueOf(InnerExp, env:add(Env, Id1, valueOf(Value, Env)));
            end
        % isZero
        {isZeroExp, Exp1} ->
            valueOf(Exp1,Env) == 0;

        % let
        {letExp, {id,_,Key}, Exp1, InnerExp} ->
            valueOf(InnerExp,env:add(Env,Key,valueOf(Exp1,Env)));

        % proc
        {procExp, {id,_,Key}, InnerExp} ->
            env:add(Env, Key, {InnerExp, Env});

    end.
%% complete
