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
        {diffExp, {numExp, {num, _, Number1}}, {numExp, {num, _, Number2}}} ->
            Number1 - Number2;
        {diffExp, {numExp, {num, _, Number}}, {idExp, {id, _, Id}}} ->
            Number - env:lookup(Env,Id);
        {diffExp, {idExp, {id, _, Id}}, {numExp, {num, _, Number}}} ->
            env:lookup(Env,Id) - Number;

        % Plus
        {plusExp, {numExp, {num, _, Number1}}, {numExp, {num, _, Number2}}} ->
            Number1 + Number2;
        {plusExp, {numExp, {num, _, Number}}, {idExp, {id, _, Id}}} ->
            env:lookup(Env,Id) + Number;
        {plusExp, {idExp, {id, _, Id}}, {numExp, {num, _, Number}}} ->
            env:lookup(Env,Id) + Number;

        % App
        {appExp, {idExp, {id, _, Id}}, {_, {_,_,Value}}} ->
            case env:lookup(Env, Id) of
                ok ->
                    io:format("hey")
                %{plusExp,{idExp,{id,1,Id}},{numExp,{num,1,Number}}} ->
                    %io:format("heyhey")
                    %valueOf({plusExp, {idExp, {id, 1, Id}}, {numExp, {num, 1, Number}}}, Env)

                %{proc, Var, Exp1, Env1} ->
                    %io:format("hello")
            end;

        % isZero
        {isZeroExp, {numExp, {num, _, Number}}} ->
            Number == 0;
        {isZeroExp, {_, {id, _, Id}}} ->
            env:lookup(Env, Id) == 0;

        % let
        {letExp, {id,_,Key}, {numExp,{num,_,Value}}, InnerExp} ->
            valueOf(InnerExp,env:add(Env,Key,Value));

        {letExp, {id,_,Key}, Proc, InnerExp} ->
            valueOf(InnerExp,env:add(Env,Key,Proc));

        % proc
        %{procExp, {id,_,Key}, InnerExp} ->
            %{proc, Key, InnerExp, Env};
        {procExp, {id,_,Key}, InnerExp} ->
            case InnerExp of
                {plusExp, {idExp, {id, _, Key}}, {numExp, {num, _, Number}}} ->
                    io:format("hello1");
                {plusExp, {idExp, {id, _, Key1}}, {idExp, {id, _, Key2}}} ->
                    io:format("hello2")
            end

    end.
%% complete
