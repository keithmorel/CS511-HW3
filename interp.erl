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
            {num, Number};

        % An id
        {idExp, {id, _, Id}} ->
            env:lookup(Env, Id);

        {proc, Var, InnerExp, Dict} ->
            {Var, InnerExp, Dict};

        % diff
        {diffExp, Exp1, Exp2} ->
            {num, numVal2Num(valueOf(Exp1,Env)) - numVal2Num(valueOf(Exp2,Env))};

        % Plus
        {plusExp, Exp1, Exp2} ->
            {num, numVal2Num(valueOf(Exp1, Env)) + numVal2Num(valueOf(Exp2, Env))};

        % If
        {ifThenElseExp, Exp1, Exp2, Exp3} ->
	    case boolVal2Bool(valueOf(Exp1, Env)) of
		true ->
		    valueOf(Exp2, Env);
		false ->
		    valueOf(Exp3, Env)
            end;

        % App
        {appExp, {idExp, {id, _, Id}}, Value} ->
            case valueOf(env:lookup(Env, Id),Env) of
                {Var, InnerExp, Dict} ->
                    valueOf(InnerExp, env:add(Dict,Var,valueOf(Value,Env)))
            end;
        
        % isZero
        {isZeroExp, Exp1} ->
            {bool, numVal2Num(valueOf(Exp1,Env)) == 0};

        % let
        {letExp, {id,_,Key}, Exp1, InnerExp} ->
            valueOf(InnerExp,env:add(Env,Key,valueOf(Exp1,Env)));

        % proc
        {procExp, {id,_,Key}, InnerExp} ->
            {proc, Key, InnerExp, Env}

    end.
%% complete
