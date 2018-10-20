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
    case Exp of ->
        {numExp, 1, int} ->
            numVal2Num(int);
        {idExp, 1, id} ->
            id.
        {diffExp, exp1, exp2} ->
            valueOf(exp1, _Env) - valueOf(exp2, _Env);
        {plusExp, exp1, exp2} ->
            valueOf(exp1, _Env) + valueOf(exp2, _Env);
        {isZeroExp, bool} ->
            boolVal2Bool(valueOf(bool, _Env));
        {ifThenElseExp, exp1, exp2, exp3} ->
            if
                boolVal2Bool(valueOf(exp1, _Env)) ->
                    valueOf(exp2, _Env);
                _ ->
                    valueOf(exp3, _Env);
            end.
        {letExp, id, exp1, exp2} ->
            valueOf(exp2, env:add(_Env, id, valueOf(exp1)));
        {procExp, id, exp} ->
            io:format("Not Implemented.");
        {appExp, exp1, exp2} ->
            io:format("Not Implemented.");
        {exp} ->
            valueOf(exp, _Env);
    end.

        
