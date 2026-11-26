#!/usr/bin/env escript
%%! +S 1:1 +A 1 -noinput
-mode(compile).

main([File, Output | Args]) ->
    try
        case erlang:system_info(otp_release) of
            "27" -> ok;
            Other -> error({requires_otp_27, Other})
        end,
        {Includes, Macros} = options(Args, [], []),
        Opts = [{includes, Includes}, {macros, Macros}, {location, {1,1}}],
        case epp:parse_file(File, Opts) of
            {ok, Forms} ->
                %% epp can embed errors between successful forms.
                check_forms(Forms, File),
                Bifs = [[atom_to_binary(Name), Arity] || {Name,Arity} <- erlang:module_info(exports),
                                                        erl_internal:bif(Name,Arity)],
                Json = #{forms => [encode(Form) || Form <- Forms], autoImports => lists:sort(Bifs)},
                ok = file:write_file(Output, json:encode(Json));
            {error, Reason} ->
                parse_error(File, Reason)
        end
    catch
        throw:{source_error, Path, Detail} ->
            Message = unicode:characters_to_binary(io_lib:format("~ts: ~ts", [Path,Detail])),
            ok = file:write_file(Output, json:encode(#{sourceError => Message})),
            halt(2);
        Class:Reason0 ->
            io:format(standard_error, "erlang2cpg OTP helper: ~tp:~tp~n", [Class,Reason0]),
            halt(1)
    end.

options(["-I", Include | Rest], Includes, Macros) ->
    options(Rest, [Include | Includes], Macros);
options(["-D", Macro | Rest], Includes, Macros) ->
    options(Rest, Includes, [macro(Macro) | Macros]);
options([], Includes, Macros) ->
    {lists:reverse(Includes), lists:reverse(Macros)}.

macro(Definition) ->
    case string:split(Definition, "=") of
        [Name] when Name =/= [] -> list_to_atom(Name);
        [Name, Val] when Name =/= [] ->
            {ok, Tokens, _} = erl_scan:string(Val ++ "."),
            {ok, Literal} = erl_parse:parse_term(Tokens),
            {list_to_atom(Name), Literal}
    end.

check_forms([{attribute,_,file,{Path,_}} | Rest], _) -> check_forms(Rest, Path);
check_forms([{error,Reason} | _], File) -> parse_error(File, Reason);
check_forms([_ | Rest], File) -> check_forms(Rest, File);
check_forms([], _) -> ok.

parse_error(File, {Loc, Module, Detail}) ->
    throw({source_error, File, io_lib:format("~p: ~ts", [Loc, Module:format_error(Detail)])});
parse_error(File, Reason) ->
    throw({source_error, File, io_lib:format("~tp", [Reason])}).

%% Tags distinguish atoms and binaries, lists and tuples, and preserve integers
%% with more than 53 significant bits across the JSON transport.
encode(Value) when is_atom(Value) -> [<<"atom">>, atom_to_binary(Value)];
encode(Value) when is_integer(Value) -> [<<"integer">>, integer_to_binary(Value)];
encode(Value) when is_float(Value) -> [<<"float">>, float_to_binary(Value, [short])];
encode(Value) when is_binary(Value) -> [<<"binary">>, base64:encode(Value)];
encode(Value) when is_bitstring(Value) ->
    Padding = (8 - bit_size(Value) rem 8) rem 8,
    [<<"bits">>, bit_size(Value), base64:encode(<<Value/bitstring, 0:Padding>>)];
encode(Value) when is_tuple(Value) -> [<<"tuple">>, [encode(Item) || Item <- tuple_to_list(Value)]];
encode(Value) when is_map(Value) ->
    [<<"map">>, [[encode(Key), encode(Item)] || {Key,Item} <- lists:sort(maps:to_list(Value))]];
encode(Value) when is_list(Value) -> encode_list(Value, []).

encode_list([Head | Tail], Items) -> encode_list(Tail, [encode(Head) | Items]);
encode_list([], Items) -> [<<"list">>, lists:reverse(Items)];
encode_list(Tail, Items) -> [<<"improper">>, lists:reverse(Items), encode(Tail)].
