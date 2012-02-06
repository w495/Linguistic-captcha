-module(captcha).

-export([new/0, new/1, new/3, check/2, remove_expired/0]).

%%% --------------------------------------------------------------------------
%%% API
%%% --------------------------------------------------------------------------
new() ->
    new({ngram, {2, 2}, {3, 3}}).

new(Opt) ->
    new(Opt, 200, 36).

new(Opt, Size, Pointsize) ->
    File_name = lists:flatmap(
        fun(Item) -> integer_to_list(Item) end,
        tuple_to_list(now())
    ),

    Code = generate_rand(Opt),

    File = io_lib:format("/tmp/~s.png",[File_name]),

    Cmd = io_lib:format(
        "convert -background 'none' -fill '#222222' -size ~p -gravity Center "
        "-wave 5x~p -swirl ~p -font DejaVu-Serif-Book -pointsize ~p label:\"~s\""
        " -draw 'Bezier 10,40 50,35 100,35 150,35 200,50 250,35 300,35' ~s",
            [Size, random:uniform(50)+50, random:uniform(20)+5, Pointsize, Code, File]),

    os:cmd(Cmd),

    Code_up = string:to_upper(Code),

    io:format("Code_up = ~p~n", [Code_up]),

    Md5 = erlang:md5(Code_up),

    Now = gs_us(),
    true = ets:insert(captcha, {Md5, {Code_up, Now}}),
    true = ets:insert(captcha_time, {Now, Md5}),

    {ok, Bin_png} = file:read_file(File),
    file:delete(File),

    Code_hex = mochihex:to_hex(Md5),
    {Code_hex, Bin_png}.

check(Code_hex, Code) when Code_hex =:= undefined; Code =:= undefined ->
    false;
check(Code_hex, Code) ->
    Code_up = string:to_upper(Code),
    Md5 = mochihex:to_bin(Code_hex),
    case ets:lookup(captcha, Md5) of
        [{_, {Val, Now}}] when Val == Code_up ->
            ets:delete(captcha, Md5),
            ets:delete(captcha_time, Now),
            true;
        _ -> false
    end.

remove_expired() ->
    {Gs,_} = gs_us(),
    Dead_time =
      Gs - round(config:get(web_session_expire_timeout, 180000)/1000),
    Match = {{'$1','_'},'_'},
    Guard = [{'<','$1',{const,Dead_time}}],
    L = ets:select(captcha_time, [{Match, Guard, ['$_']}]),
    DelFun = fun({Time_Id,Captcha_id}) ->
                   ets:delete(captcha, Captcha_id),
                   ets:delete(captcha_time, Time_Id)
    end,
    lists:foreach(DelFun, L).

%%% --------------------------------------------------------------------------
%%% Local
%%% --------------------------------------------------------------------------
generate_rand({ngram, {Pass_len_1, Offset_1}, {Pass_len_2, Offset_2}}) ->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    Initial_list =

    %%% Бармаглот на английском
        "twasbriLLigandtheslithytovesdidgyreandgimbleinthewabeallmimsywerethebo"
        "rogovesandthemomerathsoutgrabebewarethejabberwockmysonthejawsthatbitet"
        "hclawsthatcatchbewarethejubjubbirdandshunthefrumiousbandersnatchhetook"
        "hisvorpalswordinhandlongtimethemanxomefoehesoughtsorestedhebythetumtum"
        "treeandstoodawhileinthoughtandasinuffishthoughthestoodthejabberwockwit"
        "heyesofflamecamewhifflingthroughthetulgeywoodandburbledasitcameonetwoo"
        "netwoandthroughandthroughthevorpalbladewentsnickersnackheleftitdeadand"
        "withitsheadhewentgalumphingbackandhastthouslainthejabberwock?cometomya"
        "rmsmybeamishboyofrabjousdaycaLLoohcallayhechortledinhisjoytwasbrilliga"
        "ndtheslithytovesdidgyreandgimbleinthewabeallmimsyweretheborogovesandth"
        "emomerathsoutgrabe",

    List_1 = optimized_strict_ngram(Initial_list, Pass_len_1 + (A3 rem Offset_1)),
    List_2 = optimized_strict_ngram(Initial_list, Pass_len_2 + (A3 rem Offset_2)),
    lists:flatten(lists:nth(random:uniform(length(List_1)),List_1)
        ++
        lists:nth(random:uniform(length(List_2)),List_2));

generate_rand({ngram, Pass_len}) ->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    Initial_list =
        "bacadafagamanapaqarasatavaza"
        "bicidifigiminipiqirisitivizi"
        "becedefegemenepeqereseteveze"
        "bycydyfygymynypyqyrysytyvyzy"
        "bocodofogomonopoqorosotovozo"
        "bucudufugumunupuqurusutuvuzu",
    List = optimized_strict_ngram(Initial_list, 2),
    lists:flatten([lists:nth(X,List)
        || X <- lists:map(fun(_)->random:uniform(length(List)) end,
            lists:seq(1, Pass_len))]);

generate_rand({simple, Pass_len}) ->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    List = "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz23456789",
    [lists:nth(X,List)
        || X <- lists:map(fun(_)->random:uniform(length(List)) end,
            lists:seq(1, Pass_len))].

% {gregorian_seconds, microseconds}
% используется как уникальный ключ в captcha_time
% элемент gregorian_seconds используется для определения устаревших captcha
gs_us() ->
    {_,_,Mi} = Now = now(),
    DT = calendar:now_to_datetime(Now),
    {calendar:datetime_to_gregorian_seconds(DT), Mi}.

%%% ==========================================================================
%%%
%%% strict_ngram([1, 2, 3, 4, 5, 6, 7], 0).
%%% [[],[],[],[],[],[],[]]
%%%
%%% strict_ngram([1, 2, 3, 4, 5, 6, 7], 1).
%%% [[1],[2],[3],[4],[5],[6],[7]]
%%%
%%% strict_ngram([1, 2, 3, 4, 5, 6, 7], 2).
%%% [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7]]
%%%
%%% strict_ngram([1, 2, 3, 4, 5, 6, 7], 3).
%%% [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7]]
%%%
%%% strict_ngram([1, 2, 3, 4, 5, 6, 7], 4).
%%% [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7]]
%%%
%%% strict_ngram([1, 2, 3, 4, 5, 6, 7], 5).
%%% [[1,2,3,4,5],[2,3,4,5,6],[3,4,5,6,7]]
%%%
%%% strict_ngram([1, 2, 3, 4, 5, 6, 7], 6).
%%% [[1,2,3,4,5,6],[2,3,4,5,6,7]]
%%%
%%% strict_ngram([1, 2, 3, 4, 5, 6, 7], 7).
%%% [[1,2,3,4,5,6,7]]
%%%
%%% strict_ngram([1, 2, 3, 4, 5, 6, 7], 8).
%%% []
%%%
%%% strict_ngram([1, 2, 3, 4, 5, 6, 7], 9).
%%% []
%%%
strict_ngram([ _ | Tail] = List, Lenth) when length(List) >=  Lenth  ->
    {Ngram, _ } = lists:split(Lenth, List),
    [Ngram | strict_ngram(Tail, Lenth)];
strict_ngram(_,_) -> [].


optimized_strict_ngram([_ | R], 0) ->
    [[] | optimized_strict_ngram(R, 0)];
optimized_strict_ngram([H1 | R], 1) ->
    [[H1]
        | optimized_strict_ngram(R, 1)];
optimized_strict_ngram([H1, H2 | R], 2) ->
    [[H1, H2]
        | optimized_strict_ngram([H2 | R], 2)];
optimized_strict_ngram([H1, H2, H3 | R], 3) ->
    [[H1, H2, H3]
        | optimized_strict_ngram([H2, H3 | R], 3)];
optimized_strict_ngram([H1, H2, H3, H4 | R], 4) ->
    [[H1, H2, H3, H4]
        | optimized_strict_ngram([H2, H3, H4 | R], 4)];
optimized_strict_ngram([H1, H2, H3, H4, H5 | R], 5) ->
    [[H1, H2, H3, H4, H5]
        | optimized_strict_ngram([H2, H3, H4, H5 | R], 5)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6 | R], 6) ->
    [[H1, H2, H3, H4, H5, H6]
        | optimized_strict_ngram([H2, H3, H4, H5, H6 | R], 6)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7 | R], 7) ->
    [[H1, H2, H3, H4, H5, H6, H7]
        | optimized_strict_ngram([H2, H3, H4, H5, H6, H7  | R], 7)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7, H8 | R], 8) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8 ]
        | optimized_strict_ngram([H2, H3, H4, H5, H6, H7, H8  | R], 8)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7, H8, H9 | R], 9) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9 ]
        | optimized_strict_ngram([ H2, H3, H4, H5, H6, H7, H8, H9  | R], 9)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10 | R], 10) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10 ]
        | optimized_strict_ngram([H2, H3, H4, H5, H6, H7, H8, H9, H10 |R], 10)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11 | R], 11) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11 ]
        | optimized_strict_ngram([H2, H3, H4, H5, H6, H7, H8, H9, H10, H11 |R], 12)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12 | R], 12) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12 ]
        | optimized_strict_ngram([H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12 | R], 12)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13 | R], 13) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13 ]
        | optimized_strict_ngram([H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13 | R], 13)];
optimized_strict_ngram(_,_) -> [].

%%% ==========================================================================
%%%
%%% tailed_ngram([1, 2, 3, 4, 5, 6, 7], 1).
%%% [[1],[2],[3],[4],[5],[6],[7]]
%%%
%%% tailed_ngram([1, 2, 3, 4, 5, 6, 7], 2).
%%% [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7],[7]]
%%%
%%% tailed_ngram([1, 2, 3, 4, 5, 6, 7], 3).
%%% [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7],[7]]
%%%
%%% tailed_ngram([1, 2, 3, 4, 5, 6, 7], 4).
%%% [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7],[5,6,7],[6,7],[7]]
%%%
%%% tailed_ngram([1, 2, 3, 4, 5, 6, 7], 5).
%%% [[1,2,3,4,5],
%%% [2,3,4,5,6],
%%% [3,4,5,6,7],
%%% [4,5,6,7],
%%% [5,6,7],
%%% [6,7],
%%% [7]]
%%%

tailed_ngram(List, Lenth) ->
    tailed_ngram(List, 1, Lenth).

tailed_ngram(List, S, Lenth) when length(List) > S ->
    [lists:sublist(List, S, Lenth) | tailed_ngram(List, S+1, Lenth) ];

tailed_ngram(List, S, Lenth) ->
    [lists:sublist(List, S, Lenth)].
