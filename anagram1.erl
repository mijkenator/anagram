#!/usr/bin/env escript


main([File, LoadAttempts, FindAttempts]) -> main_func(File, list_to_integer(LoadAttempts), list_to_integer(FindAttempts));
main(_)      -> main_func("words", 15, 100).

main_func(File, LoadAttempts, FindAttempts) ->
    ets:new(words_ets, [set, named_table, public]),
    ets:new(word_prefix_ets, [set, named_table, public]),
    LoadAverage = measure_average(fun()-> load_file(File) end, LoadAttempts),
    io:format("~naverage load time: ~p (mks), ~p load attempts ~n", [LoadAverage, LoadAttempts]),
    finder(FindAttempts).

% all letters combinations
c(L) when length(L) > 5 -> c2(L, []);
c(L) -> c1(L).

c1([])    -> [[]];
c1(L)     -> [[H|T] || H <- L, T <- c1(L--[H])].
c2([], _) -> [[]];
c2(L, A)  -> [[H|T] || H <- L, T <- c2(L--[H], A++[H]), mcheck(A, H)].

mcheck([], H) -> memory_search([H]);
mcheck(L,  _) when length(L) > 3 -> true;
mcheck(L,  _) -> memory_search(L).
%mcheck(A, H) -> io:format("~p : ~p ~n", [A, list_to_binary([H])]), true.

memory_search(S) -> 
    case ets:lookup(word_prefix_ets, S) of
        [] -> false 
        ;_ -> true
    end.

load_file(FileName) ->
    case file:open(FileName, [read]) of
        {ok, File} -> read_lines(File)
        ;_         -> io:format("cannot open file ~p ~n", [FileName]), exit(cannot_open_file)
    end.

read_lines(File) ->
    case io:get_line(File, "") of
        eof  -> io:format(".", []);
        Line -> save_word(trim(Line)), read_lines(File)
    end.

save_word(Word) ->
    LWord = string:to_lower(Word),
    case length(LWord) of
        1 -> ets:insert(word_prefix_ets, {LWord, 1});
        2 -> 
            [F,S] = LWord,
            ets:insert(word_prefix_ets, {[F], 1}),
            ets:insert(word_prefix_ets, {[F,S], 1});
        L when L>=3 ->
            [F,S,T|_] = LWord,
            ets:insert(word_prefix_ets, {[F], 1}),
            ets:insert(word_prefix_ets, {[F,S], 1}),
            ets:insert(word_prefix_ets, {[F,S,T], 1});
        _ -> ok
    end,
    ets:insert(words_ets, {LWord, Word}).

finder(FindAttempts) ->
    case io:get_line("Enter word:") of
        "quit\n" -> io:format("Bye ~n", []);
        Word     ->
            W = string:to_lower(trim(Word)),
            %io:format("Candidates: ~p~n", [lists:usort(c(W))]),
            FindAverage = measure_average(fun() -> find_anagram(W) end, FindAttempts),
            case find_anagram(W) of
                [] -> io:format("not found~n", []);
                L  -> io:format("anagrams found: ~p ~n", [string:join(L, ", ")])
            end,
            io:format("find average: ~p (mks), ~p find attempts ~n", [FindAverage, FindAttempts]),
            finder(FindAttempts)
    end.

find_anagram(W) -> [Anagram || {_,Anagram} <- lists:flatten([ets:lookup(words_ets, Wc) || Wc <- lists:usort(c(W)), W=/=Wc])].

trim(String) -> re:replace(String, "\\s+", "", [global, {return, list}]).

measure_average(Fun, Times) -> measure_average(Fun, Times, undefined).
measure_average(_, 0, A) -> A;
measure_average(Fun, Times, Average) ->
    T1 = now(), Fun(), T2 = now(),
    Ret = timer:now_diff(T2, T1),
    Average1 = if is_integer(Average) -> (Average + Ret) div 2; true -> Ret end,
    measure_average(Fun, Times - 1, Average1).
    

