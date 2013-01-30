#!/usr/bin/env escript


main([File, LoadAttempts, FindAttempts]) -> main_func(File, list_to_integer(LoadAttempts), list_to_integer(FindAttempts));
main(_)      -> main_func("words", 15, 100).

main_func(File, LoadAttempts, FindAttempts) ->
    ets:new(words_ets, [set, named_table, public]),
    LoadAverage = measure_average(fun()-> load_file(File) end, LoadAttempts),
    %io:format("-> ~p ~n", [ets:tab2list(words_ets)]),
    io:format("~naverage load time: ~p (mks), ~p load attempts ~n", [LoadAverage, LoadAttempts]),
    finder(FindAttempts).

load_file(FileName) ->
    ets:delete_all_objects(words_ets),
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
    LWord = lists:sort(string:to_lower(Word)),
    case ets:lookup(words_ets, LWord) of
        [{LWord, List}] -> ets:insert(words_ets, {LWord, [Word]++List})
        ;_              -> ets:insert(words_ets, {LWord, [Word]})
    end.

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

find_anagram(W) ->
    SW = lists:sort(W),
    case ets:lookup(words_ets, SW) of
        [{_, List}] -> [X || X<- List, string:to_lower(X) =/= W]
        ;_          -> []
    end.

trim(String) -> re:replace(String, "\\s+", "", [global, {return, list}]).

measure_average(Fun, Times) -> measure_average(Fun, Times, undefined).
measure_average(_, 0, A) -> A;
measure_average(Fun, Times, Average) ->
    T1 = now(), Fun(), T2 = now(),
    Ret = timer:now_diff(T2, T1),
    Average1 = if is_integer(Average) -> (Average + Ret) div 2; true -> Ret end,
    measure_average(Fun, Times - 1, Average1).
    

