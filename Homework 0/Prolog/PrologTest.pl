writeList([],_).
writeList([X|Xs],N) :- format('~d: ~w~n', [N,X]), writeList(Xs,N+1).

writeArgs([]) :- format('No arguments given.~n').
writeArgs(L)  :- format('You gave the arguments:~n',[]), writeList(L,0).

main :- current_prolog_flag(argv,L),
        writeArgs(L),
        halt.