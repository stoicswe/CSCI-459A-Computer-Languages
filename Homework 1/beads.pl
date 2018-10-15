%fac(0,1).
%fac(N,X) :- N > 0, M is N - 1, fac(M,Y), X is Y * N.

fact1(0,Result) :- Result is 1.
fact1(N,Result) :- N > 0, N1 is N-1, fact1(N1,Result1), Result is Result1*N.

main :- current_prolog_flag(argv,[BB_S,CC_S]),
        atom_number(BB_S,BB),
        atom_number(CC_S,CC),
        %read_input,
        fact1(BB,FB),
        ABS is abs(BB - CC),
        fact1(ABS,FE),
        fact1(CC,FC),
        TI is FE * FC,
        BF is FB / TI,
        %BF is fac(BB) / (fac(abs(BB-CC)) * fac(CC))),
        print('Number of bracelets: '),
        write(BF),
        halt.