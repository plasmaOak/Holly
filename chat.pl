%Oksana Necio

%This was left out of the main Holly.pl to make it look cleaner
%the regular read() function only reads one word at a time
%so this is used to read entire sentences

read_in(P):-initread(L),words(P,L, []).

words([V|U]) --> word(V),!,blanks,words(U).
words([]) --> [].

word(U1) --> [K],{lc(K,K1)},!,alphanums(U2),{name(U1,[K1|U2])}.
word(nb(N)) --> [K],{digit(K)},!,digits(U),{name(N,[K|U])}.
word(V) --> [K],{name(V,[K])}.

alphanums([K1|U]) --> [K],{alphanum(K,K1)},!,alphanums(U).
alphanums([]) --> [].

alphanum(95,95) :- !.
alphanum(K,K1):-lc(K,K1).
alphanum(K,K):-digit(K).

digits([K|U]) --> [K],{digit(K)},!,digits(U).
digits([]) --> [].

blanks--> [K],{K=<32},!,blanks.
blanks --> [].

digit(K):-K>47,K<58.

lc(K,K1):-K>64,K<91,!,K1 is K+32. 
lc(K,K):-K>96,K<123.

initread([K1,K2|U]):-get_code(K1),get_code(K2), readrest(K2,U).

readrest(63,[]):-!.
readrest(33,[]):-!.
readrest(10,[]):-!.
readrest(K,[K1|U]):-K=<32,!,get_code(K1),readrest(K1,U).
readrest(_K1,[K2|U]):-get_code(K2),readrest(K2,U).