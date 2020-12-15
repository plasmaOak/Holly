%Made by Oksana Necio


:- dynamic(backup/1).
:- dynamic(suggestion_f/1).
:- dynamic(suggestion_m/1).
:- dynamic(suggestion_n/1).

%backup is used in case Holly does not understand what the user said.
backup(['I don\'t understand.', 'Can you repeat that?', 'Um... maybe just give them money?']).

%suggestions are split into feminine, masculine, and neutral suggestions.
suggestion_f(['slipper socks', 'a cute mug','a salt lamp', 'a blanket', 'jewelry', 'a succulent', 'bath bombs', 'a charmed aroma candle']).
suggestion_m(['a watch', 'a rubiks cube', 'a fancy pen', 'socks', 'a tie', 'a sports jersey', 'weights', 'a poker set']).
suggestion_n(['some alchohol', 'a gift card', 'a board game to play', 'fancy coffee beans', 'a mug warmer', 'candy', 'a boogie board', 'a fitbit', 'a book', 'a waffle maker', 'a colouring book', 'a day planner', 'a karaoke machine', 'a mousepad', 'a puzzle', 'a neck massager', 'a chess board']).

%marking these words as female, male, or neutral, so that if the user says them, Holly knows which gifts to suggest 
female(mom).
female(mother).
female(mommy).
female(sister).
female(girlfriend).
female(gf).
female(wife).
female(girl).
female(daughter).
female(grandmother).
female(grandma).
female(aunt).

male(dad).
male(father).
male(daddy).
male(brother).
male(boyfriend).
male(bf).
male(husband).
male(boy).
male(son).
male(grandfather).
male(grandpa).
male(uncle).

neutral(parent).
neutral(sibling).
neutral(friend).
neutral(child).
neutral(adult).
neutral(coworker).
neutral(boss).
neutral(teacher).
neutral(cousin).
neutral(special).
neutral(lover).
neutral(student).
neutral(neighbor).
neutral(neighbour).

%marking these words as positive and negative so that the bot knows whehter to suggest another gift or not. 
positive([yes | _ ]).
positive([yea | _ ]).
positive([yeah | _ ]).
positive([yep | _ ]).
positive([sure | _ ]).
positive([maybe | _ ]).
positive([like | _]).
positive([ok | _ ]).
positive([alright | _]).
positive([good | _ ]).

negative([no | _ ]).
negative([nope | _ ]).
negative([nah | _ ]).
negative([never | _ ]).
negative([hates | _ ]).
negative([dislikes | _ ]).
negative([bad | _ ]).

%marking greetings and different variations of 'thank you' so that Holly can respond appropriatly
greeting([hello | _ ]).
greeting([hi | _]).
greeting([hey | _ ]).

gratitude([thanks | _ ]).
gratitude([thx | _ ]).
gratitude([thank | _ ]).

%if the user wants more info about holly before using it
info :- 
	write('Holly is a chatbot used to help people come up with gift ideas.'), nl,
	write('Please try to keep responses short, and make sure to say some varient of yes or no to the gift suggestions.'), nl,
	write('Holly does not know who your friends are, so don\'t expect a gift idea if you say \'Liam\', instead say the gift is for \'a friend\''), nl,
	write('To stop talking to Holly, type \'bye\' or some varient of it'), nl,
	write('To begin using Holly type \'holly.\'').


%first thing that Holly says when she is called
holly :-
	write('Holly > '),
	write('Hello! My name is Holly.'), nl,
	holly_loop.

%all other sections come back to holly_loop, so that holly can keep taking in input
holly_loop :-
	write('You > '),
	read_in(Input), respond(Input).

%if the user wants to leave to program
respond(Input) :-
	member(Term, Input),
	member(Term, [ quit, exit, leave, bye ]),
	write('Holly > '),
	write('Goodbye!').

%after the user says hi to holly	
respond(Input) :-
	greeting(Input),
	write('Holly > '),
	write('Pleased to meet you.'), nl,
	write('Are you looking for a gift idea?'), nl,
	holly_loop.
	
%if the user says yes to wanting a gift idea	
respond(Input) :-
	positive(Input),
	write('Holly > '),
	write('Who is the gift for?'), nl,
	holly_loop.
	
%if the user does not want a gift idea
respond(Input) :-
	negative(Input),
	write('Holly > '),
	write('Then why are you here?'), nl,
	holly_loop.

%finding out who the gift is for
respond(Input) :-
	member(Who, Input),
	member(Who, [mom, mother, mommy, dad, father, daddy, brother, sister, friend, girlfriend, gf, wife, boyfriend, bf, husband, child, girl, neighbour, neighbor, boy, adult, son, daughter, grandfather, grandmother, parent, aunt, uncle, cousin, coworker, boss, teacher, student, special, lover]),
	write('Holly > '),
	write('How about '), suggest(Who), nl,
	holly_loop.

%if the user wants to get a gift for Holly
respond([you | _]) :-
	write('Holly > '),
	write('I don\'t need a gift.'), nl,
	holly_loop.

%if the user says thank you	
respond(Input) :-
	gratitude(Input),
	write('Holly > '),
	write('You\'re Welcome :)'), nl,
	write('Is there any one else you want to give a gift to?'), nl,
	holly_loop.
	
%if holly doesnt understand the input, she will go through the backup list 
respond([ _ ]) :- 
	retract(backup([ Next | Rest ])),
	append(Rest, [Next], NewBackupList),
	asserta(backup(NewBackupList)),
	write(Next), nl,
	holly_loop.
	
%after finding out who the gift is for, suggest suggests gifts from the lists above

%suggestions from the feminine suggestion list
suggest(Who) :- 
	female(Who),
	retract(suggestion_f([ Next | Rest ])),
	append(Rest, [Next], NewSuggestionList),
	asserta(suggestion_f(NewSuggestionList)),
	write(Next), nl,
	suggest_loop(Who).

%suggestions from the masculine suggestion list
suggest(Who) :- 
	male(Who),
	retract(suggestion_m([ Next | Rest ])),
	append(Rest, [Next], NewSuggestionList),
	asserta(suggestion_m(NewSuggestionList)),
	write(Next), nl,
	suggest_loop(Who).
	
%suggestions from the neutral suggestion list
suggest(Who) :- 
	neutral(Who),
	retract(suggestion_n([ Next | Rest ])),
	append(Rest, [Next], NewSuggestionList),
	asserta(suggestion_n(NewSuggestionList)),
	write(Next), nl,
	suggest_loop(Who).

%this loop is used so that Holly can keep listing suggestions after getting a response
suggest_loop(Who) :-
	write('You > '),
	read_in(Input), suggest_respond(Who,Input).
	
%Holly will suggest another gift when the user says no
suggest_respond(Who, Input) :-
	negative(Input),
	write('Holly > '),
	write('How about '), suggest(Who), nl,
	suggest_loop.

%if the user asks what an item is
suggest_respond(Who, [what,is | _ ]) :-
	write('Holly > '),
	write('I\'m not google, look it up.'), nl,
	suggest_loop(Who).
	
%if the user likes the gift suggestion
suggest_respond(Who, Input) :-
	positive(Input),
	write('Holly > '),
	write('Awesome'), nl,
	holly_loop.
	
	