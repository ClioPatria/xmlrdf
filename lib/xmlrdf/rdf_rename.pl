:- module(rdf_rename,
	  [ rdf_rename/3		% +Old, -New, ?Graph
	  ]).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta
	rdf_rename(r,r,r).

%%	rdf_rename(+OldResource, +NewResource, ?Graph) is det.
%
%	Rename  a  resource,  changing  all   references  in  all  three
%	positions of the triple. If Graph  is given, renaming is limited
%	to triples that are associated to the matching graph.

rdf_rename(Old, Old, _) :- !.
rdf_rename(Old, New, Graph) :-
	rename(Old, New, Graph).
%	rdf_transaction(rename(Old, New, Graph), rename(Old, New)).

rename(O, N, G) :-
	referencing_triples(O, G, Triples),
	length(Triples, Count),
	maplist(rename_triple(O,N), Triples, Mapped0),
	sort(Mapped0, Mapped),
	length(Mapped, NewCount),
	debug(rdf(rename), 'Renaming ~D triples into ~D', [Count, NewCount]),
	retract_old(Triples),
	assert_new(Mapped).

referencing_triples(R, G, Triples) :-
	findall(T, referencing_triple(R, G, T), Triples0),
	sort(Triples0, Triples).

referencing_triple(S, G, rdf(S,P,O,G)) :- rdf(S,P,O,G).
referencing_triple(P, G, rdf(S,P,O,G)) :- rdf(S,P,O,G).
referencing_triple(O, G, rdf(S,P,O,G)) :- rdf(S,P,O,G).

rename_triple(Old, New, rdf(S0,P0,O0,G0), rdf(S,P,O,G)) :-
	rename(S0, Old, New, S),
	rename(P0, Old, New, P),
	rename(O0, Old, New, O),
	rename(G0, Old, New, G).

rename(O, O, N, N) :- !.
rename(R, _, _, R).

retract_old([]).
retract_old([rdf(S,P,O,G)|T]) :-
	rdf_retractall(S,P,O,G),
	retract_old(T).

assert_new([]).
assert_new([rdf(S,P,O,G)|T]) :-
	rdf_assert(S,P,O,G),
	assert_new(T).

