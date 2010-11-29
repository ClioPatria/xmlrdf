:- module(rdf_rename,
	  [ rdf_rename/3		% +Old, -New, ?Graph
	  ]).
:- use_module(library(semweb/rdf_db)).

%%	rdf_rename(+OldResource, +NewResource, ?Graph) is det.
%
%	Rename  a  resource,  changing  all   references  in  all  three
%	positions of the triple. If Graph  is given, renaming is limited
%	to triples that are associated to the matching graph.

rdf_rename(Old, Old, _) :- !.
rdf_rename(Old, New, Graph) :-
	rdf_transaction(rename(Old, New, Graph), rename(Old, New)).

rename(Old, New, G) :-
	forall(rdf(Old, P, O, G),
	       rdf_update(Old, P, O, G, subject(New))),
	forall(rdf(S, Old, O, G),
	       rdf_update(S, Old, O, G, predicate(New))),
	forall(rdf(S, P, Old, G),
	       rdf_update(S, P, Old, G, object(New))).
