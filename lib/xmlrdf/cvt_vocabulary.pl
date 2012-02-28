:- module(cvt_vocabulary,
	  [ find_in_vocabulary/4,	% +Schema, +Root, +Literal, -Resource
	    find_in_vocabulary/3	% +Schema, +Root, +Literal, -Resource
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(count)).

:- rdf_meta
	find_in_vocabulary(r, +, -),
	in_vocabulary(r, r, r, o, r).

%%	find_in_vocabulary(?Scheme, ?Root, +Literal, -Resource) is semidet.
%
%	Resource is the best mapping we can   find  for Name in the SKOS
%	hierarchy below Root.
%
%	@tbd	Disambiguation
%	@tbd	Get the context in here

find_in_vocabulary(Root, Name, Resource) :-
	find_in_vocabulary(_, Root, Name, Resource).

find_in_vocabulary(Scheme, Root, Name, Resource) :-
	to_search(Name, Literal),
	in_vocabulary(Scheme, Root, skos:prefLabel, Literal, Resource).
find_in_vocabulary(Scheme, Root, Name, Resource) :-
	to_search(Name, Literal),
	unique_solution(in_vocabulary(Scheme, Root, rdfs:label, Literal, Resource),
			Resource).

in_vocabulary(Scheme, Root, LabelPred, Literal, Resource) :-
	rdf_has(Resource, LabelPred, Literal),
	in_scheme(Resource, Scheme),
	has_root(Resource, Root).


to_search(X, X) :-
	var(X), !.
to_search(X, X) :-
	X = literal(_), !.
to_search(A, literal(A)).

in_scheme(R, Scheme) :-
	(   var(Scheme)
	->  (   rdf_has(R, skos:inScheme, Scheme)
	    ->	true
	    ;	true
	    )
	;   rdf_has(R, skos:inScheme, Scheme)
	).

has_root(R, Root) :-
	(   var(Root)
	->  find_root(R, Root)
	;   rdf_reachable(R, skos:broader, Root)
	).

find_root(R, Root) :-
	(   rdf_has(R, skos:broader, B)
	*-> find_root(B, Root)
	;   Root = R
	).
