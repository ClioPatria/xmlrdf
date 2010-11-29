:- module(rdf_convert_util,
	  [ rdf_literal/1,		% @Term
	    literal_to_id/3,		% +Literal, +NameSpace, -Id
	    name_to_id/3,		% +Literal, +NameSpace, -Id
	    edm_identifier/4		% +URI, +Orig, -New, NewURI
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(apply)).


%%	rdf_literal(Term) is semidet.
%
%	True if Term is an RDF literal

rdf_literal(Term) :-
	compound(Term),
	Term = literal(_).

%%	name_to_id(+Literal, +NS, -ID)
%
%	Similar to literal_to_id/3, but  intended   to  deal with person
%	names.
%
%	@tbd	Now simply the same as literal_to_id/3

name_to_id(Literal, NS, ID) :-
	literal_to_id(Literal, NS, ID).

%%	literal_to_id(+LiteralOrList, +NS, -ID) is det.
%
%	Generate an identifier from a literal  by mapping all characters
%	that  are  not  allowed  in   a    (Turtle)   identifier  to  _.
%	LiteralOrList can be a list. In this  case we generate an id for
%	each element in LiteralOrList and append  these. A typical usage
%	scenario is to add a type:
%
%	    ==
%	    literal_to_id(['book-', Literal], NS, ID)
%	    ==
%
%	Another is to add the label of the parent:
%
%	    ==
%	    literal_to_id([ParentLit, '-', Literal], NS, ID)
%	    ==
%
%	@tbd	Verify that the generated URI is unique!
%	@tbd	Remove diacritics for non-iso-latin-1 text

literal_to_id(Literals, NS, URI) :-
	is_list(Literals), !,
	maplist(literal_to_id, Literals, IDs),
	atomic_list_concat(IDs, ID),
	rdf_current_ns(NS, Prefix),
	atom_concat(Prefix, ID, URI).
literal_to_id(Literal, NS, URI) :-
	literal_to_id(Literal, ID),
	rdf_current_ns(NS, Prefix),
	atom_concat(Prefix, ID, URI).

literal_to_id(Literal, ID) :-
	text_of_literal(Literal, Text),
	text_to_id(Text, ID).

text_of_literal(Var, _) :-
	var(Var), !,
	instantiation_error(Var).
text_of_literal(literal(Lit), Text) :- !,
	text_of_literal(Lit, Text).
text_of_literal(type(_, Text), Text).
text_of_literal(lang(_, Text), Text).
text_of_literal(Text, Text) :-
	atomic(Text).

text_to_id(Text, Id) :-
	unaccent_atom(Text, T1),
	atom_codes(T1, Codes),
	maplist(map_non_id_char, Codes, Codes1),
	normalize_underscores(Codes1, Codes2),
	atom_codes(Id, Codes2).

map_non_id_char(0'_, 0'_) :- !.
map_non_id_char(0'-, 0'-) :- !.
map_non_id_char(C, C) :-
	code_type(C, csym), !.
map_non_id_char(_, 0'_).

normalize_underscores([0'_|T0], T) :- !,
	normalize_underscores(T0, T).
normalize_underscores([], [0'_]) :- !.
normalize_underscores(In, Out) :-
	normalize_underscores_2(In, Out).

normalize_underscores_2([], []).
normalize_underscores_2([0'_|T0], Can) :- !,
	normalize_underscores(T0, T),
	(   T == [0'_]
	->  Can = []
	;   Can = [0'_|T]
	).
normalize_underscores_2([H|T0], [H|T]) :-
	normalize_underscores_2(T0, T).


%%	edm_identifier(URI, +Orig, +New, -NewURI)
%
%	Translate betweem the various EDM identifiers.  E.g.:
%
%	==
%		edm_identifier(Proxy, proxy, aggregate, Aggregate)
%	==
%
%	@error	domain_error(edm_uri, URI) if the URI doesn't contain
%		=|/<orig>-|= or contains it multiple times.

edm_identifier(URI, Orig, New, NewURI) :-
	subst_pattern(Orig, OP),
	(   sub_atom(URI, B, _, A, OP),
	    sub_atom(URI, _, A, 0, End),
	    sub_atom(URI, 0, B, _, Start),
	    \+ sub_atom(End, _, _, _, OP),
	    \+ sub_atom(Start, _, _, _, OP)
	->  subst_pattern(New, NP),
	    atomic_list_concat([Start, NP, End], NewURI)
	;   domain_error(edm_uri, URI)
	).

subst_pattern(Text, Pattern) :-
	atomic_list_concat([/, Text, -], Pattern).

