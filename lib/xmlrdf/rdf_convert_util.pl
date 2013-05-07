:- module(rdf_convert_util,
	  [ rdf_literal/1,		% @Term
	    type_time_literal/2,	% +Literal, -TypedLiteral
	    literal_to_id/3,		% +Literal, +NameSpace, -Id
	    literal_to_id/4,		% +Literal, +NameSpace, -Id, +Options
	    name_to_id/3,		% +Literal, +NameSpace, -Id
	    edm_identifier/4		% +URI, +Orig, -New, NewURI
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(apply)).


:- if(\+current_predicate(rdf_literal/1)).
%%	rdf_literal(Term) is semidet.
%
%	True if Term is an RDF literal.
%
%	@tbd	Should be merged into library(semweb/rdf_db).

rdf_literal(Term) :-
	compound(Term),
	Term = literal(_).
:- endif.

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
%       Options may include:
%
%       * accents(normalize/disable), defaults to normalize
%       * underscores(normalize/disable), defaults to normalize
%
%	@tbd	Verify that the generated URI is unique!
%	@tbd	Remove diacritics for non-iso-latin-1 text

literal_to_id(Literals, NS, URI, Options):-
	is_list(Literals), !,
	maplist(literal_to_id_nn(Options), Literals, IDs),
	atomic_list_concat(IDs, ID),
	rdf_current_ns(NS, Prefix),
	atom_concat(Prefix, ID, URI).
literal_to_id(Literal, NS, URI, Options) :-
	literal_to_id_nn(Options, Literal, ID),
	rdf_current_ns(NS, Prefix),
	atom_concat(Prefix, ID, URI).
literal_to_id(Literals, NS, URI) :-
	literal_to_id(Literals, NS, URI, []).

literal_to_id_nn(Options, Literal, ID) :-
	text_of_literal(Literal, Text),
	text_to_id(Text, ID, Options).

text_of_literal(Var, _) :-
	var(Var), !,
	instantiation_error(Var).
text_of_literal(literal(Lit), Text) :- !,
	text_of_literal(Lit, Text).
text_of_literal(type(_, Text), Text).
text_of_literal(lang(_, Text), Text).
text_of_literal(Text, Text) :-
	atomic(Text).

text_to_id(Text, Id, Options) :-
	(   option(accents(normalize), Options, normalize)
	->  unaccent(Text, T1)
	;   T1 = Text
	),
	atom_codes(T1, Codes),
	maplist(map_non_id_char, Codes, Codes1),
	(   option(underscores(normalize), Options, normalize)
	->  normalize_underscores(Codes1, Codes2)
	;   Codes2 = Codes1
	),
	atom_codes(Id, Codes2).

:- if(exists_source(library(unicode))).
unaccent(Raw, Clean) :-
	unicode_map(Raw, Clean, [decompose,stripmark]).
:- else.
unaccent(Raw, Clean) :-
	unaccent_atom(Raw, Clean).
:- endif.


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


%%	type_time_literal(+Literal, -TypedLiteral) is det.
%
%	True when TypedLiteral is a typed version of Literal.

type_time_literal(literal(Text), literal(type(Type, Text))) :-
	atom(Text),
	atom_codes(Text, Codes),
	(   phrase(gYear, Codes)
	->  rdf_equal(Type, xsd:gYear)
	;   phrase(date, Codes)
	->  rdf_equal(Type, xsd:date)
	;   phrase(dateTime, Codes)
	->  rdf_equal(Type, xsd:dateTime)
	), !.
type_time_literal(Literal, Literal).

gYear    --> digit, digit, digit, digit.
date     --> gYear, "-", month, "-", day.
time     --> hour, ":", min, ( ":", sec ; [] ).
dateTime --> date, "T", time.

month --> digit(C1), digit(C2), { number_codes(M, [C1,C2]), between(1,12,M) }.
day   --> digit(C1), digit(C2), { number_codes(D, [C1,C2]), between(1,31,D) }.
hour  --> digit(C1), digit(C2), { number_codes(H, [C1,C2]), between(0,23,H) }.
min   --> digit(C1), digit(C2), { number_codes(M, [C1,C2]), between(0,59,M) }.
sec   --> digit(C1), digit(C2), { number_codes(S, [C1,C2]), between(0,59,S) }.

digit    --> digit(_).
digit(C) --> [C], { between(0'0, 0'9, C) }.


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

