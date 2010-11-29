/*  File:    rdf_name_bnodes.pl
    Author:  Jan Wielemaker
    Created: Jan 14 2010
    Purpose: Create URIs to blank nodes
*/

:- module(rdf_name_bnodes,
	  [ name_bnodes/3,		% +Set, -Names, +Graph
	    name_instances/4		% +Class, -P, -Pairs, +Graph
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(debug)).

/** <module> Establish a URI for a set of RDF blank-nodes

This library can propose and implement a naming schema for a set of RDF
blank-nodes.

Naming schemas:

    * Create from a unique property
    In this case, give preference to word-like properties over
    database keys.  Make the property-literal turtle friendly.
    We call this key <ID>.  Use <base><ID> as identifier.

    * Create from a semi-unique property + property of parent.
    If there is a property that is nearly unique and the nodes
    are organised in a hierarchy, use the label of the parent
    (recursively) to make the names unique.  Use
    <base><parent-ID>-<ID>

    * If the set can be split into multiple classes, each of
    which can have unique names attached based on one of the
    other schemas, use <base><class-ID>-<ID>

    * If the blank-nodes are used exactly once as a property
    of another resource, use <other>-<property-ID>.  If the property
    appears multiple times, try <other>-<property-ID>-<ID> or,
    if all fails, <other>-<property-ID>-<N>

Steps:

    * Find shared (literal) properties
    * Split-by-class

@see literal_to_id/3 for generating Turtle-friendly idenfiers.
*/

:- rdf_meta
	name_instances(r, r, -, r).

%%	name_instances(+Class, ?P, -Pairs, +Graph)

name_instances(Class, P, Pairs, Graph) :-
	findall(R, rdf(R, rdf:type, Class), Rs),
	sort(Rs, Set),
	name_bnodes(Set, P, Names, Graph),
	pairs_keys_values(Pairs, Set, Names).


%%	name_resources(+Resources, -Names, ?Graph)

name_bnodes(Set, Names, Graph) :-
	name_bnodes(Set, _, Names, Graph).

name_bnodes(Set, P, Names, Graph) :-
	length(Set, RCount),
	shared_property(Set, P, Graph),
	debug(name_bnodes, 'Trying property ~q', [P]),
	maplist(local_name(Graph, P), Set, Names),
	sort(Names, Sorted),
	length(Sorted, NCount),
	(   RCount == NCount
	->  true
	;   NU is RCount - NCount,
	    debug(name_bnodes, '~D of ~D non-unique', [NU, RCount]),
	    fail
	).

%%	local_name(+Graph, +P, +R, -Name) is nondet.
%
%	Propose a local name for R based on P.
%
%	@tbd	Add 'n' if the results starts with a digit

local_name(Graph, P, R, Name) :-
	findall(T, property_text(R, P, T, Graph), Ts),
	Ts \== [], !,
	maplist(text_to_id, Ts, IDL),
	sort(IDL, SIDL),
	atomic_list_concat(SIDL, -, Name).


%%	property_text(+R, +P, -Text, ?Graph) is nondet.
%
%	Fetch a textual value for the property P.

property_text(R, P, Text, Graph) :-
	rdf(R, P, Value, Graph),
	text_of(Value, Text).

text_of(literal(X), Text) :- !,
	text_of_literal(X, Text).
text_of(R, Text) :-
	rdf_is_bnode(R),
	rdf_has(R, rdf:value, V),
	text_of(V, Text).

text_of_literal(Text, Text) :-
	atom(Text), !.
text_of_literal(lang(_, Text), Text).
text_of_literal(type(_, Text), Text).

%%	shared_property(+Set, -P, +Graph) is nondet.
%
%	True if P is a property that appears on all instances of Set.
%
%	@tbd	Should we also allow for super-properties?

shared_property(Set, P, Graph) :-
	map_list_to_pairs(property_count(Graph), Set, Keyed),
	keysort(Keyed, KeySorted),
	pairs_values(KeySorted, [H|T]),
	property_of(P, Graph, H),
	(   maplist(property_of(P, Graph), T)
	->  true
	).

%%	property_count(+Graph, +R, -Count) is det.
%
%	Count is the number of distinct properties on the resource R.

property_count(Graph, R, Count) :-
	findall(P, rdf(R, P, _, Graph), Ps),
	sort(Ps, Set),
	length(Set, Count).

%%	property_of(?P, +Graph, +Resource) is nondet.
%
%	True if P is a property on Resource in Graph.

property_of(P, Graph, R) :-
	atom(P), !,
	(   rdf(R, P, _, Graph)
	->  true
	).
property_of(P, Graph, R) :-
	findall(P, rdf(R, P, _, Graph), Ps),
	sort(Ps, Set),
	member(P, Set).


		 /*******************************
		 *		UTIL		*
		 *******************************/

:- dynamic
	text_id_cache/2.

text_to_id(Text, Id) :-
	(   text_id_cache(Text, Id0)
	->  Id = Id0
	;   text_to_id_raw(Text, Id0)
	->  assertz(text_id_cache(Text, Id0)),
	    Id = Id0
	;   debug(name_bnodes, 'No id from ~q', [Text]),
	    fail
	).

text_to_id_raw(Text, Id) :-
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
