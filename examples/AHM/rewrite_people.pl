:- module(ahm_rewrite_people,
	  [ rewrite/0,
	    rewrite/1,
	    rewrite/2,
	    list_rules/0
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xmlrdf/rdf_convert_util)).
:- use_module(library(xmlrdf/cvt_vocabulary)).
:- use_module(library(xmlrdf/rdf_rewrite)).

:- debug(rdf_rewrite).

%%	rewrite
%
%	Apply all rules on the graph =people=

rewrite :-
	rdf_rewrite(people).

%%	rewrite(+Rule)
%
%	Apply the given rule on the graph =people=

rewrite(Rule) :-
	rdf_rewrite(people, Rule).

%%	rewrite(+Graph, +Rule)
%
%	Apply the given rule on the given graph.

rewrite(Graph, Rule) :-
	rdf_rewrite(Graph, Rule).

%%	list_rules
%
%	List the available rules to the console.

list_rules :-
	rdf_rewrite_rules.

:- discontiguous
	rdf_mapping_rule/5.


people_type @@
{ A, rdf:type, ahm:'Record' }
	<=>
	{ A, rdf:type, ahm:'Person' }.

%VIC: removed this as it forces skos, for now we go with rda
% {A,skos:inScheme,http://purl.org/collections/nl/am/AM_PeopleScheme'}.
							%

people_uris @@
{ A, ahm:name, _Name } \ {A} <=>
	rdf_is_bnode(A),
	rdf(A, ahm:priref, literal(Priref)),
	rdf_current_ns(ahm, S1),
	concat_atom([S1, 'p-', Priref], S),
	{S}.

