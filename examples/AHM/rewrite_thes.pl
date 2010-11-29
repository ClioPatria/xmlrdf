:- module(ahm_rewrite_thes,
	  [ rewrite/0,
	    rewrite/1,
	    rewrite/2,
	    list_rules/0
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(xmlrdf(rdf_convert_util)).
:- use_module(xmlrdf(cvt_vocabulary)).
:- use_module(xmlrdf(rdf_rewrite)).
:- use_module(util).

:- debug(rdf_rewrite).

%%	rewrite
%
%	Apply all rules on the graph =data=

rewrite :-
	rdf_rewrite(thesaurus).

%%	rewrite(+Rule)
%
%	Apply the given rule on the graph =data=

rewrite(Rule) :-
	rdf_rewrite(thesaurus, Rule).

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





record_to_concepts @@
{S, rdf:type, ahm:'Record'}
<=>
{S, rdf:type, skos:'Concept'}.




% ------------ SKOS hierarchy +related properties------
%
% Terms are unique, so we can find the correct concept through this property

narrower @@
{S, ahm:narrowerTerm, NarTerm}
<=>
rdf(NarUri, ahm:term, NarTerm),
	{S, skos:narrower, NarUri}.

% if it doesnt exist, create (or refer to) a new skos:concept
narrower @@
{S, ahm:narrowerTerm, NarTerm}
<=>
literal_to_id(['t00',NarTerm],ahm, NarUri),
	{NarUri, rdf:type, skos:'Concept'},
	{NarUri, skos:prefLabel, NarTerm},
	{S, skos:narrower, NarUri}.


% same for broader
broader @@
{S, ahm:broaderTerm, BroadTerm}
<=>
rdf(BroadUri, ahm:term, BroadTerm),
	{S, skos:broader, BroadUri}.

% if it doesnt exist, create a skos:concept
broader @@
{S, ahm:broaderTerm, BroTerm}
<=>
literal_to_id(['t00',BroTerm],ahm, BroUri),
	{BroUri, rdf:type, skos:'Concept'},
	{BroUri, skos:prefLabel, BroTerm},
	{S, skos:broader, BroUri}.


% same for related
related @@
{S, ahm:relatedTerm,RelTerm}
<=>
rdf(RelUri, ahm:term, RelTerm),
	{S, skos:related, RelUri}.

% if it doesnt exist, create a skos:concept
related @@
{S, ahm:relatedTerm,RelTerm}
<=>
literal_to_id(['t00',RelTerm],ahm, RelUri),
	{RelUri, rdf:type, skos:'Concept'},
	{RelUri, skos:prefLabel, RelTerm},
	{S, skos:related, RelUri}.



% ------------ equivalent term --------
%
% get the correct uri.
% TODO: is this skos:exactmatch or owl:sameas?

equivalent_term @@
{S, ahm:equivalentTerm, EqTerm}
<=>
rdf(EqUri, ahm:term, EqTerm),
	{S, skos:exactMatch, EqUri}.


%------------- TERMTYPES ------------

% termTypes are mapped to scopeNotes in the schema file.
% language: we have four values, [neutral, 0,1,2,3], corresponding to
% "neutral"/ EN/ NL/ DL. The first one is used as the rdf:label of the
% bnode, the other ones are values, which are subproperties of
% rdf:label

langvals @@
{S, ahm:value, Val},
	{Val, ahm:lang, literal('neutral')},
	{Val, rdf:value, RVAL}
<=>
	{S, ahm:value, RVAL}.

langvals @@
{S, ahm:value, Val},
	{Val, ahm:lang, literal(Lang)},
	{Val, rdf:value, literal(RVal)}
<=>
lang_to_langcode(Lang,LangCode),
	{S, ahm:value, literal(lang(LangCode, RVal))}.

langvals @@
{_S, ahm:value, Val}
<=>
rdf_is_bnode(Val),true.

langvals @@
{_, ahm:lang, _}
<=>
true.

% Give the Term type bnodes a uri
termtypes_to_uris @@
{_S, ahm:termType, TT},
{TT, ahm:value, VAL}\
{TT}
<=>
rdf_is_bnode(TT),
not(VAL = literal(lang(_,_))),
	literal_to_id(['tt00',VAL],ahm,TypeUri),
	{TypeUri}.

termtypes_to_uris @@
{_S, ahm:termType, literal('')}
<=>
true.
% ----------- USE/USEFOR ---------

% use becomes altlabel. Todo: clean up the rest
use_to_altlabel @@
{S, ahm:use, UseTerm}
 <=>
rdf(UseUri, ahm:term, UseTerm),
rdf(S, ahm:term, AltLab),
	{UseUri, skos:altLabel, AltLab}.

use_to_altlabel @@
{_S, ahm:use, _}
 <=>
true.


% If it's not yet added, do it now.
use_to_altlabel @@
{S, ahm:usedFor, AltLab}
 <=>
   not(rdf(S, skos:altLabel, AltLab)),
	{S, skos:altLabel, AltLab}.

use_to_altlabel @@
{_, ahm:usedFor, _}
 <=>
   true.





% ----------- LABELS ---------

% preflabel (do this last, to avoid rewrite order problems
term_to_label @@
{S, ahm:term, Term}
 <=>
{S, skos:prefLabel, Term}.


% ----------- URIs ---------

skos_uris @@
{S, rdf:type, skos:'Concept'},
{S, ahm:priref, literal(Pri)},
{S, skos:prefLabel, literal(Term)} \  {S}
<=>
literal_to_id(['t',Pri, Term], ahm, URI),
{URI}.



% ----------- CLEAN UP ---------

% Category has only one unimportant triple
remove_category @@
{_, ahm:category, _}
 <=>
true.

% all  selected values are "false", can be removed
remove_selected @@
{_, ahm:selected, _}
 <=>
true.



