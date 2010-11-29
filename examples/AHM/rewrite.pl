:- module(ahm_rewrite,
	  [ rewrite/0,
	    rewrite/1,
	    rewrite/2,
	    list_rules/0
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xmlrdf/rdf_convert_util)).
:- use_module(library(xmlrdf/cvt_vocabulary)).
:- use_module(library(xmlrdf/rdf_rewrite)).
:- use_module(util).

:- debug(rdf_rewrite).

%%	rewrite
%
%	Apply all rules on the graph =data=

rewrite :-
	rdf_rewrite(data).

%%	rewrite(+Rule)
%
%	Apply the given rule on the graph =data=

rewrite(Rule) :-
	rdf_rewrite(data, Rule).

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

% URI's are based on ahm:objectnumber as they seem unique \
%(could also have chosen ahm:priref). Here we only make the
% proxy, the rest of the EDM triangle is made later

assign_uris @@
{ S, ahm:objectNumber, ObjectNumber } \
{ S } <=>
	literal_to_id(['proxy-', ObjectNumber], ahm, URI),
	{ URI }.


% Title is always in Dutch, for a few objects, a translation into
% English is available, which maps to @en
title_nl @@
{ S, ahm:title, TitleNL }
<=>
{ S, ahm:title, TitleNL@nl}.

title_en @@
{ S, ahm:titleTranslation, TitleEN }
<=>
{ S, ahm:title, TitleEN@en}.


%------------ CREATOR RULES ------------

% For makers, we use the person thesaurus. TODO: make the object point
% to the person URI if it is in the thesaurus, or to a BNode object if
% it is not (or add it to the thesaurus).
%
% Problematic are two Maker properties that are context dependent, the
% role and the qualifier. there are instances where both are present.
% The solution we use is for the roles to use subproperties and for the
% qualifiers to keep the bnode, with rdf:value=thesaurus uri. Another
% possible solution is to make a subproperty for each qualifier/role
% combi.


% Find uri's for creators. This version checks agains existence of a uri
% in the people thesaurus (currently, this looks in all loaded rdf, no
% graph is given. But we do check for the rdf type). Make sure that this
% is up to date with the people conversion: the literal to id scheme
% should be the same. We assume here that the data instances correspond
% exactly to the thesaurus concepts.
% NOTE: we here use the bnode-reification trick, with rdf:value because
% we have a uri

% CreatorRoles. Here we used to do the same as in bibliopolis: we make
% different subproperties for different roles. If there is no specified
% role (""), we keep ahm:maker. This uses a util predicate. NOTE:
% CHANGED THIS, no longer uses sub-property-role
%
/* OLD RULES
creator_hackrule @@ % This rule to not mess up the ahm:maker
{S, ahm:maker, M}
<=>
{S, ahm:makerorig, M}.

creator_to_uris @@
{ S, ahm:makerorig, B},
{ B, ahm:creator, Name},
{ B, ahm:creatorRole, literal(Role)}

<=>
  rdf_is_bnode(B),
  role_to_property(Role, Property),
     {S, Property, B},
     rdf(PersonURI, ahm:name, Name),
     rdf(PersonURI, rdf:type, ahm:'Person'),
     {B, rdf:value, PersonURI}.
*/

creator_to_uris @@
{ _, ahm:maker, B}\
{ B, ahm:creator, Name}
<=>
  rdf_is_bnode(B),
     rdf(PersonURI, ahm:name, Name),
     rdf(PersonURI, rdf:type, ahm:'Person'),
     {B, rdf:value, PersonURI}.

creator_clean @@
{M, ahm:creatorDateOfBirth, _}
<=>
rdf_is_bnode(M),
true.

creator_clean @@
{M, ahm:creatorDateOfDeath, _}
<=>
rdf_is_bnode(M),
true.

creator_clean @@
{M, ahm:creatorDateOfDeath, _}
<=>
rdf_is_bnode(M),
true.



% Same for associated persons
association_person @@
{S, ahm:associationPerson, AP}
<=>
rdf(PersonURI, ahm:name, AP),
rdf(PersonURI, rdf:type, ahm:'Person'),
{S, ahm:associationPerson, PersonURI}.

% Same for associated persons
content_person @@
{S, ahm:contentPersonName, CP}
<=>
rdf(PersonURI, ahm:name, CP),
rdf(PersonURI, rdf:type, ahm:'Person'),
{S, ahm:contentPersonName, PersonURI}.

% Same for documentationAuthors
documentation_author @@
{S, ahm:documentationAuthor, DA}
<=>
rdf(PersonURI, ahm:name, DA),
rdf(PersonURI, rdf:type, ahm:'Person'),
{S, ahm:documentationAuthor, PersonURI}.

% Same for reproductionCreators
reproduction_creator @@
{S, ahm:reproductionCreator, RC}
<=>
rdf(PersonURI, ahm:name, RC),
rdf(PersonURI, rdf:type, ahm:'Person'),
{S, ahm:reproductionCreator, PersonURI}.

% Same for exhibitionOrganiser
exhibition_organiser @@
{S, ahm:exhibitionOrganiser, RC}
<=>
rdf(PersonURI, ahm:name, RC),
rdf(PersonURI, rdf:type, ahm:'Person'),
{S, ahm:exhibitionOrganiser, PersonURI}.



% For dimensions, we concatenate all available information into a single
% literal value, which becomes the rdf:label(!) of the bnode. The
% individual properties are retained.

dimensions @@
{ _S, ahm:dimension, B},
{ B, ahm:dimensionValue, literal(Val)},
{ B, ahm:dimensionUnit, literal(Unit)}?,
{ B, ahm:dimensionType, literal(Type)}?,
{ B, ahm:dimensionPrecision, literal(Prec)}?,
{ B, ahm:dimensionPart, literal(Part)}?,
{ B, ahm:dimensionNotes, literal(Notes)}?

==>
rdf_is_bnode(B),
        concat_maybe([Type,Val,Unit,Prec,Part,Notes], ConcatVal),
	{B, rdfs:label, literal(ConcatVal)}.


% For documentations, we do the same thing: gather all literals,
% concatenate the bound variables and use that as rdfs:label of the
% bnode.

documentation @@
{ _S, ahm:documentation, B},
{ B, ahm:documentationAuthor, literal(Author)}?,
{ B, ahm:documentationPageReference, literal(PageRef)}?,
{ B, ahm:documentationShelfMark, literal(ShelfMark)}?,
{ B, ahm:documentationSortYear, literal(SortYear)}?,
{ B, ahm:documentationTitle, literal(Title)}?,
{ B, ahm:documentationTitleArticle, literal(TitleArticle)}?,
{ B, ahm:documentationLRef, literal(LRef)}?
==>
rdf_is_bnode(B),
        concat_maybe([Author, TitleArticle, Title, PageRef, SortYear, ShelfMark, LRef], ConcatVal),
	{B, rdfs:label, literal(ConcatVal)}.


% For Exhibitions, a better way would be to model them as events using
% the EDM classes and properties available and the info should be stored
% in a (separate) thesaurus. For now, we use bnodes and 'treat them as
% resources', rdfs:label is a maybeconcat of the available info. Also,
% ahm:exhibition appears twice, once with domain=resource and once with
% domain=Exhibition. We rename the latter.

exhibitions @@
{S, ahm:exhibition, B},
{ B, ahm:exhibitionCatalogueNumber, literal(CatNum)}?,
{ B, ahm:exhibitionCode, literal(Code)}?,
{ B, ahm:exhibitionDateEnd , literal(DateEnd)}?,
{ B, ahm:exhibitionDateStart, literal(DateStart)}?,
{ B, ahm:exhibitionLref, literal(Lref)}?,
{ B, ahm:exhibitionObjectLocation  , literal(ObjLoc)}?,
{ B, ahm:exhibitionOrganiser , literal(Organiser)}?,
{ B, ahm:exhibitionVenue  , literal(Venue)}?,
{S, ahm:exhibition, B}	\ %cant combine ? and \, so this is a hack

{ B, ahm:exhibition, literal(ExName)}
<=>
rdf_is_bnode(B),
        concat_maybe([ExName, Venue, DateStart, DateEnd, Organiser, Code, Lref, CatNum, ObjLoc], ConcatVal),
	{B, ahm:exhibitionTitle, literal(ExName)},
	{B, rdfs:label, literal(ConcatVal)}.


% For Locations, we concatenate a term using all descriptions. Not sure
% if these hould actually to be thesaurus concepts

exhibitions @@
{_S, ahm:locat, B},
{B, ahm:currentLocation, literal(Loc)}?,
{B, ahm:currentLocationDateEnd, literal(LocDE)}?,
{B, ahm:currentLocationDateStart, literal(LocDS)}?,
{B, ahm:currentLocationFitness, literal(LocFit)}?,
{B, ahm:currentLocationLref, literal(LocLref)}?,
{B, ahm:currentLocationNotes, literal(LocNotes)}?,
{B, ahm:currentLocationType, literal(LocType)}?
==>
rdf_is_bnode(B),
        concat_maybe([Loc, LocDS, LocDE, LocFit, LocLref, LocNotes, LocType], ConcatVal),
	{B, rdfs:label, literal(ConcatVal)}.



% Related Object Reference, convert from literal to object (proxy)
% uri Note that in the original data there is no XML grouping of
% related_object.references and related_object.notes, so we do not
% reconstruct that.

related_object_reference @@
{S, ahm:relatedObjectReference, literal(ObjRef)}
<=>
literal_to_id(['proxy-', ObjRef],ahm, ObjURI),
	{S,ahm:relatedObjectReference, ObjURI}.

% can be removed if already the title of the thing itself
related_object_title @@
{S, ahm:relatedObjectReference, O},
	{O, ahm:title, T}\
	{S, ahm:relatedObjectTitle, T}
	<=>
	true.

% ----------- PART OF ----------------
%
% Replace literal with object uri (think about partsTitle?)

partofref @@
{Obj, ahm:objectNumber, ON}\
{S, ahm:partOfReference, ON}
<=>
{S, ahm:partOfReference, Obj}.

partofref @@
{Obj, ahm:objectNumber, ON}\
{S, ahm:partsReference, ON}
<=>
{S, ahm:partsReference, Obj}.



% RULES BELOW ARE BY JAN
%
% All text classification seems tripled, with lang=neutral, lang=0 and
% lang=1.  This rule deletes all the ones except for the neutral one.


clean_empty @@
{ _, _, "" } <=> true.

clean_selected @@
{ _, ahm:selected, "False" } <=> true.



text_clean @@
{ N, ahm:value, TextType },
{ TextType, rdf:value, Value} \
{ TextType, ahm:lang, "neutral"},
{ N, ahm:value, TextType2 },
{ TextType2, rdf:value, Value},
{ TextType2, _, _}
	<=> TextType \== TextType2.

webtextlang @@
{T,  ahm:'AHMTextsTekst', Text},
{T,  ahm:'AHMTextsType', BN1},
{BN1, rdf:type, ahm:'AHMTextsType'},
{BN1, ahm:value, BN},
{BN, rdf:value, "webtekst ENG"},
{BN, _, _}
	<=>
	{T, ahm:'AHMTextsType', "webtekst"},
	{T, ahm:'AHMTextsTekst', Text@en}.
webtextlang @@
{T,  ahm:'AHMTextsTekst', Text},
{T,  ahm:'AHMTextsType', BN1},
{BN1, rdf:type, ahm:'AHMTextsType'},
{BN1, ahm:value, BN},
{BN, rdf:value, "webtekst NL"},
{BN, _, _}
	<=>
	{T, ahm:'AHMTextsType', "webtekst"},
	{T, ahm:'AHMTextsTekst', Text@nl}.
webtextlang @@
{T,  ahm:'AHMTextsTekst', Text},
{T,  ahm:'AHMTextsType', BN1},
{BN1, rdf:type, ahm:'AHMTextsType'},
{BN1, ahm:value, BN},
{BN, rdf:value, "zaaltekst ENG"},
{BN, _, _}
	<=>
	{T, ahm:'AHMTextsType', "zaaltekst"},
	{T, ahm:'AHMTextsTekst', Text@nl}.
webtextlang @@
{T,  ahm:'AHMTextsTekst', Text},
{T,  ahm:'AHMTextsType', BN1},
{BN1, rdf:type, ahm:'AHMTextsType'},
{BN1, ahm:value, BN},
{BN, rdf:value, "zaaltekst NL"},
{BN, _, _}
	<=>
	{T, ahm:'AHMTextsType', "zaaltekst"},
	{T, ahm:'AHMTextsTekst', Text@nl}.


% Added by Victor: takes care of the category
webtextcat @@
{T,  ahm:'AHMTextsType', BN1},
{BN1, rdf:type, ahm:'AHMTextsType'},
{BN1, ahm:value, BN},
{BN, rdf:value, literal(Lit)},
{BN, _, _}
	<=>
	{T, ahm:'AHMTextsType', literal(Lit)}.


% clean
webtextcat @@
{_T,  ahm:'AHMTextsType', BN1},
{BN1, rdf:type, ahm:'AHMTextsType'}
<=>
true.
% clean
webtextcat @@
{_,  ahm:value, _}
<=>
true.
webtextcat @@
{_, rdf:type, ahm:'Value'}
<=>
true.


		 /*******************************
		  *   MAP TO THESAURUS	        *
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The following rules replace a literal value by the thesaurus skos
concept for a given set of properties. Each property uses a separate
rule. */

labelPred(skos:prefLabel).
labelPred(skos:altLabel).

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:acquisitionMethod, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:acquisitionMethod, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:alternativeNumberInstitution, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:alternativeNumberInstitution, C}.
map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:associationSubject, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:associationSubject, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:contentMotifGeneral, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:contentMotifGeneral, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:contentSubject, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:contentSubject, C}.

%clean
map_to_thesaurus @@
 {P, rdf:type, ahm:'ContentSubject'},
 {_, ahm:contentSubject,P}
 <=>
 true.


% maybe not this one (is always AHM
map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:creditLine, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:creditLine, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:currentLocation, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:currentLocation, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:currentLocationFitness, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:currentLocationFitness, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:dimensionNotes, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:dimensionNotes, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:dimensionPart, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:dimensionPart, C}.


map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:dimensionType, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:dimensionType, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:dimensionUnit, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:dimensionUnit, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:documentationTitle, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:documentationTitle, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:exhibitionVenue, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:exhibitionVenue, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:material, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:material, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:objectCategory, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:objectCategory, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:objectName, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:objectName, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:productionPeriod, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:productionPeriod, C}.
%clean
map_to_thesaurus @@
 {P, rdf:type, ahm:'ProductionPlace'},
 {_, ahm:productionPlace,P}
 <=>
 true.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:productionPlace, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:productionPlace, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:reproductionType, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:reproductionType, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:technique, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:technique, C}.

map_to_thesaurus @@
{C, rdf:type, skos:'Concept'},
	{C, P, literal(Lit)}\
	{S, ahm:collection, literal(Lit)}
	<=>
	labelPred(P),
	{S, ahm:collection, C}.


		 /*******************************
		  *	     EDM                *
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Given a nice DC model, we now change this into an EDM model.  To do so,
we must create three resources:

    1. the aggregation
    2. the physical object
    3. a proxy with the description

The proxy is where the description lives, so we use the initial record
with DC attributes for that.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- rdf_register_ns(ens,  'http://www.europeana.eu/schemas/edm/').
:- rdf_register_ns(irw,  'http://ontologydesignpatterns.org/ont/web/irw.owl#').
:- rdf_register_ns(ore,  'http://www.openarchives.org/ore/terms/').

edm @@
{S, rdf:type, ahm:'Record'},
{S, ahm:reproduction, Reproduction } ? % reproduction becomes a hasView
	<=>
	edm_identifier(S, proxy, aggregation, AggURI),
	edm_identifier(S, proxy, physical, PhysURI),
	{ S, rdf:type, ore:'Proxy'},
	{ AggURI, rdf:type, ore:'Aggregation' },
	{ PhysURI, rdf:type, ens:'PhysicalThing' },
	{ S, ore:proxyIn, AggURI },
        { S, ore:proxyFor, PhysURI },
	{ AggURI, ore:aggregates, PhysURI },
	{ AggURI, ens:aggregatedCHO, PhysURI },
	{ AggURI, ens:hasView, Reproduction }.


% ------------- Thumbnails -------------
% This guesses the thumbnail uri, based on the object number
%
get_thumbnails @@
{S, ahm:objectNumber, literal(ON)}
==>
	edm_identifier(S, proxy, aggregation, AggURI),
	object_number_to_url(ON, URL),
   { AggURI, ens:hasThumbnail, URL },	   % create new property with link to the ens:WebResource
   { URL, rdf:type, ens:'WebResource' }.






