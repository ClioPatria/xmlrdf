% Utility predicates for

:- module(ahm_util,
	  [ role_to_property/2,	 	 % +Role, -SubProperty
	    concat_maybe/2,              % +ListofArgs, -OutputLiteral
	    concat_maybe/3,              % +ListofArgs, +Separator, -OutputLiteral
	    object_number_to_url/2,	 % +Object Number -URL
	    lang_to_langcode/2		 % +AHMLangNr, -IsoLangCode
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xmlrdf/rdf_convert_util)).


% Utility preds that make a creator subproperty based on creator Role.
% If Role = "", use ahm:maker.

%role_to_property('', 'http://purl.org/collections/ahm/maker').
role_to_property(Role, Property):-
	rp1(Role, Literal),
	literal_to_id([Literal],ahm,Property).

rp1('', maker).
rp1('glasblazer', glasblazer).
rp1('glasgraveur',glasgraveur).
rp1('goud- en zilversmid', goud_en_zilversmid).
rp1('glasschilder',glasschilder).
rp1('schilder', schilder).
rp1('graveur', graveur).

% ConcatMaybe concatenates list of atoms, skipping the unbound
% variables. This is used to make labels for bnodes. A separator can
% also be passed
%

concat_maybe(List, Literal):-
	concat_maybe(List, ' ', Literal).

concat_maybe([],_, '').
concat_maybe([A|List],Sep, Literal):-
        nonvar(A),
	atom_concat(A,Sep,Pre),
	concat_maybe(List,Sep, Post),
	atom_concat(Pre,Post,Literal).
concat_maybe([A|List],Sep, Literal):-
        var(A),
	concat_maybe(List,Sep, Literal).

% 'guess' the thumbnail url
%
object_number_to_url(ON,URL):-
	space_to_underscore(ON, ON1),
	atom_concat('http://ahm.adlibsoft.com/wwwopacx/wwwopac.ashx?command=retrievecontent&imageserver=images&value=',ON1, Almost),
	atom_concat(Almost,'.jpg',URL).

space_to_underscore(In, Out) :-
	atom_codes(In, CodesIn),
	maplist(map_space, CodesIn, CodesOut),
	atom_codes(Out, CodesOut).

map_space(0' , 0'_) :- !.
map_space(C, C).



% Language for vocabulary rewrite
lang_to_langcode('0',en).
lang_to_langcode('1',nl).
lang_to_langcode('3',de).
