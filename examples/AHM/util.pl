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
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_cookie)).

:- rdf_register_ns(ahm,	   'http://purl.org/collections/nl/am/').
:- rdf_register_ns(ens,	   'http://www.europeana.eu/schemas/edm/').
:- rdf_register_ns(ore,    'http://www.openarchives.org/ore/terms/').
:- rdf_register_ns(rdf,     'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

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
object_number_to_url(ON,URL):-
	space_to_underscore(ON, ON1),
	atom_concat('http://ahm.adlibsoft.com/wwwopacx/wwwopac.ashx?command=retrievecontent&imageserver=images&value=',ON1, Almost),
	atom_concat(Almost,'.jpg',URL).


space_to_underscore(In, Out) :-
	atom_codes(In, CodesIn),
	maplist(map_space, CodesIn, CodesOut),
	atom_codes(Out, CodesOut).

map_space(0' , 0'_) :- !.
map_space(0'., 0'_) :- !.
map_space(C, C).



% Language for vocabulary rewrite
lang_to_langcode('0',en).
lang_to_langcode('1',nl).
lang_to_langcode('3',de).




% Image checker
%
%
lp1('http://collectie.ahm.nl/dispatcher.aspx?action=detail&database=ChoiceCollect&priref=14233').
lp2('http://collectie.ahm.nl/dispatcher.aspx?action=detail&database=ChoiceCollect&priref=14234').

check_all_img:-
	findall(Agg-LP, rdf(Agg, ens:landingPage, LP),List),
	assert(todolist(List)),
	forall(member(A-LP,List), (check_img(A-LP),sleep(2))).

check_img(A-LP):-
	remove_existing(A),
	(   get_img_from_lp(LP, Image) ->
	    (   rdf_assert(A, ens:object, Image),
	        rdf_assert(Image, rdf:type, ens:'WebResource')
	    );
	    true),
	format('\tdone ~w\n',[A]).

remove_existing(A):-
	rdf(A, ens:object, WR),
	rdf_retractall(A, ens:object, WR),
	rdf_retractall(WR, rdf:type, _).
remove_existing(_).


get_img_from_lp(LandingURL, ImageURL):-
	http_open(LandingURL, S, []),
	load_html(S, Term),
	sub_term(element(img, Attr, _), Term),
	member(class=detailImage,Attr),
	member(src=ImageURL, Attr),
	close(S).

load_html(URL, DOM) :-
       setup_call_cleanup(http_open(URL, In, []),
                          (   dtd(html, DTD),
                              load_structure(stream(In),
                                             DOM,
                                             [ dtd(DTD),
                                               dialect(sgml),
                                               shorttag(false),
                                               syntax_errors(quiet)
                                             ])
                          ),
                          close(In)).

