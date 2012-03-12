/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
		   VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(xmlrdf,
	  [ load_xml_as_rdf/2,		% +Input, +Options
	    xmldom_to_rdf/2,		% +DOM, +Options
	    xmldom_rdf_properties/3	% +URI, +DOM, +Options
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(uri)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(xsdp_types)).
:- use_module(library(record)).
:- use_module(library(apply)).

:- use_module(library(skos_schema)).
:- use_module(library(foaf_schema)).

:- rdf_register_ns(map, 'http://cs.vu.nl/eculture/map/').

/** <module> Generic translation from XML to RDF
*/

:- record
	option(units:list=[],
	       dialect:oneof([xml,xmlns])=xmlns,
	       graph:atom=data,
	       prefix:atom=(-),
	       use_schema:boolean=true,
	       class_style:oneof(['OneTwo','oneTwo',
				  'one_two','One_Two',keep])='OneTwo',
	       predicate_style:oneof(['OneTwo','oneTwo',
				      'one_two','One_Two',keep])='oneTwo').

%%	load_xml_as_rdf(From, Options)
%
%	Convert an XML file into `crude' RDF. From is either a filename,
%	a  URL  (using  either  =file=  or  =http=  scheme)  or  a  term
%	stream(Stream). Options is a list of the following options:
%
%	    * unit(+Elements)
%	    If provided, consider elements whose name match one of
%	    the members of the list Elements a toplevel structure
%	    and process the file one element at a time.  If there
%	    is just one toplevel structure, this may be passed without
%	    using a list.
%
%	    * dialect(Dialect)
%	    One of =xml= or =xmlns=.  Use =xmlns= if the file contains
%	    xmlns= attributes and XML names of the form ns:local.  If
%	    neither is present, the file must be processed using the
%	    =xml= dialect.
%
%	    * graph(+Graph)
%	    RDF Graph for storing the output.  Default is =data=
%
%	    * use_schema(+Boolean)
%	    If =true= (default), use schema information to guide the
%	    translation.
%
%	    * prefix(+Prefix)
%	    Create a URI from an XML name by putting Prefix in front
%	    of it.  If we are processing _xmlns_ (see dialect), the
%	    XML namespace declaration is ignored.  I.e., the URI is
%	    formed from Prefix followed by the XML local name.
%
%	    If this option is not present the URI is simply the XML
%	    name if the dialect is =xml= or the XML namespace followed
%	    by the local name if the dialect is =xmlns=.
%
%	    * predicate_style(+Style)
%	    Change the `identifier style' for RDF predicates creates
%	    from XML names.  The default is 'oneTwo'.  Other values are
%	    'OneTwo', 'one_two' or 'One_Two'.  The value =keep= uses
%	    the XML name directly as RDF name.  This is valid, but
%	    often leads to names that cannot be written using the Turtle
%	    and SPARQL shorthand notation.
%
%	    * class_style(+Style)
%	    Same as predicate_style, but used when generating a
%	    class-name.  The default is 'OneTwo'.

load_xml_as_rdf(From, Options) :-
	canonical_unit_option(Options, COptions),
	make_option(COptions, Record, _Rest),
	flush_name_uri_cache,
	flush_property_map,
	rdf_statistics(triples(C0)),
	statistics(cputime, T0),
	setup_call_cleanup(open_input(From, In, Cleanup),
			   process(In, Record),
			   Cleanup),
	statistics(cputime, T1),
	rdf_statistics(triples(C1)),
	T is T1-T0,
	C is C1-C0,
	print_message(informational, xmlrdf(loaded(From, T, C))).

canonical_unit_option(Options, COptions) :-
	select_option(unit(Unit), Options, Rest), !,
	to_list(Unit, Units),
	COptions = [units(Units)|Rest].
canonical_unit_option(Options, Options).

to_list(List, List) :-
	is_list(List), !.
to_list(Elem, [Elem]).

%%	open_input(+Spec, -Stream, -Close)
%
%	Open the input Spec, returning  Stream   and  a closure Close to
%	revert the side-effects.

open_input(stream(In), In, true) :- !.
open_input(URL, In, Cleanup) :-
	atom(URL),
	uri_file_name(URL, File), !,
	open_file(File, In, Cleanup).
open_input(URL, In, Cleanup) :-
	atom(URL),
	uri_components(URL, Data),
	uri_data(scheme, Data, http), !,
	http_open(URL, In, []),
	set_stream(In, file_name(URL)),
	Cleanup = close(In).
open_input(Spec, In, Cleanup) :-
	absolute_file_name(Spec, Path, [extensions([gz,'']), access(read)]),
	open_file(Path, In, Cleanup).

open_file(Path, In, Cleanup) :-
	(   file_name_extension(_, gz, Path)
	->  gzopen(Path, read, In, [type(binary)])
	;   open(Path, read, In, [type(binary)])
	),
	Cleanup = close(In).


%%	process(+Stream, +Options)

process(Stream, Options) :-
	option_units(Options, Units),
	Units \== [], !,
	b_setval(xmlrdf_unit, Units),
	b_setval(xmlrdf_options, Options),
	setup_call_cleanup(new_sgml_parser(Parser, []),
			   (   configure_parser(Parser, Options),
			       sgml_parse(Parser,
					  [ source(Stream),
					    call(begin, on_begin)
					  ])
			   ),
			   free_sgml_parser(Parser)).
process(Stream, Options) :-
	setup_call_cleanup(new_sgml_parser(Parser, []),
			   (   configure_parser(Parser, Options),
			       sgml_parse(Parser,
					  [ source(Stream),
					    document(Document)
					  ])
			   ),
			   free_sgml_parser(Parser)),
	Document = [Element],
	convert(Element, Options).


configure_parser(Parser, Options) :-
	option_dialect(Options, Dialect),
	set_sgml_parser(Parser, dialect(Dialect)),
	set_sgml_parser(Parser, space(sgml)).


on_begin(Element, Attr, Parser) :-
	b_getval(xmlrdf_unit, Elements),
	memberchk(Element, Elements), !,
	b_getval(xmlrdf_options, Options),
	sgml_parse(Parser,
		   [ document(Content),
		     parse(content)
		   ]),
	convert(element(Element, Attr, Content), Options).


		 /*******************************
		 *	  RDF CONVERSION	*
		 *******************************/

%%	xmldom_to_rdf(+DOM, +Options) is det.
%
%	Convert an XML DOM into RDF  and   assert  the  RDF into the RDF
%	store. Options:
%
%	    * uri(URI)
%	    URI for the topmost element
%	    * type(URI)
%	    rdf:type URI for the topmost element. If provided, we assume
%	    that the triple is already added.
%	    * graph(+Graph)
%	    Named graph in which to store the results.

xmldom_to_rdf(DOM, Options) :-
	make_option(Options, Record, _),
	option_graph(Record, Graph),
	element_uri(DOM, URI, Options),
	(   option(type(Type), Options)
	->  true
	;   element_type(DOM, Type, Record),
	    rdf_assert(URI, rdf:type, Type, Graph)
	),
	set_properties(URI, DOM, Record).

%%	xmldom_rdf_properties(+URI, +DOM, +Options) is det.
%
%	Assign properties to URI from the given XML DOM element.

xmldom_rdf_properties(URI, DOM, Options) :-
	make_option(Options, Record, _),
	set_properties(URI, DOM, Record).


%%	convert(+Element, +Options) is det.

convert(Element, Options) :-
	option_graph(Options, Graph),
	element_uri(Element, URI),
	element_type(Element, Type, Options),
	rdf_assert(URI, rdf:type, Type, Graph),
	set_properties(URI, Element, Options).

element_uri(_Element, URI, Options) :-
	option(uri(URI), Options), !.
element_uri(Element, URI, _Options) :-
	element_uri(Element, URI),
	rdf_bnode(URI).

element_uri(_Element, URI) :-
	rdf_bnode(URI).

element_type(element(Name, _, _), Class, _) :-
	rdf(Class, map:xmlname, literal(Name)),
	rdfs_individual_of(Class, rdfs:'Class'), !.
element_type(element(EName, _, _), Name, Options) :-
	name_to_uri(EName, class, Name, Options),
	debug(xmlrdf(type), 'No element type for element-name ~p', [Name]).


		 /*******************************
		 *	     PROPERTIES		*
		 *******************************/

%%	set_properties(+URI, +Element, +Options) is det.

set_properties(URL, element(_, Attrs, Content), Options) :-
	set_properties(URL, element(_, Attrs, Content), -, Options).

set_properties(URL, element(_, Attrs, Content), Lang, Options) :-
	setp_from_attributes(Attrs, URL, Lang, Lang1, Options),
	setp_from_content(Content, URL, Lang1, Options).

setp_from_attributes([], _, Lang, Lang, _).
setp_from_attributes([xmlns:_=_|T], URL, Lang0, Lang, Options) :- !,
	setp_from_attributes(T, URL, Lang0, Lang, Options).
setp_from_attributes([xmlns=_|T], URL, Lang0, Lang, Options) :- !,
	setp_from_attributes(T, URL, Lang0, Lang, Options).
setp_from_attributes([xml:_=_|T], URL, Lang0, Lang, Options) :- !,
	setp_from_attributes(T, URL, Lang0, Lang, Options).
setp_from_attributes([AttName=Value|T], URL, Lang0, Lang, Options) :-
	map_literal_property(URL, AttName, Prop, Options),
	option_graph(Options, Graph),
	(   Lang0 == (-)
	->  rdf_assert(URL, Prop, literal(Value), Graph)
	;   rdf_assert(URL, Prop, literal(lang(Lang0, Value)), Graph)
	),
	setp_from_attributes(T, URL, Lang0, Lang, Options).


%%	setp_from_content(+Content, +URL, +Lang, +Options) is det.
%
%	Create  attributes  for  URL  from  the  given  content.  If  we
%	encounter CDATA, this is typically  from   an  element  that has
%	attributes. We use rdf:value for the property in this case.

setp_from_content([], _, _, _).
setp_from_content([element(Name, Attrs0, Content)|T], URL, Lang, Options) :- !,
	exclude(xmlns_property, Attrs0, Attrs),
	setp_from_content_element(element(Name, Attrs, Content), URL,
				  Lang, Options),
	setp_from_content(T, URL, Lang, Options).
setp_from_content([Text|T], URL, Lang, Options) :-
	make_literal_value(Lang, Text, Value),
	option_graph(Options, Graph),
	rdf_assert(URL, rdf:value, Value, Graph),
	setp_from_content(T, URL, Lang, Options).


xmlns_property(xmlns=_) :- !.
xmlns_property(xmlns:_=_) :- !.
xmlns_property(P=_) :-
	atom(P),
	sub_atom(P, 0, _, _, 'xmlns:'), !.


%%	setp_from_content_element(+Element, +URL, +Lang, +Options) is det.
%
%	Create a property for URL from the   XML element Element. If the
%	property is mapped, we know the property and target datatype. If
%	not, we must decide whether to go for  a literal or an bnode. If
%	all data can be expressed as  a   literal,  we use a literal and
%	else we create a bnode.  We  can   only  express  the  data as a
%	literal if it  has  no  attributes   or  the  only  attribute is
%	xml:lang.

setp_from_content_element(element(EName, AL, CL), URL, Lang, Options) :-
	mapped_property(URL, EName, Prop, Type, Options), !,
	debug(xmlrdf(pmap), '~p ~p', [URL, Prop]),
	make_value(EName, AL, CL, Type, Value, Lang, Options),
	option_graph(Options, Graph),
	rdf_assert(URL, Prop, Value, Graph).
setp_from_content_element(element(EName, [], [Text]), URL, Lang, Options) :-
	atom(Text), !,
	name_to_uri(EName, predicate, Prop, Options),
	make_literal_value(Lang, Text, Value),
	option_graph(Options, Graph),
	rdf_assert(URL, Prop, Value, Graph).
setp_from_content_element(element(EName, [], []), URL, _, Options) :- !,
	name_to_uri(EName, predicate, Prop, Options),
	option_graph(Options, Graph),
	rdf_assert(URL, Prop, literal(''), Graph).
setp_from_content_element(element(EName, [xml:lang=Lang], [Text]),
			  URL, _, Options) :-
	atom(Text), !,
	name_to_uri(EName, predicate, Prop, Options),
	make_literal_value(Lang, Text, Value),
	option_graph(Options, Graph),
	rdf_assert(URL, Prop, Value, Graph).
setp_from_content_element(element(EName, Attrs0, Content),
			  URL, Lang, Options) :-
	name_to_uri(EName, predicate, Prop, Options),
	name_to_uri(EName, class, Type, Options),
	(   select(xml:lang=Lang1, Attrs0, Attrs)
	->  true
	;   Lang1 = Lang,
	    Attrs = Attrs0
	),
	make_value(EName, Attrs, Content, Type, Value, Lang1, Options),
	option_graph(Options, Graph),
	rdf_assert(URL, Prop, Value, Graph).

make_literal_value(-,    Text, literal(Text)) :- !.
make_literal_value(Lang, Text, literal(lang(Lang, Text))).


%%	make_value(+Element, +Attributes, +Content, +Type, -Value,
%%		   +Lang, +Options)

make_value(_, Atts, Content, Literal, literal(Value), Lang, _) :-
	rdf_equal(rdfs:'Literal', Literal),
	(   Content = [Text],
	    atom(Text)
	->  true
	;   Content == []
	->  Text = ''
	), !,
	(   memberchk(xml:lang=TheLang, Atts)
	->  Value = lang(TheLang, Text)
	;   Lang = (-)
	->  Value = Text
	;   Value = lang(Lang, Text)
	).
make_value(_, _, [Text], Type, literal(type(Type, Text)), _, _) :-
	atom(Text),
	datatype_type(Type), !.
make_value(_, _, [], Type, literal(type(Type, '')), _, _) :-
	datatype_type(Type), !.
make_value(_, Attrs, Content, Type, literal(type(XMLLit, Content)), _, _) :-
	rdf_equal(rdf:'XMLLiteral', XMLLit),
	maplist(xml_attribute, Attrs),
	(   Type = XMLLit
	;   rdf_equal(rdfs:'Literal', Type)
	), !.
make_value(Element, Attrs, Content, Type, ValueURI, Lang, Options) :-
	rdf_equal(rdf:'XMLLiteral', XMLLit),
	(   Type = XMLLit
	;   rdf_equal(rdfs:'Literal', Type)
	), !,
	element_uri(element(Element, Attrs, Content), ValueURI),
	option_graph(Options, Graph),
	rdf_assert(ValueURI, rdf:type, Type, Graph),
	setp_from_attributes(Attrs, ValueURI, Lang, _Lang1, Options),
	rdf_assert(ValueURI, rdf:value, literal(type(XMLLit, Content)),Graph).
make_value(_, [], [URL], Type, URL, _, _) :-
	atom(URL),
	rdfs_subclass_of(Type, rdfs:'Resource'), !.
make_value(Element, Attrs, Content, Type, ValueURI, Lang, Options) :-
	element_uri(element(Element, Attrs, Content), ValueURI),
	option_graph(Options, Graph),
	rdf_assert(ValueURI, rdf:type, Type, Graph),
	setp_from_attributes(Attrs, ValueURI, Lang, Lang1, Options),
	setp_from_content(Content, ValueURI, Lang1, Options).

datatype_type(URI) :-
	xsdp_uri_type(URI, _Type).

xml_attribute(xml:_=_).
xml_attribute(xmlns:_=_).


		 /*******************************
		 *	       MAP		*
		 *******************************/

%%	map_literal_property(+Subject, +XMLName, -RDFProperty, +Options)
%%		is det.
%
%	RDFProperty is a URI for the   property  indicated in the source
%	with XMLName. The property will be  added to Subject. Subject is
%	guaranteed to have an rdf:type when this predicate is called.

:- thread_local
	literal_property_map/3,
	property_map/5.

flush_property_map :-
	retractall(literal_property_map(_,_,_)),
	retractall(property_map(_,_,_,_,_)).

map_literal_property(Subject, XMLName, RDFProperty, Options) :-
	rdf(Subject, rdf:type, Class),
	(   literal_property_map(XMLName, Class, RDFProperty)
	->  true
	;   map_lprop_class(Subject, XMLName, RDFProperty, Options),
	    assert(literal_property_map(XMLName, Class, RDFProperty))
	).

map_lprop_class(Subject, XMLName, RDFProperty, Options) :-
	mapped_property(Subject, XMLName, RDFProperty, _Type, Options), !.
map_lprop_class(_Subject, XMLName, Prop, Options) :-
	name_to_uri(XMLName, predicate, Prop, Options),
	rdf_equal(rdfs:'Literal', Literal),
	update_schema(XMLName, Prop, Literal).

update_schema(XMLName, Prop, Type) :-
	name_to_atom(XMLName, Atom),
	(   rdfs_individual_of(Prop, rdf:'Property')
	->  true
	;   rdf_assert(Prop, rdf:type, rdf:'Property', schema)
	),
	(   rdf_has(Prop, rdfs:range, _Range)
	->  true
	;   rdf_assert(Prop, rdfs:range, Type, schema)
	),
	(   rdf_has(Prop, map:xmlname, _)
	->  true
	;   rdf_assert(Prop, map:xmlname, literal(Atom), schema)
	).

%%	mapped_property(+Subject, +XMLName,
%%			-RDFProperty, -RDFType, +Options) is semidet.
%
%	True if XMLName is mapped  to   RDFProperty  with  the given RDF
%	type. There are three ways  to  decide   that  we  deal  with an
%	established mapping:
%
%	    1. There is a property with map:xmlname with a literal value
%	    that matches the XML Name.  Namespace qualified names are
%	    written as <prefix><local>
%
%	    2. There is a property with map:xmlname that matches the
%	    default translation.
%
%	    3. There is a property with a URI that matches the default
%	    translation that has a type.

mapped_property(Subject, XMLName, Prop, Type, Options) :-
	option_use_schema(Options, true),
	rdf(Subject, rdf:type, Class),
	(   property_map(XMLName, Class, Prop, Type, Mapped)
	->  Mapped == true
	;   mapped_property_nc(Subject, XMLName, Prop, Type, Options)
	->  assert(property_map(XMLName, Class, Prop, Type, true))
	;   assert(property_map(XMLName, Class, _, _, false)),
	    fail
	).

mapped_property_nc(_Subject, XMLName, Prop, Type, Options) :-
	name_to_uri(XMLName, predicate, Prop0, Options),
	(   (   name_to_atom(XMLName, Atom),
		rdf(Prop, map:xmlname, literal(Atom))
	    ;	rdf(Prop, map:xmlname, Prop0)
	    ),
	    rdfs_individual_of(Prop, rdf:'Property')
	->  (   rdf(Prop, rdfs:range, Type)
	    ->  true
	    ;   rdf_equal(rdfs:'Literal', Type)
	    )
	;   Prop = Prop0,
	    rdf(Prop0, rdfs:range, Type)
	->  true
	).

name_to_atom(Prefix:Local, Name) :-
	atom_concat(Prefix, Local, Name).
name_to_atom(Name, Name).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

%%	name_to_uri(+XMLName, +Type, -URI, +Options) is det.
%
%	@param	XMLName is an atom for dialect =xml= or a term
%		Prefix:Local when using the =xmlns= dialect.
%	@param	Type is one of =class= or =predicate=
%	@URI	Is an RDF resource (atom)

:- thread_local
	name_uri_cache/4.

flush_name_uri_cache :-
	retractall(name_uri_cache(_,_,_,_)).

name_to_uri(NS:Local, Type, URI, Options) :- !,
	(   name_uri_cache(Local, NS:Local, Type, URI)
	->  true
	;   name_to_uri_nc(NS:Local, Type, URI, Options),
	    assert(name_uri_cache(Local, NS:Local, Type, URI))
	).
name_to_uri(Name, Type, URI, Options) :-
	(   name_uri_cache(Name, Name, Type, URI)
	->  true
	;   name_to_uri_nc(Name, Type, URI, Options),
	    assert(name_uri_cache(Name, Name, Type, URI))
	).

name_to_uri_nc(NS:Local, Type, URI, Options) :- !,
	restyle(Type, Local, Local1, Options),
	(   option_prefix(Options, Prefix),
	    Prefix \== (-)
	->  atom_concat(Prefix, Local1, URI)
	;   atom_concat(NS, Local1, URI)
	).
name_to_uri_nc(Name, Type, URI, Options) :-
	restyle(Type, Name, Name1, Options),
	(   option_prefix(Options, Prefix),
	    Prefix \== (-)
	->  atom_concat(Prefix, Name1, URI)
	;   URI = Name1
	).

restyle(predicate, Name0, Name, Options) :-
	option_predicate_style(Options, Style),
	(   Style == keep
	->  Name = Name0
	;   restyle_identifier(Style, Name0, Name)
	).
restyle(class, Name0, Name, Options) :-
	option_class_style(Options, Style),
	(   Style == keep
	->  Name = Name0
	;   restyle_identifier(Style, Name0, Name)
	).


		 /*******************************
		 *	    IDENTIFIERS		*
		 *******************************/

%%	restyle_identifier(+Style, +In, -Out) is det.
%
%	Restyle an identifier by extracting the alnum substrings and
%	joining them together according to Style.
%
%	@param Style is described with join_name_parts/3.

restyle_identifier(Style, In, Out) :-
	name_parts(In, Parts),
	join_name_parts(Style, Parts, Out).


%%	name_parts(+Identifier, -Parts) is det.
%
%	Parts is a list of atoms  that   make  up  Identifier. The parts
%	found are turned into lowercase, unless   all its characters are
%	uppercase.  E.g.,
%
%	==
%	?- name_parts('sourceCodeURI', X).
%	X = [source, code, 'URI'].
%	==

name_parts(Name, Parts) :-
	atom_codes(Name, Codes),
	phrase(name_parts(Parts), Codes).

name_parts([H|T]) -->
	name_part(H), !,
	name_parts(T).
name_parts([]) --> [].

name_part(H) -->
	string(Codes, Tail),
	sep(Tail), !,
	{ Codes = [_|_],
	  atom_codes(H0, Codes),
	  (   maplist(is_upper, Codes)
	  ->  H = H0
	  ;   downcase_atom(H0, H)
	  )
	}.

string(T,T) --> [].
string([H|T], L) --> [H], string(T, L).

sep([]) --> sep_char, !, sep_chars.
sep([T]), [N] -->
	[T,N],
	{ code_type(T, lower),
	  code_type(N, upper)
	}.
sep([],[],[]).

sep_char -->
	[H],
	{ \+ code_type(H, alnum) }.

sep_chars --> sep_char, !, sep_chars.
sep_chars --> [].

%%	join_name_parts(+Style, +Parts, -Identifier)
%
%	Join parts of an identifier according to Style.  Style is
%	one of:
%
%	    * 'OneTwo'
%	    * oneTwo
%	    * one_two
%	    * 'One_Two'

join_name_parts(Style, [First|Parts], Identifier) :-
	style(Style, CapFirst, CapRest, Sep),
	capitalise(CapFirst, First, H),
	maplist(capitalise(CapRest), Parts, T),
	atomic_list_concat([H|T], Sep, Identifier).

style('OneTwo',	 true,	true,  '').
style(oneTwo,	 false,	true,  '').
style(one_two,	 false,	false, '_').
style('One_Two', true,	true,  '_').

capitalise(false, X, X) :- !.
capitalise(true, X, Y) :-
	atom_codes(X, [H0|T]),
	code_type(H0, to_lower(H)),
	atom_codes(Y, [H|T]).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(xmlrdf(loaded(From, Time, Count))) -->
	[ 'Loaded ~D triples in ~3f seconds from ~p'-[Count, Time, From] ].
