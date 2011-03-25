:- module(ahm_convert_people,
	  [ run_people/0
	  ]).

user:file_search_path(data,       metadata('AHM')).

:- load_files(library(semweb/rdf_db), [silent(true)]).

:- rdf_register_ns(ahm,	   'http://purl.org/collections/nl/am/').
:- rdf_register_ns(ulan,   'http://e-culture.multimedian.nl/ns/getty/ulan#').
:- rdf_register_ns(aatned, 'http://e-culture.multimedian.nl/ns/rkd/aatned/').
:- rdf_register_ns(skos,   'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_ns(foaf,   'http://xmlns.com/foaf/0.1/').

:- load_files([ cliopatria(cliopatria),
		library(xmlrdf/xmlrdf),
		library(semweb/rdf_cache),
		library(semweb/rdf_library),
		library(semweb/rdf_turtle_write)
	      ], [silent(true)]).
:- use_module(rewrite_people).

load_ontologies :-
	rdf_attach_library(cliopatria(ontologies)),
	rdf_load_library(dc),
	rdf_load_library(skos),
	rdf_load_library(rdfs),
	rdf_load_library(owl).


:- initialization			% run *after* loading this file
	ensure_dir(cache),
	rdf_set_cache_options([ global_directory('cache/rdf'),
				create_global_directory(true)
			      ]),
	load_ontologies.


ensure_dir(Dir) :-
	exists_directory(Dir), !.
ensure_dir(Dir) :-
	make_directory(Dir).



load_people:-
        absolute_file_name(data('src/people.xml'), File,
			   [ access(read)
			   ]),
	load(File).

load(File) :-
	rdf_current_ns(ahm, Prefix),
	load_xml_as_rdf(File,
			[ dialect(xml),
			  unit(record),
			  prefix(Prefix),
			  graph(people)
			]).


run_people:-
	load_people,
	rewrite,
	rdf_assert(ahm:'AM_PeopleScheme',rdf:type, skos:'ConceptScheme',people),
	rdf_assert(ahm:'AM_PeopleScheme', rdfs:label, literal('AM People thesaurus'),people),
	save_thesaurus.

save_thesaurus:-
	absolute_file_name(data('rdf/am-people.ttl'), File,
			   [ access(write)
			   ]),
	rdf_save_turtle(File,[graph(people)]).
