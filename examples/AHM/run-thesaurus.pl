:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(ahm, Dir)).

user:file_search_path(xmlrdf,     ahm('..')).
user:file_search_path(data,       ahm('../../AHM')).
user:file_search_path(cliopatria, ahm('../../ClioPatria')).
user:file_search_path(getty,      ahm('../../../eculture/RDF/vocabularies/getty')).

:- load_files(library(semweb/rdf_db), [silent(true)]).

:- rdf_register_ns(ahm,	   'http://purl.org/collections/ahm/').
:- rdf_register_ns(ulan,   'http://e-culture.multimedian.nl/ns/getty/ulan#').
:- rdf_register_ns(aatned, 'http://e-culture.multimedian.nl/ns/rkd/aatned/').
:- rdf_register_ns(skos,   'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_ns(foaf,   'http://xmlns.com/foaf/0.1/').

:- load_files([ cliopatria(cliopatria),
		xmlrdf(xmlrdf),
		library(semweb/rdf_cache),
		library(semweb/rdf_library),
		library(semweb/rdf_turtle_write)
	      ], [silent(true)]).
:- use_module(rewrite_thes).

load_ontologies :-
	rdf_attach_library(cliopatria(ontologies)),
	rdf_load_library(dc),
	rdf_load_library(skos),
	rdf_load_library(rdfs),
	rdf_load_library(owl),
	absolute_file_name(data('rdf/ahm-voc-schema.ttl'), VocSchema, [access(read)]),
	rdf_load(VocSchema,[graph(thesaurus_schema)]).

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



load_thesaurus:-
        absolute_file_name(data('src/thesaurus.xml'), File,
			   [ access(read)
			   ]),
	load(File).

load(File) :-
	rdf_current_ns(ahm, Prefix),
	load_xml_as_rdf(File,
			[ dialect(xml),
			  unit(record),
			  prefix(Prefix),
			  graph(thesaurus)
			]).


run_thesaurus:-
	load_thesaurus,
	rewrite_thes:rewrite,
	save_thesaurus.

save_thesaurus:-
	absolute_file_name(data('rdf/thesaurus.ttl'), File,
			   [ access(write)
			   ]),
	rdf_save_turtle(File,[graph(thesaurus)]).
