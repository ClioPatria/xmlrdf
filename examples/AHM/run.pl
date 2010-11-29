:- load_files(library(semweb/rdf_db), [silent(true)]).

:- rdf_register_ns(ahm,	   'http://purl.org/collections/ahm/').
:- rdf_register_ns(ulan,   'http://e-culture.multimedian.nl/ns/getty/ulan#').
:- rdf_register_ns(aatned, 'http://e-culture.multimedian.nl/ns/rkd/aatned/').

user:file_search_path(data, '../metadata/AHM').

:- load_files([ cliopatria(cliopatria),
		library(xmlrdf/xmlrdf),
		library(semweb/rdf_cache),
		library(semweb/rdf_library),
		library(semweb/rdf_turtle_write)
	      ], [silent(true)]).
:- use_module(rewrite).

load_ontologies :-
	rdf_attach_library(cliopatria(rdf)),
%	rdf_attach_library(getty(.)),
	rdf_load_library(dc),
	rdf_load_library(skos),
	rdf_load_library(rdfs),
	rdf_load_library(owl),
	absolute_file_name(data('rdf/ahm-schema.ttl'), Schema, [access(read)]),
	rdf_load(Schema).

:- initialization			% run *after* loading this file
	rdf_set_cache_options([ global_directory('cache/rdf'),
				create_global_directory(true)
			      ]),
	load_ontologies.

:- debug(xmlrdf).

load :-
	absolute_file_name(data(src), Dir,
			   [ file_type(directory)
			   ]),
	atom_concat(Dir, '/collection-*.xml', Pattern),
	expand_file_name(Pattern, Files),
	maplist(load, Files).

load_people_bob:-
        absolute_file_name(data('rdf/persons.ttl'), File,
			   [ access(read)
			   ]),
	rdf_load(File,[graph(peoplebob)]),
	absolute_file_name(data('rdf/ahm-people-schema.ttl'), FileSchema,
			   [ access(read)
			   ]),
	rdf_load(FileSchema,[graph(peoplebob_schema)]).

load_thesaurus:-
        absolute_file_name(data('rdf/thesaurus.ttl'), File,
			   [ access(read)
			   ]),
	rdf_load(File,[graph(thesaurus)]),
	absolute_file_name(data('rdf/ahm-voc-schema.ttl'), FileSchema,
			   [ access(read)
			   ]),
	rdf_load(FileSchema,[graph(thesaurus_schema)]).


load(File) :-
	rdf_current_ns(ahm, Prefix),
	load_xml_as_rdf(File,
			[ dialect(xml),
			  unit(record),
			  prefix(Prefix)
			]).

clean :-
	rdf_retractall(_,_,_,data),
	rdf_retractall(_,_,_,peoplebob),
	rdf_retractall(_,_,_,thesaurus).

sample :-
	absolute_file_name(data('src/collection-11001-12001.xml'), File,
			   [ access(read)
			   ]),
	load(File).


sample2 :-
	absolute_file_name(data('src/collection-11001-12001.xml'), File1,
			   [ access(read)
			   ]),
	absolute_file_name(data('src/collection-21001-22001.xml'), File2,
			   [ access(read)
			   ]),
	absolute_file_name(data('src/collection-41001-42001.xml'), File3,
			   [ access(read)
			   ]),
	absolute_file_name(data('src/collection-61001-62001.xml'), File4,
			   [ access(read)
			   ]),

	load(File1),load(File2),load(File3),load(File4).


save :-
	absolute_file_name(data('rdf/ahm.ttl'), File,
			   [ access(write)
			   ]),
	rdf_save_turtle(File, [graph(data)]).

run :-
	load,
	load_people_bob,
	load_thesaurus,
	rewrite,
	save.

