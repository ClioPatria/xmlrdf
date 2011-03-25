:- module(ahm_convert_data,
	  [ run_metadata/0
	  ]).

user:file_search_path(data,       metadata('AHM')).

:- load_files(library(semweb/rdf_db), [silent(true)]).

:- rdf_register_ns(ahm,	   'http://purl.org/collections/nl/am/').

:- load_files([ library(xmlrdf/xmlrdf),
		library(semweb/rdf_cache),
		library(semweb/rdf_library),
		library(semweb/rdf_turtle_write)
	      ], [silent(true)]).
:- use_module(rewrite_data).

load_ontologies :-
	rdf_load_library(dc),
	rdf_load_library(skos),
	rdf_load_library(rdfs),
	rdf_load_library(owl),

	rdf_load(data('rdf/am-schema.ttl'),[graph(am_schema)]),
	rdf_load(data('rdf/am-people-rdagr2-schema.ttl'),[graph(am_rda_schema)]),
	rdf_load(data('rdf/am-thesaurus-schema.ttl'),[graph(am_thesaurus_schema)]),
	rdf_load(data('rdf/ElementsGr2.rdf'),[graph(arda_elementsGr2)]).


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
        absolute_file_name(data('rdf/am-people.ttl'), File,
			   [ access(read)]),
	rdf_load(File,[graph(peoplebob)]).

load_thesaurus:-
        absolute_file_name(data('rdf/am-thesaurus.ttl'), File,
			   [ access(read)]),
	rdf_load(File,[graph(thesaurus)]).


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
	absolute_file_name(data('rdf/am-data.ttl'), File,
			   [ access(write)
			   ]),
	rdf_save_turtle(File, [graph(data)]).

run_metadata :-
	load,
	load_people_bob,
	load_thesaurus,
	rewrite,
	save.

runsample2:-
        sample2,
	load_people_bob,
	load_thesaurus,
	rewrite.
