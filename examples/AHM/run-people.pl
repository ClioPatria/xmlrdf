:- module(ahm_convert_people,
	  [ ahm_run_people/0
	  ]).

% Setup a search path for finding the data.  Of course this can also
% use relative or absolute paths, but this is a bit easier to addapt
% to the environment.  The search path 'metadata' is predefined as
% one of $RDF_METADATA_DIR or ~/RDF/metadata

user:file_search_path(data, metadata('AHM')).

% Get core libraries

:- use_module(library(semweb/rdf_db)).		% Core RDF
:- use_module(library(semweb/rdf_library)).	% Load RDF from library
:- use_module(library(xmlrdf/xmlrdf)).		% XML --> RDF conversion
:- use_module(library(semweb/rdf_turtle_write)).% Save results
:- use_module(library(semweb/rdf_cache)).	% Cache control
:- use_module(library(semweb/rdf_persistency)).	% Persistency control

% Configure the environment:
%
%  - Make prefixes (namespaces) known
%  - Enable caching (speedup reloading large RDF files)
%  - Disable persistency for our rewrite graph(s).

:- rdf_register_ns(ahm,	   'http://purl.org/collections/nl/am/').
:- rdf_set_cache_options([ global_directory('cache/rdf'),
			   create_global_directory(true)
			 ]).
:- rdf_persistency(people, false).

% Load our dataset specific rewrite rules

:- use_module(rewrite_people).

%%	load_ontologies
%
%	Loads ontologies that we need.  The set below comes from the
%	ClioPatria library.

load_ontologies :-
	rdf_load_library(dc),
	rdf_load_library(skos),
	rdf_load_library(rdfs),
	rdf_load_library(owl).

% We always need the ontologies, and thus we load them with the program

:- initialization
	load_ontologies.

%%	load_people
%
%	Load the AM People database  from   the  XML  source file. First
%	locate the file using  the  Prolog   file  search  and  then use
%	load_xml/1.

load_people :-
        absolute_file_name(data('src/people.xml'), File,
			   [ access(read)
			   ]),
	load_xml(File).

%%	load_xml(+File)
%
%	Load the XML.  Relevant options:
%
%	  - The file is a plain XML file (not using XML namespaces)
%	  - Database entries are mapped to the XML element =record=.
%	    we process the file record-by-record and ignore all data
%	    outside the record elements (e.g., header)
%	  - Set the RDF prefix to the =ahm= prefix.
%	  - Dump all data into the graph =people=
%
%	After running this, we have `raw RDF'.

load_xml(File) :-
	rdf_current_ns(ahm, Prefix),
	load_xml_as_rdf(File,
			[ dialect(xml),
			  unit(record),
			  prefix(Prefix),
			  graph(people)
			]).

%%	save_thesaurus
%
%	Save the result relative to the =data= search path.

save_thesaurus :-
	absolute_file_name(data('rdf/am-people.ttl'), File,
			   [ access(write)
			   ]),
	rdf_save_turtle(File, [graph(people)]).

%%	ahm_run_people
%
%	Perform the entire conversion:
%
%	  1. Load the data.
%	  2. Rewrite the RDF graph
%	  3. Add some RDF statements.  Note thae we can also load this
%	     from an RDF file.
%	  4. Save the result.

ahm_run_people:-
	load_people,
	rewrite,
	rdf_assert(ahm:'AM_PeopleScheme',
		   rdf:type,   skos:'ConceptScheme', people),
	rdf_assert(ahm:'AM_PeopleScheme',
		   rdfs:label, literal('AM People thesaurus'), people),
	save_thesaurus.
