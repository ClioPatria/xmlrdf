:- module(conf_xmlrdf, []).

/** <module> XML to RDF conversion
*/

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(metadata, Dir) :-
	getenv('RDF_METADATA_DIR', Dir).
user:file_search_path(metadata, Dir) :-
	expand_file_name('~/RDF/metadata', [Dir]).
