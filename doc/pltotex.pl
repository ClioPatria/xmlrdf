#!/home/janw/bin/swipl -q -g pltotex,halt -t halt(1) -s

:- module(pltotex,
	  [ pltotex/2,
	    pltotex/0
	  ]).
:- use_module(library(doc_latex)).
:- use_module(library(main)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(lists)).

pltotex(File, Options) :-
	text_file(File), !,
	tex_file(File, Out, Options),
	merge_options(Options, [stand_alone(false)], DocOptions),
	doc_latex(File, Out, DocOptions).
pltotex(Lib, Options) :-
	pl_file(Lib, File),
	tex_file(File, Out, Options),
	user:use_module(File),		% we want the operators in user
	merge_options(Options, stand_alone(false), DocOptions),
	doc_latex(File, Out, DocOptions).

text_file(File) :-
	file_name_extension(_, txt, File), !.
text_file(File) :-
	file_base_name(File, Base),
	text_file_name(Base).

pl_file(Text, File) :-
	(   file_name_extension(_, pl, Text)
	->  Spec = Text
	;   atom_to_term(Text, Spec, _)
	),
	absolute_file_name(Spec, File,
			   [ access(read),
			     file_type(prolog)
			   ]).

text_file_name('README').
text_file_name('TODO').

tex_file(_, TeXFile, Options) :-
	option(out(Base), Options), !,
	file_name_extension(Base, tex, TeXFile).
tex_file(File, TeXFile, _) :-
	file_base_name(File, Local),
	file_name_extension(Base0, _, Local),
	strip(Base0, 0'_, Base),
	file_name_extension(Base, tex, TeXFile).

strip(In, Code, Out) :-
	atom_codes(In, Codes0),
	delete(Codes0, Code, Codes),
	atom_codes(Out, Codes).


%%	pltotex
%
%	Usage: pl -q -s pltotex.pl -g pltotex -- [option ...] file ...

pltotex :-
	current_prolog_flag(argv, Argv),
	append(_, [--|More], Argv), !,
	pltotex(More).

pltotex(Argv) :-
	partition(is_option, Argv, OptArgs, Files),
	maplist(to_option, OptArgs, Options),
	(   option(text(true), Options)
	->  convert_text_file(Files, Options)
	;   maplist(process_file(Options), Files)
	).

usage :-
	format(user_error,
	       'Usage: pltotex option ,,, file ...', []),
	halt(1).

is_option(Arg) :-
	sub_atom(Arg, 0, _, _, --).

to_option('--section', section_level(section)) :- !.
to_option('--subsection', section_level(subsection)) :- !.
to_option('--subsubsection', section_level(subsubsection)) :- !.
to_option(Arg, Option) :-
	atom_concat(--, Opt, Arg),
	sub_atom(Opt, B, _, A, =), !,
	sub_atom(Opt, 0, B, _, Name),
	sub_atom(Opt, _, A, 0, Value),
	Option =.. [Name, Value].
to_option(Arg, Option) :-
	atom_concat(--, Opt, Arg),
	Option =.. [Opt, true].

process_file(Options, File) :-
	pltotex(File, Options).

convert_text_file(Files, Options) :-
	partition(text_file, Files, TextFiles, PlFiles),
	(   TextFiles = [TextFile]
	->  true
	;   usage
	),
	maplist(load_pl_file, PlFiles),
	pltotex(TextFile, Options).

load_pl_file(Spec) :-
	pl_file(Spec, File),
	load_files(user:File, []).
