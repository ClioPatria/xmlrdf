:- module(rdf_rewrite,
	  [ op(1200, xfx, (@@)),	% Name @@ Rule
	    op(1180, xfx, ==>),		% Head ==> Body
	    op(1180, xfx, <=>),		% Head <=> Body
	    op(1100, xfx, \),		% Head \ Del <=> Body
	    op(200,  fx, ^),		% ^Predicate
	    op(700, xfx, ^^),		% Text^^Type
	    op(700, xfx, @),		% Text@Lang
	    op(200, xf, ?),		% Triple ?
	    op(200, xfx, >>),		% Triple >> Graph
					% Toplevel converter
	    rdf_rewrite_rules/0,
	    rdf_list_rule/1,		% +Name
	    rdf_rewrite/2,		% +Graph, Rule
	    rdf_rewrite/1,		% +Graph
					% Runtime support
	    subject_triple_sequence/3,	% +Pattern, -Data, +Graph
	    rdf_assert_new/4,		% +S,+P,+O,+Graph
	    rdf_retract_if_ground/4,	% +S,+P,+O,+Graph
	    rdf_assert_if_ground/4,	% +S,+P,+O,+Graph
	    rdf_set_lang/3,		% +LitIn, +Lang, -LitOut
	    rdf_set_type/3,		% +LitIn, +Type, -LitOut
					% Re-exporting
	    rdf_rename/3		% +Old,+New,?Graph
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(uri)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(rdf_rename).


/** <module> A generic RDF rewrite engine

Triple notation:

	{Subject, Predicate, Object}

Object is one of:

	* URI, written as ns:local or 'full URI'
	* "literal"
	* "literal"^^URI
	* "literal"@lang
	* Variable

Rename URIs:  {X} is a shorthand for {X,P,O},{S,X,O},{S,P,X}

{S, ^vra:idNumber, ID} \ {S} <=>
	a vra:'Work',
	make_identifier(ID, URI),
	{URI}.

Map into

rdf_mapping_rule(Id, Name, Graph, Actions, Options) :-
	Code

Where Actions is a conjunction of the following statements:

    * rdf_retractall(S,P,O,Graph)
    * rdf_retract_if_ground(S,P,O,Graph)
    * rdf_assert_new(S,P,O,Graph)
    * rdf_assert_if_ground(S,P,O,Graph)
*/

		 /*******************************
		 *	   THE REWRITER		*
		 *******************************/

:- meta_predicate
	rdf_rewrite(:),
	rdf_rewrite(:, +).

rdf_rewrite(Graph) :-
	rdf_rewrite(Graph, _).

rdf_rewrite(Module:Graph, Rule) :-
	rdf_generation(G0),
	rewrite_step(Module, Graph, Rule),
	rdf_generation(G1),
	debug(rdf_rewrite, 'Rewrite: generation ~D --> ~D',
	      [G0,G1]),
	G0 \== G1, !.

rewrite_step(Module, Graph, Rule) :-
	(   mapping_rules(Module, Rules),
	    \+ \+ member(Rule-_, Rules)
	->  true
	;   existence_error(rule, Rule)
	),
	(   member(Rule-Pairs, Rules),
	    nth1(I, Pairs, Id-Options),
	    debug(rdf_rewrite, 'Applying ... ~q (~d)', [Rule, I]),
	    rdf_generation(G0),
	    rdf_statistics(triples(TC0)),
	    statistics(cputime, T0),
	    (	option(transaction(true), Options, true)
	    ->	rdf_transaction(
		    call_rewrite_rule(Id, Module, Graph, Options),
		    Rule)
	    ;	call_rewrite_rule(Id, Module, Graph, Options)
	    ),
	    statistics(cputime, T1),
	    rdf_statistics(triples(TC1)),
	    rdf_generation(G1),
	    T is T1 - T0,
	    GDiff is G1-G0,
	    (	GDiff == 0
	    ->	debug(rdf_rewrite, '~3f seconds; no change', [T])
	    ;   debug(rdf_rewrite, '~3f seconds; ~D changes; ~D --> ~D triples',
		      [T, GDiff, TC0, TC1])
	    ),
	    fail
	;   true
	).

%%	call_rewrite_rule(+RuleID, +Module, +Graph, +Options)
%
%	If debug(profile(rule(RuleID))) is enabled, the execution of the
%	rule is profiled.

call_rewrite_rule(Rule, Module, Graph, Options) :-
	debugging(profile(rule(Rule))),
	profile(call_rewrite_rule2(Rule, Module, Graph, Options)).
call_rewrite_rule(Rule, Module, Graph, Options) :-
	call_rewrite_rule2(Rule, Module, Graph, Options).


call_rewrite_rule2(Rule, Module, Graph, Options) :-
	bnode_terms(Options, BNodes, BNodeOptions, _RestOptions),
	BNodes \== [], !,
	Template =.. [v,Actions|BNodes],
	findall(Template,
		Module:rdf_mapping_rule(Rule, _Name, Graph, Actions, Options),
		Bag),
	create_bnodes(BNodeOptions, 2, Bag, Graph),
	call_actions(Bag).
call_rewrite_rule2(Rule, Module, Graph, Options) :-
	findall(Actions,
		Module:rdf_mapping_rule(Rule, _Name, Graph, Actions, Options),
		Goals),
	maplist(call, Goals).

%%	rdf_rewrite_rules
%
%	List available rules

rdf_rewrite_rules :-
	format('Defined RDF mapping rules:~n~n', []),
	(   mapping_rules(_, Rules),
	    forall(append(Seen, [Rule-Ids|_], Rules),
		   list_rule(Rule, Ids, Seen)),
	    fail
	;   true
	),
	format('~n', []).

list_rule(Rule, [_Id], Seen) :-
	memberchk(Rule-_, Seen), !,
	format('\t~q ~t~40|(DISCONTIGUOUS)~n', [Rule]).
list_rule(Rule, [_Id], _) :- !,
	format('\t~q~n', [Rule]).
list_rule(Rule, Ids, Seen) :-
	memberchk(Rule-_, Seen), !,
	length(Ids, Len),
	format('\t~q ~t~40|(~d rules, DISCONTIGUOUS)~n', [Rule, Len]).
list_rule(Rule, Ids, _) :-
	length(Ids, Len),
	format('\t~q ~t~40|(~d rules)~n', [Rule, Len]).


%%	mapping_rules(?Module, -Rules) is nondet.
%
%	@param Rules is a list Name-IdOptionPairs

mapping_rules(Module, Rules) :-
	current_module(Module),
	current_predicate(Module:rdf_mapping_rule/5),
	findall(Name-(Id-Options),
		clause(Module:rdf_mapping_rule(Id, Name, _, _, Options), _),
		Pairs),
	group_pairs_by_key(Pairs, Rules).


%%	rdf_list_rule(+Name) is det.
%
%	Produce a listing of the generated Prolog for the named rule.

:- meta_predicate
	rdf_list_rule(:).

rdf_list_rule(M:Name) :-
	(   (   M == user
	    ;	M == rdf_rewrite
	    )
	->  true
	;   Module = M
	),
	(   current_module(Module),
	    current_predicate(Module:rdf_mapping_rule/5),
	    Head = Module:rdf_mapping_rule(_, Name, _, _, _),
	    forall(clause(Head, Body),
		   portray_clause((Head :- Body))),
	    fail
	;   true
	).


		 /*******************************
		 *	    BNODE MAGIC		*
		 *******************************/

%%	bnode_terms(+RuleOptions, -BNodeTemplates, -BNodeOptions, -RestOpts)
%
%	Split the option-list

bnode_terms([], [], [], []).
bnode_terms([bnode(BN, Props, Options)|T0],
	    [bnode(BN, Props)|BNT],
	    [Options|OT],
	    Rest) :-
	bnode_terms(T0, BNT, OT, Rest).
bnode_terms([H|T0], BN, O, [H|T]) :-
	bnode_terms(T0, BN, O, T).


%%	create_bnodes(+BNOptions, +Index, +Bag, +Graph)
%

create_bnodes([], _, _, _).
create_bnodes([BNOptions|OT], I, Bag, Graph) :-
	create_bnodes_arg(Bag, I, BNOptions, Graph),
	I2 is I + 1,
	create_bnodes(OT, I2, Bag, Graph).

%%	create_bnodes(+BNTerms, +Options, +Graph)
%
%	Share blank nodes.
%
%	@param BNTerms is a list bnode(Id, Properties)
%	@param Options describes the sharing.  Currently supports
%
%		* equal
%		All properties must be equal
%		* equal(ListOfProperties)
%		Only the indicated properties must be equal

create_bnodes_arg(BNTerms, I, Options, Graph) :-
	option(share_if(Share), Options, equal),
	key_bnodes(BNTerms, Share, I, KTerms),
	group_pairs_by_key(KTerms, Grouped),
	maplist(make_bnode(Graph), Grouped).

make_bnode(Graph, _Key-[bnode(BN, P0)|BNodes]) :-
	merge_bnodes(BNodes, BN, P0, Properties),
	rdf_bnode(BN),
	forall(member(P=O, Properties),
	       rdf_assert_if_ground(BN, P, O, Graph)).

merge_bnodes([], _, PL, PL).
merge_bnodes([bnode(BN, P2)|T], BN, PL0, PL) :-
	union(PL0, P2, PL1),
	merge_bnodes(T, BN, PL1, PL).


key_bnodes([], _, _, []).
key_bnodes([Templ|T0], Share, I, [Keyed|T]) :-
	key_bnode(Share, I, Templ, Keyed),
	key_bnodes(T0, Share, I, T).


key_bnode(Equal, I, Template, Key-BNode) :- !,
	arg(I, Template, BNode),
	arg(2, BNode, Properties),
	(   Equal == equal
	->  sort(Properties, Key)
	;   Equal = equal(L)
	->  maplist(pvalues(Properties), L, Key)
	;   domain_error(share_if, Equal)
	).

pvalues([], _, []).
pvalues([P=V|T0], P, [V|T]) :-
	ground(V), !,
	pvalues(T0, P, T).
pvalues([_|T0], P, T) :-
	pvalues(T0, P, T).


%%	call_actions(+Templates)
%
%	Call the actions associated with  each template-instantiation of
%	the findall.

call_actions([]).
call_actions([Template|T]) :-
	arg(1, Template, Actions),
	Actions,
	call_actions(T).


		 /*******************************
		 *	  TERM-EXPANSION		*
		 *******************************/

%%	expand_rule(+Rule, -Clause) is det.
%
%	Expand the rule-language into  proper   Prolog  rules. Rules are
%	clauses for rdf_mapping_rule/4.

expand_rule(Name@@Rule, Clause) :-
	rule_id(Id),
	expand_rule(Rule, Name, Id, Clause).
expand_rule(Rule, Clause) :-
	rule_term(Rule), !,
	(   rule_id(Id),
	    expand_rule(Rule, Id, Id, Clause)
	->  true
	;   print_message(warning, illegal_rdf_rule)
	).
expand_rule(Term0, Term) :-
	expand_rdf(Term0, Term),
	Term0 \== Term.


expand_rule((Keep \ Delete <=> Body), Name, Id,
	    (rdf_mapping_rule(Id, Name, Graph, Actions, Options) :-
		Rule)) :- !,
	expand_body(Body, Guard, Add, Options0),
	actions(Graph, Delete, Add, Actions),
	(   Actions = rdf_rename(_,_,_),
	    Options0 == []
	->  Options = [transaction(false)]
	;   Options = Options0
	),
	rule_body(Graph, Keep, Delete, Guard, Rule).
expand_rule((Delete <=> Body), Name, Id,
	    (rdf_mapping_rule(Id, Name, Graph, Actions, Options) :-
		Rule)) :- !,
	expand_body(Body, Guard, Add, Options),
	actions(Graph, Delete, Add, Actions),
	rule_body(Graph, true, Delete, Guard, Rule).
expand_rule((Keep ==> Body), Name, Id,
	    (rdf_mapping_rule(Id, Name, Graph, Actions, Options) :-
		Rule)) :- !,
	expand_body(Body, Guard, Add, Options),
	actions(Graph, true, Add, Actions),
	rule_body(Graph, Keep, true, Guard, Rule).

rule_term(_<=>_).
rule_term(_==>_).

%%	rule_id(-Id)
%
%	Give  an  identifier  to  the  rule.    Currently   we  use  the
%	source-location. We probably need some way to name the rule, but
%	a good syntax is hard. (Name  @  Rule)   as  used  by CHR is not
%	possible because @ is already used for language-tagged literals.

rule_id(Id) :-
	source_location(File, Line),
	uri_file_name(URI, File),
	atomic_list_concat([URI, #, Line], Id).

%%	expand_body(+Body, -Guard, -Add, -Options) is det.
%
%	Split the body into two goal-lists; one describing the
%	guard and one adding data.

expand_body(Body, Guard, Add, Options) :-
	comma_list(Body, Members),
	partition(is_triple, Members, Add0, Guard0),
	phrase(expand_add(Add0, MoreGuard, Options), Add),
	append(Guard0, MoreGuard, Guard1),
	expand_rdf(Guard1, Guard).

is_triple(V) :-
	var(V), !, fail.
is_triple({}(_)).
is_triple({}(_)>>_).

%%	expand_add(+In, -Goal, -Options)//
%
%	Expand object-lists in triples to generate a blank-node

expand_add([], [], []) --> [].
expand_add([Triple|T0], [rdf_bnode(BN)|Goals], Options) -->
	{ triple(Triple, S,P,O,G),
	  nonvar(O),
	  O = bnode(Properties), !
	},
	g_triple(G,S,P,BN),
	bnode_triples(Properties, BN),
	expand_add(T0, Goals, Options).
expand_add([Triple|T0], Goals,
	   [bnode(BN, Properties, BNOptions)|Options]) -->
	{ triple(Triple, S,P,O,G),
	  nonvar(O),
	  O = bnode(Properties0, BNOptions0), !,
	  expand_rdf(Properties0, Properties),
	  expand_rdf(BNOptions0, BNOptions)
	},
	g_triple(G,S,P,BN),
	expand_add(T0, Goals, Options).
expand_add([Triple|T0], [Fix|Goals], Options) -->
	{ triple(Triple, S,P,O,G),
	  nonvar(O),
	  (   O = (Var@Lan)
	  ->  Fix = rdf_set_lang(Var,Lan,O2)
	  ;   O = (Var@Type)
	  ->  Fix = rdf_set_type(Var,Type,O2)
	  )
	}, !,
	g_triple(G,S,P,O2),
	expand_add(T0, Goals, Options).
expand_add([X|T0], Goals, Options) -->
	[X],
	expand_add(T0, Goals, Options).

g_triple(-, S,P,O) --> !,
	[ {S,P,O} ].
g_triple(G, S,P,O) -->
	[ {S,P,O} >> G ].

bnode_triples([], _) --> [].
bnode_triples([P=O|T], S) -->
	[ {S,P,O} ],
	bnode_triples(T, S).


%%	actions(+Graph, +Delete, +AddList, -Actions) is det.
%
%	Create an action-goal from the list of   RDF  objects to add and
%	delete.

actions(Graph, Delete, AddList, Actions) :-
	comma_list(Delete, DelList0),
	flatten(DelList0, DelList),	% Deal with sequences
	join_actions(Graph, DelList, AddList, ActionList),
	comma_list(Actions, ActionList).

join_actions(Graph, DelList, AddList, Actions) :-
	select(Del, DelList, RDel),
	single_resource(Del, R0),
	select(Add, AddList, RAdd),
	single_resource(Add, R1), !,
	no_more_single_updates(RDel),
	no_more_single_updates(RAdd),
	Actions = [rdf_rename(R0, R1, Graph)|RActions],
	join_actions(Graph, RDel, RAdd, RActions).
join_actions(Graph, DelList, AddList, Actions) :-
	delete_actions(DelList, Graph, CondVars, Actions, AddActions),
	add_actions(AddList, Graph, CondVars, AddActions, []).

single_resource({R}, R) :-
	var(R), !.
single_resource({R}, R) :-
	nonvar(R),
	R \= (_,_).

no_more_single_updates(List) :-
	member(X, List),
	single_resource(X, _), !,
	representation_error(multiple_single_resources).
no_more_single_updates(_).


delete_actions([], _, [], L, L).
delete_actions([X|_], _, _, _, _) :-
	var(X), !,
	instantiation_error(X).
delete_actions([Triple?|T0], Graph,
	       CondVars,
	       [rdf_retract_if_ground(S,P,O,Graph)|T], L) :- !,
	expanded_triple(Triple, S,P,O),
	term_variables(Triple, CondVars, CVTail),
	delete_actions(T0, Graph, CVTail, T, L).
delete_actions([Triple|T0], Graph, CondVars,
	       [rdf_retractall(S,P,O,Graph)|T], L) :-
	expanded_triple(Triple, S,P,O),
	delete_actions(T0, Graph, CondVars, T, L).


%%	add_actions(+Triples, +Graph, +CondVars, +Actions, ?ActionTail)
%
%	@tbd	conditional-variable computation in ==> rules is missing,
%		which is why disabled this and always use
%		rdf_assert_if_ground/4 for now.  This works fine, but
%		makes it harder to track bugs in rules.

add_actions([], _, _, L, L).
add_actions([Triple>>Graph|T0], Graph0, CV, Actions, L) :- !,
	add_actions([Triple], Graph, CV, Actions, Tail),
	add_actions(T0, Graph0, CV, Tail, L).
add_actions([Triple|T0], Graph, CV, [Action|T], L) :-
	Action = rdf_assert_if_ground(S,P,O, Graph),
	expanded_triple(Triple, S,P,O),
	add_actions(T0, Graph, CV, T, L).


%%	rule_body(+Graph, +Keep, +Delete, +Guard:list, -Rule)
%
%	Construct the actual body for our mapping rule.
%
%	@tbd:	Use the RDF query optimizer to finish the job.

rule_body(Graph, Keep, Delete, GuardList, Rule) :-
	comma_list(Guard, GuardList),
	make_goal((Keep, Delete, Guard), Graph, Rule0),
	expand_goal(Rule0, Rule).

make_goal(G, _, G) :-
	var(G), !.
make_goal((A0,B0), Graph, (A,B)) :- !,
	make_goal(A0, Graph, A),
	make_goal(B0, Graph, B).
make_goal((A0;B0), Graph, (A;B)) :- !,
	make_goal(A0, Graph, A),
	make_goal(B0, Graph, B).
make_goal((A0->B0), Graph, (A->B)) :- !,
	make_goal(A0, Graph, A),
	make_goal(B0, Graph, B).
make_goal(List, Graph, Goal) :-
	is_list(List), !,
	(   same_subject_triples(List, Subject, Pairs)
	->  Goal = subject_triple_sequence(Subject, Pairs, Graph)
	;   type_error(same_subject_triples, List)
	).
make_goal(X, Graph, Goal) :-
	expanded_triple(X, S,P,O), !,
	make_rdf_goal(S,P,O, Graph, Goal).
make_goal(T, _, true) :-
	single_resource(T, _), !.
make_goal(X?, Graph, (G*->true;true)) :- !,
	make_goal(X, Graph, G).
make_goal(G, _, G).

make_rdf_goal(S,SP,O, _, Goal) :-
	nonvar(SP),
	SP = ^P, !,
	Goal = rdf_has(S, P, O).
make_rdf_goal(S,P,O, _, Goal) :-
	Goal = rdf(S, P, O).


%%	same_subject_triples(+List, -Subject, -PredObjPairs) is semidet.
%
%	Matches [{S,P,O}, {S,P2,O2}, ...]

same_subject_triples([H|T0], S, [P-O|T]) :-
	expanded_triple(H, S,P,O),
	same_subject_triples_2(T0, S, T).

same_subject_triples_2([], _, []).
same_subject_triples_2([H|T0], S, [P-O|T]) :-
	expanded_triple(H, S1,P,O),
	S1 == S,
	same_subject_triples_2(T0, S, T).


%%	expanded_triple(+Term, -S,-P,-O) is semidet.
%
%	As triple/4, expanding the 3 arguments.

expanded_triple(Triple, S,P,O) :-
	triple(Triple, S0, P0, O0),
	expand_resource(S0, S),
	expand_predicate(P0, P),
	expand_object(O0, O).

expand_predicate(P, P) :-
	var(P), !.
expand_predicate(^P0, ^P) :-
	expand_resource(P0, P).
expand_predicate(P0, P) :-
	expand_resource(P0, P).


%%	triple(+Term, -S,-P,-O) is semidet.
%
%	True if Term is of the form   {S,P,O}. Note that all {...} terms
%	are mapped to the canonical Prolog term   {}(Arg), so we must be
%	careful  when  matching.  In   particular,    {_}   =  {_,_,_}!.
%	Alternative,  we  could  use  subsumbes/2  to  do  the  checking
%	properly.

triple({}(X), S,P,O) :-
	nonvar(X),
	X = (S,X2),
	nonvar(X2),
	X2 = (P,O).

triple(T>>G, S,P,O,G) :- !,
	triple(T,S,P,O).
triple(T, S,P,O,-) :- !,
	triple(T,S,P,O).


%%	expand_resource(+In, -Out) is det.

expand_resource(X, X) :-
	var(X), !.
expand_resource(X, X) :-
	atom(X), !.
expand_resource(NS:Local, Global) :-
	must_be(atom, NS),
	must_be(atom, Local),
	(   rdf_current_ns(NS, Full)
	->  atom_concat(Full, Local, Global)
	;   existence_error(namespace, NS)
	).

expand_object(O, O) :-
	var(O), !.
expand_object(literal(X), literal(X)) :- !.
expand_object("", literal('')) :- !.
expand_object(O, O) :-
	atom(O), !.
expand_object(NS:Local, O) :- !,
	expand_resource(NS:Local, O).
expand_object(String^^R, literal(type(Type, Text))) :- !,
	to_literal_text(String, Text),
	expand_resource(R, Type).
expand_object(String@Lang, literal(lang(Lang, Text))) :- !,
	to_literal_text(String, Text).
expand_object(String, literal(Text)) :-
	to_literal_text(String, Text).

to_literal_text(Var, Var) :-
	var(Var), !.
to_literal_text(String, Text) :-
	string(String), !,
	atom_concat(String, '', Text).
to_literal_text(String, Text) :-
	atom_codes(Text, String).

%%	comma_list(+Conjunction, -List) is det.
%%	comma_list(-Conjunction, +List) is det.
%
%	Translate between a Prolog  conjunction   and  a  list. Elements
%	=true= are removes from both  translations.   The  empty list is
%	mapped to a single =true=.

comma_list(Conj, List) :-
	is_list(List),
	list_comma(List, Conj).
comma_list(Conj, List) :-
	phrase(comma_list(Conj), List).

comma_list(A)     --> {var(A)}, !, [A].
comma_list((A,B)) --> !, comma_list(A), comma_list(B).
comma_list(true)  --> !, [].
comma_list(A)     --> [A].

list_comma([], true).
list_comma([H|T], C) :-
	(   T == []
	->  C = H
	;   H == true
	->  list_comma(T, C)
	;   C = (H,B),
	    list_comma(T, B)
	).


		 /*******************************
		 *	 GENERAL CLAUSES	*
		 *******************************/

%%	expand_rdf(+Term0, -Term) is det.
%
%	Expand our symbolic representation to RDF structures anywhere in
%	the code. This is somewhat dubious  because mapping "..." into a
%	literal can be ambiguous and such   is  mapping ns:local for RDF
%	namespaces.
%
%	@tbd Should we introduce `Term to quote terms?

expand_rdf(Term0, Term) :-
	compound(Term0), !,
	(   expand_to_literal(Term0, Term)
	->  true
	;   Term0 = NS:Local,
	    atom(NS), atom(Local)
	->  expand_resource(NS:Local, Term)
	;   Term0 =.. [F|Args0],
	    maplist(expand_rdf, Args0, Args),
	    Term =.. [F|Args]
	).
expand_rdf(Term, Term).

expand_to_literal(Text^^Type, literal(type(URI, Value))) :- !,
	to_literal_text(Text, Value),
	expand_resource(Type, URI).
expand_to_literal(Text@Lang, literal(lang(Lang, Value))) :- !,
	to_literal_text(Text, Value).
expand_to_literal(Text, literal(Value)) :-
	is_list(Text),
	maplist(sensible_char, Text),
	atom_codes(Value, Text).

sensible_char(C) :-
	integer(C),
	sensible_char_2(C).

sensible_char_2(C) :-
	between(32, 127, C), !.
sensible_char_2(0'\t).
sensible_char_2(0'\n).


		 /*******************************
		 *	      RUNTIME		*
		 *******************************/

%%	rdf_assert_new(+S,+P,+O,+Graph) is det.

rdf_assert_new(S,P,O,Graph) :-
	(   O = literal(L),
	    atom(L)
	->  rdf(S,P,literal(plain(L),L),Graph)
	;   rdf(S,P,O,Graph)
	), !.
rdf_assert_new(S,P,O,Graph) :-
	rdf_assert(S,P,O,Graph).

%%	rdf_assert_if_ground(+S,+P,+O,+Graph) is det.
%
%	Assert if the goal is instantiated.  Used to deal with
%	optional assertions

rdf_assert_if_ground(S,P,O,Graph) :-
	nonvar(S), nonvar(P), nonvar(O), !,
	rdf_assert_new(S,P,O,Graph).
rdf_assert_if_ground(_,_,_,_).

%%	rdf_retract_if_ground(+S,+P,+O,+Graph) is det.
%
%	Retract if the goal is instantiated.  Used to deal with
%	optional retraction

rdf_retract_if_ground(S,P,O,Graph) :-
	nonvar(S), nonvar(P), nonvar(O), !,
	(   O = literal(L),
	    atom(L)
	->  rdf_retractall(S,P,literal(plain(L),L),Graph)
	;   rdf_retractall(S,P,O,Graph)
	).
rdf_retract_if_ground(_,_,_,_).


%%	subject_triple_sequence(?S, +Pattern, ?Graph) is nondet.
%
%	True if S has the P-O pairs from Pattern in the same order as in
%	Pattern.

subject_triple_sequence(S, Pattern, Graph) :-
	best_guard(Pattern, S, Guard),
	findall(S, Guard, SList),
	sort(SList, SSet),
	assertion(maplist(atom, SSet)),
	member(S, SSet),
	findall(P-O, rdf(S, P, O, Graph), Data),
	sequence_in(Pattern, Data).

best_guard([], S, rdf_subject(S)).
best_guard([P-O|T], S, Guard) :-
	copy_term(P-O, P1-O1),
	estimate(rdf(S,P1,O1), C0),
	best_guard(T, S, C0, rdf(S,P1,O1), Guard).

best_guard([], _, _, G, G).
best_guard([P-O|T], S, C0, G0, G) :-
	copy_term(P-O, P1-O1),
	estimate(rdf(S,P1,O1), C1),
	(   C1 < C0
	->  best_guard(T, S, C1, rdf(S,P1,O1), G)
	;   best_guard(T, S, C0, G0, G)
	).

estimate(rdf(_,P,O), C) :-
	atom(P), var(O), !,
	rdf_predicate_property(P, triples(C)).
estimate(rdf(S,P,O), C) :-
	rdf_estimate_complexity(S,P,O,C).




%%	sequence_in(+ListPattern, -Data) is nondet.
%
%	True if ListPattern appears in Data.   There are two patterns of
%	interest to us, for which we  give   an  example  if the pattern
%	appears three times:
%
%		a,b,a,b,a,b
%		a,a,a,b,b,b
%
%	In both cases, the pattern [a,b]  must   match  3 times. To deal
%	with this, we first try to match the 2nd type.
%
%	@param ListPattern is a list of Pred-Object pairs.

sequence_in(Pattern, Data) :-
	maplist(indices(Data), Pattern, Places),
	maplist(arity, Places, [Len|Lens]),
	all_same(Lens, Len),
	Array =.. [d|Data],
	between(1, Len, I),
	data(Pattern, I, Places, Array).

data([], _, _, _).
data([H|T], I, [P|PT], Data) :-
	arg(I, P, Place),
	arg(Place, Data, H),
	data(T, I, PT, Data).

indices(Data, H, Indices) :-
	findall(I, nth1(I, Data, H), IndexList),
	Indices =.. [i|IndexList].

arity(Term, Arity) :-
	functor(Term, _, Arity).

all_same([], _).
all_same([H|T], H) :-
	all_same(T, H).


%%	rdf_set_lang(+O0, +Lang, -O)
%
%	Set/change the language of a literal

rdf_set_lang(Lit, Lang, literal(lang(Lang, Text))) :-
	text_of_literal(Lit, Text).

%%	rdf_set_type(+O0, +Lang, -O)
%
%	Set/change the type of a literal

rdf_set_type(Lit, Type, literal(type(Type, Text))) :-
	text_of_literal(Lit, Text).

text_of_literal(Var, _) :-
	var(Var), !,
	fail.
text_of_literal(literal(Lit), Text) :- !,
	text_of_literal(Lit, Text).
text_of_literal(lang(_, Text), Text) :- !.
text_of_literal(type(_, Text), Text) :- !.
text_of_literal(Text, Text).


		 /*******************************
		 *	      EXPANSION		*
		 *******************************/

user:term_expansion(In, Out) :-
	prolog_load_context(module, Module),
	predicate_property(Module:rdf_rewrite(_),
			   imported_from(rdf_rewrite)),
	expand_rule(In, Out).


		 /*******************************
		 *	 PCE EMACS SUPPORT	*
		 *******************************/

:- multifile
	prolog_colour:term_colours/2,
	prolog_colour:goal_colours/2,
	prolog_colour:style/2,
	prolog_colour:identify/2,
	prolog:called_by/2.

term_colours((_Name@@Rule),
	     expanded - [ identifier, RuleColours ]) :-
	term_colours(Rule, RuleColours).
term_colours((Head <=> Body),
	     expanded - [ HeadColours, BodyColours ]) :-
	head_colours(Head, HeadColours),
	body_colours(Body, BodyColours).
term_colours((Head ==> Body),
	     expanded - [ HeadColours, BodyColours ]) :-
	head_colours(Head, keep, HeadColours),
	body_colours(Body, BodyColours).

head_colours((Keep \ Del),
	     expanded - [ KeepColours, DelColours ]) :- !,
	head_colours(Keep, keep, KeepColours),
	head_colours(Del, del, DelColours).
head_colours(Del, Colours) :-
	head_colours(Del, del, Colours).

%%	head_colours(+BodyTerm, +KeepDel, -Colours) is det.

head_colours(Var, _, classify) :-
	var(Var).
head_colours((A,B), Keep, control - [AC, BC]) :- !,
	head_colours(A, Keep, AC),
	head_colours(B, Keep, BC).
head_colours(X?, Keep, optional - [Colours ]) :- !,
	head_colours(X, Keep, Colours).
head_colours(List, Keep, sequence - Colours) :-
	is_list(List), !,
	head_sequence(List, Keep, Colours).
head_colours({_}, keep, keep_triple - [classify ]) :- !.
head_colours({_}, del, del_triple - [classify ]) :- !.
head_colours(_, _, classify).

head_sequence([], _, []).
head_sequence([H|T0], Keep, [C|T]) :-
	head_colours(H, Keep, C),
	head_sequence(T0, Keep, T).

%%	body_colours(+BodyTerm, -Colours) is det.

body_colours(Var, classify) :-
	var(Var), !.
body_colours((A,B), control - [AC, BC]) :- !,
	body_colours(A, AC),
	body_colours(B, BC).
body_colours(X>>_, redirect - [Colours, graph]) :- !,
	body_colours(X, Colours).
body_colours({_}, add_triple - [classify ]) :- !.
body_colours(_, body).

prolog_colour:term_colours(Term, Colours) :-
	term_colours(Term, Colours).

prolog_colour:style(add_triple, [background('#a2ffa1')]).
prolog_colour:style(del_triple, [background('#ffb3b3')]).
prolog_colour:style(optional,   [bold(true)]).
prolog_colour:style(sequence,   [bold(true)]).
prolog_colour:style(graph,      [bold(true)]).
