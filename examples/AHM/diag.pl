num_creators(R, N) :-
	bagof(C, rdf(R, ahm:creator, C), Cs),
	length(Cs, N).
num_date_of_birth(R, N) :-
	bagof(C, rdf(R, ahm:creatorDateOfBirth, C), Cs),
	length(Cs, N).

bad(R) :-
	num_creators(R, N1),
	num_date_of_birth(R, N2),
	N1 \== N2.
