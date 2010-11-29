---+ Introduction

Many datasets are transferred as XML, providing a tree-based datamodel
that is purely syntactic in nature. Semantic processing is standardised
around RDF, which provides a graph-based model. In the transformation
process we must identify syntactic artifacts such as meaningless
ordering in the XML data, lacking structure (e.g., the _creator_ of an
artwork is not a literal string by a person identified by a resource
with properties) and overly structured data (e.g. the dimension of an
object is a property of the object, not of some placeholder that
combines physical properties of the object). These syntactic artifacts
must be translated into a proper semantic model where objects and
properties are typed and semantically related to common vocabularies
such as SKOS and Dublin Core.

This document describes our toolkit for supporting this transformation
process, together with examples taken from actual translations. The
toolkit is implemented in SWI-Prolog and can be downloaded using GIT
from one of the addresses below. Running the toolkit requires
SWI-Prolog, which can be downloaded from http://www.swi-prolog.org
for Windows, MacOS and Linux or in source for many other platforms.

    * git://eculture.cs.vu.nl/home/git/econnect/xmlrdf.git
    * http://eculture.cs.vu.nl/git/econnect/xmlrdf.git

The graph-rewrite engine is written in Prolog.  This document does not
assume any knowledge about Prolog. The rule-language is, as far as
possible, a clean declarative graph-rewrite language. The transformation
process for actual data however can be complicated.  For these cases the
rule-system allow mixing rules with arbitrary Prolog code, providing an
unconstrained transformation system. We provide a (dynamically extended)
library of Prolog routines for typical conversion tasks.


---+ Converting XML into RDF

The core idea behind  converting  `data-xml'   into  RDF  is  that every
complex XML element maps to a resource  (often a bnode) and every atomic
attribute maps to an attribute of this bnode. Such a translation gives a
valid RDF document, which is much easier to access for further
processing.

There are a few places where we must be more subtle in the initial
conversion.  First, the XML reserved attributes:

    * The xml:lang attribute is kept around and if we create an RDF
    literal, it is used to create a literal in the current language.

    * xmlns declarations are ignored (they make the declarations
    available to the application, but the namespaces are already
    processed by the XML parser).

Second, we may wish to map some   of our properties into rdfs:XMLLiteral
or RDF dataTypes. In particular the first   _must_  be done in the first
pass to avoid all the complexities  of   turning  the  RDF back into XML
(think of the above mentioned   declarations,  but ordering requirements
can make this fundamentally impossible).

Because this step needs type information   about properties, we might as
well allow for some very  simple   transformations  in  the first phase.
These  transformations  are  guided  by  the   target  RDF  schema.  The
transformation process can add additional properties   to the target RDF
properties and RDF classes. The property   is  called map:xmlname, where
the =map= prefix is currently  defined as http://cs.vu.nl/eculture/map/.
If this property is associated  to  a   class,  an  XML element with the
defined name is translated into an  instance   of  this  class. If it is
associated to a property, it  affects   XML  attribute or atomic element
translation in two ways:

    * It uses the RDF property name rather than the XML name for
    the property

    * The rdfs:range of the property affects the value translation:

	* If it is rdfs:XMLLiteral, the sub-element is translated
	to an RDF XMLLiteral.

	* If it is an XSD datatype, the sub-element is translated
	into a typed RDF literal

	* It it is a proper class and the value is a valid URI, the
	URI is used as value without translation into a literal.

Below is an example  that  maps   XML  elements  =record=  into vra:Work
instances  and  maps  the  XML  attribute  =title=  into  the  vra:title
property. Note that it is not required   (and  not desirable) to add the
=|map:xmlname|= properties to the actual schema files. Instead, put them
in a separate file and load both into the conversion engine.

    ==
    @prefix   vra: <http://www.vraweb.org/vracore/vracore3#> .
    @prefix   map: <http://cs.vu.nl/eculture/map/> .

    # Map element-names to rdf:type

    vra:Work map:xmlname "record" .

    # Map xml attribute and sub-element names to properties

    vra:title map:xmlname "title" .
    ==

---+ Default XML name mapping

The initial XML to RDF mapper uses the XML attribute and tag-names for
creating RDF properties.  It provides two optional processing steps that
make identifiers fit better with the RDF practice.

    1. It can add a _prefix_ to each XML name to create a fully
    qualified URI.  E.g.,

	==
	?- rdf_current_ns(ahm, Prefix),
	   load_xml_as_rdf('data.xml',
			   [ prefix(Prefix)
			   ]).
	==

    2. It `restyles' XML identifiers. Notably identifiers that contain a
    dot (.) are hard to process using Turtle.  The library identifies
    alphanumerical substrings of the XML name and constructs new
    identifiers from these parts.  By default, predicates start with
    a lowercase letter and each new part starts with an uppercase
    letter, as in =oneTwo=.  Types (classes) start with an uppercase
    letter, as in =OneTwo=.  This behaviour can be controlled with the
    options =predicate_style= and =class_style= of load_xml_as_rdf/2.


---+ Subsequent meta-data mapping

Further mapping of meta-data consists of the following steps:

    1. Fix the node-structure.
    2. Re-establish internal links.
    3. Re-establish external links.
    4. Create a mapping schema that link the classes and predicates
       of the source to the target schema (e.g., Dublin Core).
    5. Assign URIs to blank nodes where applicable.

---++ Fix the node-structure

Source-data generally uses a record structure. Sometimes, each record is
a simple flat list of properties, while in other cases it has a deeply
nested structure.  We distinguish three types of properties:

    1. Properties with a clear single literal value, such as a
    collection-identifier.  Such properties are directly mapped to
    RDF literals.

    2. Properties with instance-specific scope that may have
    annotations. Typical examples are the title (multiple, translations,
    who has given the work a title, etc.) or a dimension (unit, which
    dimension, etc.).  In this case, we create a new RDF node for each
    instance.

    3. Properties that link to external resources: persons (creator),
    material (linking to a controlled vocabulary), etc.  In this case
    the mapper unites multiple values that have the same properties.
    E.g., we create a single creator node for all creators found in
    a collection that have the same name a date of birth.

    Addition (bibliographical) information is accumulated in the RDF
    node.

    PROBLEM: sometimes the additional information clarifies the relation
    of the shared resource to a specific work and sometimes it privides
    more information about the resource (e.g. place of birth).

For cases (2) and (3) above, each metadata field has zero or more RDF
nodes that act as value. The principal value is represented by
rdf:value, while the others use the original property name. E.g., the
AHM data contains

    ==
    Record title Title .
    Record title.type Type .
    ==

This is translated into

    ==
    Record title [ a ahm:Title ;
		   rdf:value "Some title" ;
		   ahm:titleType Type ;
		 ] .
    ==

If the work has multiple titles, each title is represented by a
separate node.

Because this step may involve using ordering information of the initial
XML data that is still present in the raw converted RDF graph, this step
must be performed before the data is saved.


---++ Re-establish internal links

This step is generally trivial. Some properties represent links to other
works in the collection. The property value is typically a literal
representing a unique identifier to the target object such as the
collection identifier or a database key. This step replaces the
predicate-value with an actual link to the target resource.


---++ Re-establish external links

This step re-establishes links from external resources such as
vocabularies which we know to be used during the annotation. In this
step we only make mapping for which we are _absolutely_ sure. I.e., if
there is any ambiguity, which is not uncommon, we maintain the value as
a blank node created in step (1).


---++ Create a mapping schema

It is adviced to maintain the original property- and type-names
(classes) in the RDF because this

    1. Allows to reason about possible subtle differences between the
    source-specific properties and properties that come from generic
    schemas such as Dublin Core. E.g., a creator as listed for a work in
    a museum for architecture is typically an architect and the work in
    the museum is some form of reproduction on the real physical object.
    If we had replacd the original creator property by
    =|dcterms:creator|=, this information is lost.

    2. It makes it much easier to relate the RDF to the original
    collection data.  One of the advantages of this is that it becomes
    easier to reuse the result of semantic enrichment in the original
    data-source.

The toolkit provides a predicate to derive the initial schema from the
converted data using the predicate make_schema/2:

    * [[make_schema/2]]

After running this predicate, the schema can be downloaded from the
target graph through the web-interface, or it can be saved using
rdf_save_turtle/2, as in

    ==
    ?- make_schema(data, schema).
    ?- rdf_save_turtle('schema.ttl', [graph(schema)]).
    ==


---++ Assign URIs to blank nodes where applicable.

Any blank node we may wish to link to from the outside world needs to be
given a real URI.  The record-URIs are typically created from the
collection-identifier. For other blank nodes, we look for distinguishing
(short) literals.


---+ Enriching the crude RDF

The obtained RDF is generally rather crude. Typical `flaws' are:

    * It contains literals where it should have references to other RDF
    instances

    * One probably wants proper resources for many of the blank nodes.

    * Some blank nodes provide no semantic organization and must be
    removed.

    * At other place, intermediate instances must be created (as
    blank nodes or named instances).

    * In addition to the above, some literal fields need to be
    rewritten, sometimes to (multiple) new literals and sometimes
    to a named or bnode instance.

Our rewrite language is a production-rule   system,  where the syntax is
modelled  after  CHR  (a  committed-choice    language   for  constraint
programming) and the triple notation is   based  on Turtle/SPARQL. There
are 3 types of production rules:

    * *|Propagation rules|* add triples

    * *|Simplication rules|* delete triples and add new triples.

    * *|Simpagation rules|* are in between. They match triples, delete
    triples and add triples,

The overall syntax for the three rule-types is (in the order above):

    ==
    <name>? @@ <triple>* ==> <guard>? , <triple>*.
    <name>? @@ <triple>* <=> <guard>? , <triple>*.
    <name>? @@ <triple>* \ <triple>* <=> <guard>? , <triple>*.
    ==

Here, <guard> is an arbitrary Prolog term. <triple> is a triple
in a Turtle-like, but Prolog native, syntax:

    ==
    { <subject> , <predicate> , <object> }
    ==

Any of these fields  may  contain  a   variable,  written  as  a  Prolog
variable: an uppercase letter followed by   zero or more letters, digits
or the underscore. E.g.,  =Hello=,   =Hello_world=,  =A9=. Resources are
either fully (single-)quoted Prolog atoms (E.g. 'http://example.com/me',
or terms of the form <prefix>  :   <local>,  where <prefix> is a defined
prefix (see rdf_register_ns/2) and <local> is   a possible quoted Prolog
atom. E.g., =|vra:title|= or =|ulan:'Person'|= (note the quotes to avoid
interpretation as a variable). Literals can use a more elaborate syntax:

    ==
    <string> ^^ <type>
    <string> @ <lang>
    <string>
    literal(Atom)
    ==

Here, <string> is  a  double-quoted  Prolog   string  and  <type>  is  a
resource. The form literal(Atom) can be  used   to  match the text of an
otherwise unqualified literal with a variable.  I.e.,

    ==
    { S, vra:title, literal(A) }
    ==

has the same meaning as the SPARQL expression =|?S vra:title ?A FILTER
isLiteral(?A)|=,

Triples in the _condition_ side can  be   postfixed  using '?', in which
case they are optional matches. If the triple cannot be matched, triples
on the production-side that use the variable are ignored.

Triples in the _condition_  can  also  be   enclosed  in  a  Prolog list
([...]), In this case, the triples are   requested  to be in the *order*
specified. Ordering is not an official part   of  the RDF specs, but the
SWI-Prolog RDF store maintains the order of  triples in generated in the
XML conversion process. An ordered set  can   match  multiple times on a
given subject, where it AB can match both AAABBB and ABABAB.  Both forms
appear in real-world XML data.

Finally, on the _production_ side, the _object_ can take this form:

    ==
    bnode([ {<predicate> = <object>}
	  ],
	  [ {<option>}
	  ])
    ==

This means, `for the object, create a bnode from the given <predicate> =
<object> pairs'. The <option>s guide the  process. At this moment, there
is only one option with two values:

    ==
    share_if(equal)
    share_if(equal([<predicate>*]))
    ==

Without any option, each execution of the rule creates a new bnode. With
the =share_if= option  =equal=,  it  uses   the  same  bnode-id  for all
productions that produce the same   predicate-object  list (in canonical
order, after removing duplicates). Using the last form, it considers two
blank nodes equal if they have the same triples on the given predicates.
All other predicates are simply added to the blank-node.


---++ Renaming resoures (or naming blank-nodes)

The construct =|{X}|= can be used on the  condition and action side of a
rule. If used, there must be  exactly   one  such construct, one for the
resource to be deleted  and  one  for   the  resource  to  be added. All
resources for which the  condition  matches   are  renamed.  Below is an
example rule. The first triple extracts the identifier. This triple must
remain in the database. The =|\ {A}=|  binds the (blank node) identifier
to be renamed. The two Prolog guards verify that the resource is a blank
node and generate an identifier (URI).  The _action_ (_|{S}|_) gives the
rule engine the URI that must be given to the matched =|{A}=|.

    ==
    work_uris @@
    { A, vra:'idNumber.currentRepository', ID } \ {A} <=>
	rdf_is_bnode(A),
	literal_to_id(ID, ahm, S),
	{S}.
    ==


---++ Putting triples in another graph

Triples created by the _action_ side of a rule are added to the graph
that is being rewritten. It is also possible to add them to another
graph using the syntax below:

    ==
	{ S,P,O } >> Graph
    ==

E.g., if we want to store the information about person resources that
we create in a graph named =persons=, we can so so using a rule like
this:

    ==
    person @@
    {S, creator, Name},
    {S, 'creator.date_of_birth', Born} ?,
    {S, 'creator.date_of_death', Died} ?,
    {S, 'creator.role', Role} ?
	    <=>
	    Name \== "onbekend",
	    name_to_id(Name, ahm, Creator),
	    { S, vra:creator, Creator },
	    { Creator, rdf:type, ulan:'Person' } >> persons,
	    { Creator, vp:labelPreferred, Name } >> persons,
	    { Creator, ulan:birthDate, Born }    >> persons,
	    { Creator, ulan:deathDate, Died }    >> persons,
	    { Creator, ulan:role, Role }	 >> persons.
    ==


---++ Utility predicates

The rewriting process is often guided by a _guard_ which is, as already
mentioned, an arbitrary Prolog goal. Because translation of repositories
shares a lot of common tasks, we plan to develop a library for these.
This section documents the available predicates.

    * [[find_in_vocabulary/3]]
    * [[literal_to_id/3]]
    * [[name_to_id/3]]


---+ Putting it all together (examples)

Below we give some rules that we wrote to convert real data.

---++ Deleting a triple

Sometimes XML contains data that  simply   means  `nothing'.  We want to
delete this data:

    ==
    {_, creator, "onbekend" } <=>
	true.
    ==

Now, in the data from which this was  extracted, this is a bit too crude
because some records keep data  about   the  creator even though his/her
name is not known. Therefore, we preceed the   rule with the rule of the
next section. Note that the order of   rules  matter: a rule is executed
before the next one. In this particular   case we could have removed the
=|{S, creator, "onbekend"}|= triple from the   example  below to make it
match after the rule above is executed.


---++ Preserving info about unknown creators

The example below deals with entries in the database where the `creator'
is unknown (Dutch: _onbekend_), but some  properties are known about him
or her. The remainder of  the   condition  matches  possible information
about this creator using an `optional' match. The _guard_ verifies there
is at least some information  about   our  unknown creator. The _action_
part of the rule associates a new blank node as a creator.

    ==
    creator_onbekend @@
    {S, creator, "onbekend"},
    {S, 'creator.date_of_birth', Born} ?,
    {S, 'creator.date_of_death', Died} ?,
    {S, 'creator.role', Role} ?
	    <=>
	    at_least_one_given([Born, Died, Role]),
	    { S, vra:creator,
	      bnode([ ulan:birthDate = Born,
		      ulan:deathDate = Died,
		      ulan:role = Role
		    ])
	    }.

    at_least_one_given(Values) :-
	    member(V, Values),
	    ground(V), !.
    ==

---++ Negation

Negation is only provided as Prolog negation--by-failure in the guard.
This implies that we cannot use the =|{...}|= triple notation to test on
the absence of a triple, but instead we need to use the SWI-Prolog
RDF-DB primitive rdf/3. For example, to delete all person records that
have no name, we can use the rule below. The first triple verifies the
record-type. The second matches all triples on that record and the guard
verifies that the subject has no triples for the property ahm:name.

    ==
    delete_no_name @@
    { S, rdf:type, ahm:'Person' },
    { S, _, _ }
	    <=>
	    \+ rdf(S, ahm:name, _).
    ==

---+ Running the toolkit

Currently, there is no well-defined workflow for running the tools. The
files run.pl and rewrite.pl contain a skeleton that I use to convert the
data from AHM (Amsterdams Historisch Museum). The file run.pl loads
relevant background data and defines run/0 to call the initial
converter. The relevant steps of the initial converter are to load VRA
and mapping.ttl that contains the map:xmlname declarations discussed
above. Next, we load the XML into crude RDF using the call below. The
options specify that the input in XML without namespaces (dialect =xml=
rather than =xmlns=) and that the file contains XML elements named
=record= as the desired unit of data for conversion.

    ==
    run(File) :-
	    load_xml_as_rdf(File,
			    [ dialect(xml),
			      unit(record)
			    ]).
    ==

The result can be browsed by typing =|?- triple20.|=

The file rewrite.pl scripts the rewrite phase.  It sets up namespaces,
calls to the rewrite predicates with the proper arguments and finally
provides the rules.  Here are the toplevel predicates:

    * [[rewrite/0]]
    * [[rewrite/1]]
    * [[rewrite/2]]
    * [[list_rules/0]]

Below is an example run, showing all available rules and running a
single rule. The example demonstrates that rules are applied until a
fixed-point is reached (i.e., the RDF database does not change by
applying the rules).

    ==
    ?- [rewrite].
    true.

    ?- list_rules.
    Defined RDF mapping rules:

	    title_translations
	    dimension
	    work_uris
	    creator_sequence
	    creator_onbekend
	    delete_unknown_creator
	    delete_empty_literal
	    creator
	    material_aat
	    related_object

    true.

    ?- rewrite(delete_empty_literal).
    % Applying ... delete_empty_literal (1)
    % 0.100 seconds; 23,456 changes; 2,008,860 --> 1,985,404 triples
    % Step 1: generation 2,020,746 --> 2,044,202
    % Applying ... delete_empty_literal (1)
    % 0.000 seconds; no change
    % Step 2: generation 2,044,202 --> 2,044,202
    true.
    ==
