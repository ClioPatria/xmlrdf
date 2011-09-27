---+ The Amsterdam Museum example

Below are the steps for converting the   the  Amsterdam Museum XML dumps
into RDF.  We encourage copying the content of this directory into a new
directory for starting a new conversion process.

---++ Running the AHM example

  1. Make sure that the data can be found.  By default, these scripts
  are configured to look in $RDF_METADATA_DIR/AHM or ~/RDF/metadata/AHM.
  In this directory they expect a subdirectory =src= holding these
  files that contain the original XML dumps from the Amsterdam Museum:

    - people.xml
    - thesaurus.xml
    - collection-<start>-<end>.xml

  2. Configure ClioPatria and make sure the xmlrdf package is installed
  using

    ==
    ?- cpack_install(xmlrdf).
    ==

  3. Load the desired script. For converting the people database, this
  is done using this command:

    ==
    ?- ['cpack/xmlrdf/run-people'].
    ==

  4. Run the conversion using

    ==
    ?- ahm_run_people.
    ==

