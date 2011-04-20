PL=	../../../run.pl \
	../examples/AHM/run-data.pl \
	../lib/xmlrdf/cvt_vocabulary.pl

all:	xmlrdf.pdf

xmlrdf.pdf: xmlrdf.tex README.tex
	pdflatex xmlrdf.tex

README.tex: README.txt $(PL)
	./pltotex.pl --section --text README.txt $(PL)

clean::
	rm -f *~
	rm -f README.tex README.out README.log README.idx README.aux
	rm -f xmlrdf.log xmlrdf.out xmlrdf.aux xmlrdf.idx
	rm -rf cache RDF-store

distclean:	clean
	rm -f xmlrdf.pdf
