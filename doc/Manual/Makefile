manual = R-Forge_Manual
IA = R-Forge_I+A
R_FORGE = R-Forge.R-project.org
destination = /srv/gforge/download/

all: manual IA

manual: 
	pdflatex ${manual}
	bibtex ${manual}
	bibtex ${manual}
	pdflatex ${manual}
	pdflatex ${manual}

IA: 
	texi2pdf ${IA}.tex	

clean:
	rm -f *~
	rm -f $(manual).{aux,log,bbl,blg,out}
	rm -f $(IA).{aux,log,bbl,blg,out} 

install:
	scp ${manual}.pdf ${R_FORGE}:${destination}
