SHELL=		/bin/sh
RM=		/bin/rm
LATEX=		/usr/bin/latex
BIBTEX=		/usr/bin/bibtex
DVIPS=		/usr/bin/dvips
PS2PDF=		/usr/bin/ps2pdf
GS=		/usr/bin/gs

.SUFFIXES:      .tex .dvi .eps .ps .pdf

MAIN = thesis

EMAIN = ethesis

FIGDIR = figures

FIGURES =	$(FIGDIR)/MUN_Logo_Pantone.eps		\
		$(FIGDIR)/enrollment.eps 		\
		$(FIGDIR)/enrollment-landscape.eps 	\
		$(FIGDIR)/enrollment-rotate.eps 	\
		$(FIGDIR)/db-deadlock.eps

FILES = thesis.tex thesis.sty						\
	1-Abstract.tex contents.tex tables.tex figures.tex	\
	2-Introduction.tex 3-ReviewOfTheCurrentState.tex \
	4.1-PatternsOO.tex 4.2-PatternsFP.tex 4.3-Refactoring.tex 4.3-Project.tex \
	5-Conclusion.tex chap6.tex	\
	bib.tex 6-Bibliography.bib apdxa.tex

$(MAIN).dvi:    $(MAIN).tex $(FIGURES) $(FILES)
	$(LATEX) $*.tex; 
	$(BIBTEX) $*;
	$(LATEX) $*.tex;
	while grep -s 'Rerun' $*.log 2> /dev/null; do	\
		$(LATEX) $*.tex;			\
	done

.dvi.ps:        $*.dvi
	$(DVIPS) -Ppdf -G0 -t letter -o $@ $<
 
.ps.pdf:       $*.dvi
	$(PS2PDF) -sPAPERSIZE=letter $< $@

clean:
	$(RM) -f *.aux \
		$(MAIN).log $(MAIN).dvi $(MAIN).ps $(MAIN).blg $(MAIN).bbl \
		$(MAIN).lot $(MAIN).lof $(MAIN).toc $(MAIN).pdf $(EMAIN).pdf

# Suggested by Neil B.
neat:
	$(RM) -f *.aux \
		$(MAIN).log $(MAIN).blg $(MAIN).bbl \
		$(MAIN).lot $(MAIN).lof $(MAIN).toc
