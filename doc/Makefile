# arch-tag: Makefile for documentation

all: Pesco.pdf

clean:
	-rm -f Pesco.{aux,log,tex} *~

Pesco.tex: ../MissingH/Regex/Pesco.lhs
	lhs2TeX --poly $< > $@

Pesco.pdf: Pesco.tex
	pdflatex Pesco.tex
