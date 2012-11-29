all: references.pdf

.PHONY: references.pdf references.tex

references.pdf: references.tex
	xelatex $< -o $@
	bibtex references
	xelatex $< -o $@
	xelatex $< -o $@

references.tex: README.md
	pandoc --template template.tex --latex-engine=xelatex --toc $< -o $@

clean:
	rm -f references.{tex,pdf,aux,bbl,blg,log,out}

p:
	permamake.sh README.md template.tex references.bib
