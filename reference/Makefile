all: interim.pdf

.PHONY: references.pdf references.tex interim.pdf interim.tex

references.pdf: references.tex
	xelatex $< -o $@
	bibtex references
	xelatex $< -o $@
	xelatex $< -o $@

interim.pdf: interim.tex
	xelatex $< -o $@
	bibtex references
	xelatex $< -o $@
	xelatex $< -o $@

references.tex: README.md
	pandoc --template template.tex --latex-engine=xelatex --toc $< -o $@

interim.tex: interim.md
	pandoc --template template.tex --latex-engine=xelatex --toc $< -o $@

clean:
	rm -f references.{tex,pdf,aux,bbl,blg,log,out}

p:
	permamake.sh template.tex references.bib interim.md
