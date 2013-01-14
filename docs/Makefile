all: interim.pdf

.PHONY: interim.pdf interim.tex

interim.pdf: interim.tex
	xelatex $< -o $@
	bibtex interim
	xelatex $< -o $@

interim.tex: interim.md
	pandoc --template template.tex --latex-engine=xelatex --toc $< -o $@

clean:
	rm -f {references,interim}.{tex,pdf,aux,bbl,blg,log,out,bcf}

p:
	permamake.sh template.tex interim.bib interim.md Makefile
