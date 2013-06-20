all: final.pdf

.PHONY: final.pdf final.tex

final.pdf: final.tex
	xelatex $< -o $@
	bibtex final
	xelatex $< -o $@

final.tex: final.md
	pandoc --template template.tex --latex-engine=xelatex --toc $< -o $@

clean:
	rm -f {references,final}.{tex,pdf,aux,bbl,blg,log,out,bcf,toc}

p:
	permamake.sh template.tex references.bib final.md Makefile
