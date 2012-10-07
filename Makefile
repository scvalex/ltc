all: proposal.pdf

.PHONY: all clean

proposal.pdf: proposal.md
	pandoc --template template.tex --latex-engine=xelatex --toc $< -o $@

clean:
	rm -f proposal.pdf
