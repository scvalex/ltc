all: build

.PHONY: all build clean p

build: presentation.pdf

presentation.pdf: presentation.md template.tex
	pandoc --template template.tex -t beamer $< -o $@

clean:
	rm -f presentation.pdf

p:
	permamake.sh *.tex *.md Makefile
