all: ps1-kritphong.pdf

%.tex: %.Rnw
	Rscript -e "library(knitr); knit('$^')"

%.pdf: %.tex
	pdflatex $^

.PHONY: clean

clean:
	rm -fv	ps1-kritphong.pdf \
		ps1-kritphong.tex \
		ps1-kritphong.aux \
		ps1-kritphong.log \
		texput.log
