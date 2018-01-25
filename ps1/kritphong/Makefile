all: ps1-kritphong.pdf

%.pdf: %.Rnw
	Rscript -e "library(knitr); knit('$^')" && \
	pdflatex $(^:.Rnw=.tex)

.PHONY: clean

clean:
	rm -fv	ps1-kritphong.pdf \
		ps1-kritphong.tex \
		ps1-kritphong.aux \
		ps1-kritphong.log \
		texput.log
