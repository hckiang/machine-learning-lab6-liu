TEXFILES  = Group2.tex
NAME      = Group2

all: pdf

pdf: $(NAME).pdf

%.pdf:
	pdflatex $(TEXFILES) && pdflatex $(TEXFILES) && pdflatex $(TEXFILES)

clean:
	rm -f $(NAME).log $(NAME).aux $(NAME).bbl $(NAME).synctex.gz $(NAME).pdf

