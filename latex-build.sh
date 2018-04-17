pdflatex -synctex=1 -interaction=nonstopmode -shell-escape -enable-write18 thesis.tex
bibtex thesis
pdflatex -synctex=1 -interaction=nonstopmode -shell-escape -enable-write18 thesis.tex
pdflatex -synctex=1 -interaction=nonstopmode -shell-escape -enable-write18 thesis.tex
./latex.clean.sh
