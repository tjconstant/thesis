xelatex -synctex=1 -interaction=nonstopmode -shell-escape -enable-write18 thesis.tex
bibtex thesis
xelatex -synctex=1 -interaction=nonstopmode -shell-escape -enable-write18 thesis.tex
xelatex-synctex=1 -interaction=nonstopmode -shell-escape -enable-write18 thesis.tex
./latex-clean.sh
