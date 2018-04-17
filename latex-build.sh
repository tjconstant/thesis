xelatex -synctex=1 -interaction=nonstopmode -shell-escape -enable-write18 thesis.tex
bibtex thesis
makeindex thesis
xelatex -synctex=1 -interaction=nonstopmode -shell-escape -enable-write18 thesis.tex
makeindex thesis
xelatex -synctex=1 -interaction=nonstopmode -shell-escape -enable-write18 thesis.tex
./latex-clean.sh
