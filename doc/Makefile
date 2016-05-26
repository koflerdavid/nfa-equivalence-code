TEXIFIED_LHS_FILES != find .. -iname '*.lhs' -printf '%p.tex '
#TEXIFIED_LHS_FILES = ../src/Algorithm/AutomataEquivalence.lhs.tex

LHS2TEX != which lhs2TeX
ifeq ($(origin LHS2TEX), "")
LHS2TEX != stack setup; stack build lhs2tex; echo "stack exec lhs2TeX"
endif

doc: report.tex $(TEXIFIED_LHS_FILES)
	echo $(TEXIFIED_LHS_FILES)
	latexmk -pdf -interaction=nonstopmode -synctex=1 report.tex

%.lhs.tex : %.lhs
	$(LHS2TEX) --poly $< -o temp1
	# lhs2TeX assumes a standalone output file and adds those three lines
	# They have to be removed before `pdflatex` can include them.
	grep -v '\usepackage{amstext}' temp1 > temp2
	grep -v '\usepackage{amssymb}' temp2 > temp3
	grep -v '\usepackage{polytable}' temp3 > temp4
	grep -v '\usepackage{stmaryrd}' temp4 > $@

.PHONY: clean

clean:
	for extension in lhs.tex aux fls fdb_latexmk log ptb synctex.gz toc; do\
		find .. -name '*'.$$extension -delete;\
	done
	$(RM) temp1 temp2 temp3 temp4 report.pdf
