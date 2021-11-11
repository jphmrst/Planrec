
%.ps: %.scala Makefile
	a2ps -2r --chars-per-line=80 -o $@ $<
#	enscript -2r --lines-per-page=57 --columns-per-page=80 -o $@ $<
