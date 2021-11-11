
%.ps: %.scala
	enscript -2r --lines-per-page=57 -o $@ $<
