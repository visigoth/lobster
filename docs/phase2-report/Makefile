#
# Makefile
#
# Copyright (C) 2015, Galois, Inc.
# All Rights Reserved.
#

PDF_FILES := phase2-report.pdf

all: $(PDF_FILES)

%.pdf: %.md
	@echo "PANDOC      $@"
	@pandoc -V documentclass=galois-proposal -f markdown+simple_tables -t latex -o $@ $<

clean:
	rm -f $(PDF_FILES)

# vim: set ft=make ts=2 noet:
