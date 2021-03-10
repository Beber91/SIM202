#!/bin/bash

Rscript -e "library(knitr); knit('rapport.Rnw')"
pdflatex rapport.tex