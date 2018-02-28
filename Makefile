# tools for active package development

VIGNETTES_RMD = $(wildcard vignettes/*.Rmd)
VIGNETTES_PDF = $(VIGNETTES_RMD:.Rmd=.pdf)

all: docu check

docu: Roxygen site vignettes

Roxygen:
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"

site:
	Rscript -e "pkgdown::build_site()"

vignettes: $(VIGNETTES_PDF)
	mv -f vignettes/*.html docs/articles/
	mv -f vignettes/*.pdf docs/articles/
	mv -f vignettes/*.docx docs/articles/

vignettes/%.pdf: vignettes/%.Rmd
	Rscript -e "require(rmarkdown); render('$<', output_format = 'all')"
	
check:
	Rscript -e "devtools::check()"

gui_dev:
	bundle exec guard

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
