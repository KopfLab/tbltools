# tools for active package development

VIGNETTES_RMD = $(wildcard vignettes/*.Rmd)
VIGNETTES_PDF = $(VIGNETTES_RMD:.Rmd=.pdf)

all: docu check

# documentation
docu: Roxygen site vignettes cleanup

Roxygen:
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"

# generate site (includes relevant HTML, pdf, docx vignettes)
site: site_articles site_html cleanup

site_html: vignettes/module.Rmd vignettes/demo.Rmd
	Rscript -e "pkgdown::build_site()"

site_articles: vignettes/module.pdf vignettes/demo.pdf

vignettes/%.Rmd: vignettes/assessment_tests.html
	Rscript -e "rmarkdown::render('$<', output_format = 'all')"

vignettes/%.html: vignettes/%.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'all')"

vignettes/%.pdf: vignettes/%.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'all')"
	mv -f vignettes/*.pdf docs/articles/
	mv -f vignettes/*.docx docs/articles/

# generate standard vignettes
vignettes: 
	Rscript -e "devtools::build_vignettes()"

# cleanup
cleanup:
	# remove temp files
	rm -f vignettes/*.R
	rm -f vignettes/*.html
	rm -f vignettes/*.xlsx
	rm -f vignettes/*.png
	rm -f vignettes/*.tex
	rm -f vignettes/*.docx
	rm -f vignettes/*.pdf
	rm -f vignettes/module.Rmd
	rm -f vignettes/demo.Rmd
	rm -f -R vignettes/peer\ eval\ 1
	rm -f -R vignettes/peer\ eval\ 2

# check
check:
	Rscript -e "devtools::check()"

# gui development
gui_dev:
	bundle exec guard

# auto testing during development
auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
