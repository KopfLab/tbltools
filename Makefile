# tools for active package development

VIGNETTES_RMD = $(wildcard vignettes/*.Rmd)
VIGNETTES_PDF = $(VIGNETTES_RMD:.Rmd=.pdf)

all: docu check

docu: Roxygen site vignettes

Roxygen:
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"

# generate site (includes HTML version of vignettes)
site:
	Rscript -e "pkgdown::build_site()"

# generate vignettes (pdf and word, excluding HTML, better to have that from site
vignettes: $(VIGNETTES_PDF)
	# move PDF and word
	mv -f vignettes/*.pdf docs/articles/
	mv -f vignettes/*.docx docs/articles/
	# remove other intermediates (HTML should come from build_site)
	rm -f vignettes/*.html
	rm -f vignettes/*.xlsx
	rm -f vignettes/*.png

vignettes/%.pdf: vignettes/%.Rmd
	Rscript -e "require(rmarkdown); render('$<', output_format = 'all')"
	
check:
	Rscript -e "devtools::check()"

gui_dev:
	bundle exec guard

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
