# tools for active package development

check:
	Rscript -e "devtools::check()"

gui_dev:
	bundle exec guard

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
