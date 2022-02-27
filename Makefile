# main commands
## command to open R in interactive session
R:
	R --quiet --no-save

## command to remove automatically generated files
clean:
	rm -rf man/*
	rm -rf docs/*
	rm -rf inst/doc/*

# R package development commands
## build docs and run tests
all: man readme test check spellcheck

## build docs
man:
	R --slave -e "devtools::document()"

## simulate data
data:
	R --slave -e "source('inst/scripts/simulated-data.R')"
	R --slave -e "source('inst/scripts/test-data.R')"

## copy data to production directory
prod-data:
	cd /opt/whattodo/projects & rm -rf simulated-data
	cd /opt/whattodo/projects & rm -rf test-data
	cp -R inst/extdata/projects/simulated-data /opt/whattodo/projects
	cp -R inst/extdata/projects/test-data /opt/whattodo/projects

## rebubild readme
readme:
	R --slave -e "rmarkdown::render('README.Rmd')"

## run tests
test:
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf

## test examples
examples:
	R --slave -e "devtools::run_examples(test = TRUE, run = TRUE);warnings()"  >> examples.log
	rm -f Rplots.pdf

## run checks
check:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(cran = FALSE, build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1

## check docs for spelling mistakes
spellcheck:
	echo "\n===== SPELL CHECK =====\n" > spell.log 2>&1
	R --slave -e "devtools::spell_check()" >> spell.log 2>&1

## install package
install:
	R --slave -e "devtools::install_local(getwd(), force = TRUE, upgrade = 'never')"

## build entire site
site:
	R --slave -e "pkgdown::clean_site()"
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = FALSE)"

## rebuild update files for site
quicksite:
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = TRUE)"

# commands to launch app
## launch local version using system libraries
debug:
	R -e "Sys.setenv('SHINYPROXY_USERGROUPS'='admin'); options(golem.app.prod = FALSE); golem::run_dev()"

quick-debug:
	R -e "Sys.setenv('SHINYPROXY_USERGROUPS'='admin'); options(golem.app.prod = FALSE, quick = TRUE); golem::run_dev()"

## launch local version inside Docker container
demo:
	docker-compose up --build -d

demo-kill:
	docker-compose down

## launch released version inside Docker container
launch:
	docker run -dp 3838:3838 --name whattodo -it naturecons/whattodo

launch-kill:
	docker rm --force whattodo

## deploy app on shinyapps.io
shinyapps:
	R -e "rsconnect::deployApp(getwd(), appName = 'whattodo', launch.browser = TRUE)"

# Docker commands
## create local image and push to docker
image:
	docker build -t naturecons/whattodo:latest .
	docker push naturecons/whattodo:latest

## delete all local containers and images
reset:
	docker rm $(docker ps -aq) || \
	docker rmi -f $(docker images -aq)

# renv commands
## update deps
updatedeps:
	R --slave -e "remotes::install_github('NCC-CNC/whatdataio', force = TRUE, upgrade = 'never')"
	R --slave -e "renv::snapshot()"

## snapshot R package dependencies
snapshot:
	R -e "renv::snapshot()"

.PHONY: clean data readme test check install man spellcheck examples site quicksite snapshot deploy demo demo-kill image debug snapshot updatedeps
