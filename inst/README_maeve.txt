### Modeling Accessories Enabling Visualization of Experiments (maeve)
### is an R package under development in Genentech Research & Early
### Development (gRED) for analysis of longitudinal data from
### in vivo mouse studies.  The current use case is from Translational
### Oncology (e.g., mouse tumor xenograft studies).

### The under-the-hood workhorse packages are 'mgcv', 'gamm4',
### 'lme4', 'multcomp', 'settings', and the 'tidyverse.org' packages.

### An example workflow can be found in /inst/scripts/maeve_QuickStart.pdf.

### Version-to-version development has been via the "devtools"
### package in a UNIX environment. The following R code is
### an example of how to re-install a newly updated package,
### after incrementing the version number in the DESCRIPTION
### file (/maeve/DESCRIPTION).

### Bill Forrest (forrest.bill@gene.com), Michal Jakubczak, Pawel Piatkowski

####
#### R code below
####

### Make sure 'devtools' & 'testthat' are loaded:
 suppressPackageStartupMessages( library( devtools ) )
 suppressPackageStartupMessages( library( testthat ) )

###
### Run the following lines in sequence from the parent directory of maeve:
 devtools::document('maeve/') 
 devtools::test(    'maeve/') # see /inst/tests/
 devtools::check(   'maeve/')
 devtools::build(   'maeve/')
 devtools::install( 'maeve/')
### all done!
