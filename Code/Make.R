#-------------------------------------------------------------------
##################
# Setup 
##################
library(devtools)
library(roxygen2)
library(MiscUtils)

 
pmdir <- path.expand("~/Desktop/Packages/")

#-------------------------------------------------------------------
##################
# Make 
##################
pack  <- "GradeR"
pdir  <- paste0(path.expand("~/Desktop/Packages/"),pack,"/")
packg <- paste0(pdir, pack)

Version <- numeric_version("0.1.1")

# Setup R Package
source(paste0(pdir,"Code/PackageSetup.R") )

# Create R Package Contents
source(paste0(pdir,"Code/CodeSetup.R") )

pack_up(pdir)

#-------------------------------------------------------------------
##################
# Install 
##################
devtools::install(packg) ## Locally Works

devtools::install_github( paste0("Jadamso/",pack), subdir=pack)
## Public Package From Github Fails Often

citation(pack)

print("Done")

## source("~/Desktop/Packages/GradeR/Code/GradeR.R")

## R CMD BATCH --no-save Code/Make.R && rm Make.Rout

#mkdir ~/Desktop/Packages/GradeR/Examples
#cp ~/Desktop/Teaching/IntroMacro/Current/Records/{ExamMaker.R,Grades.R,StudentAssignment.R} ~/Desktop/Packages/GradeR/Examples

