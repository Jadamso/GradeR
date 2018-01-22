#-------------------------------------------------------------------
##################
# Libraries
##################

## Imported Packages: (.packages())
rfiles <- c("stargazer", "RndTexExams")
for( i in rfiles) {
    devtools::use_package( i, pkg=packg)
}



# See https://www.ctan.org/topic/exam

# https://cran.r-project.org/web/packages/exams
    ## useful alternative for blackboard integration
    
# https://cran.r-project.org/web/packages/ProfessR
    ## unused because it requires unique storage of exam questions

sfiles <- c("exams", "ProfessR")
for( i in sfiles) {
    devtools::use_package( i, "Suggests", pkg=packg)
}
#-------------------------------------------------------------------
##################
# Which Codes
################## 

rfile <- c(
    "TeachingFunctions_Assignments.R",
    "TeachingFunctions_Grades.R",
    "RLatexExam.R"
)


rfiles <- paste0(pdir,"Code/R_Code/",rfile)
    
# Move Code
file.copy(rfiles, rdir, overwrite=T )
devtools::load_all( rdir )

# Create Code Documentation
devtools::document( pkg=packg)

