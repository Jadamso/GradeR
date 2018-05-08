#------------------------------------------------------------------
##################
# Author Info
################## 

#devtools::session_info()
JordanR <- person(
        given = "Jordan",
        family= "Adamson",
        email = "jordan.m.adamson@gmail.com",
        role  = c("aut", "cre"))
        
authors_at_r <- paste0( "'", JordanR, "'" )

options(devtools.name="Jordan Adamson", 
    devtools.desc.author=JordanR)

#------------------------------------------------------------------
##################
# Package Description
################## 

Title <- 'Functions to make exams, plot grades, ...'
Description <- ' '
#This projects adheres to semantic versioning <http://semver.org/>

URL   <- paste0(' <https://github.com/Jadamso/',pack,'>')
#'https://sites.google.com/a/g.clemson.edu/ja-resources'
Maint <- "Jordan Adamson <jordan.m.adamson@gmail.com>"
Author <- "Jordan Adamson [aut, cre]"


my_description <- list(
    "Title"      = Title,
    "Author"     = Author,
    "Maintainer" = Maint, 
    "Description"= Description,
    "Published"  = Sys.Date(),
    "Date"       = Sys.Date(),
    "URL"        = URL,
    "Version"    = Version,
    "Authors@R"  = 'person("Jordan", "Adamson",
        email="jordan.m.adamson@gmail.com",
        role=c("aut","cre"))'
)
options(devtools.desc=my_description)

#------------------------------------------------------------------
##################
# Create Package
################## 
## package.skeleton
## install.packages(sinew)

# license warning goes away with next code block
package.setup.safe(packg, check=F)


rdir    <- paste0(packg, "/R")
ddir    <- paste0(packg, "/data/")
idir    <- paste0(packg, "/inst") 
extdir  <- paste0(packg, "/inst/extdata/")

for (i in c(ddir, idir, extdir) ) { dir.create.safe(i) }


#------------------------------------------------------------------
##################
# Citation
################## 

year  <- as.numeric(format(Sys.Date(), "%Y"))
month <- as.numeric(format(Sys.Date(), "%m"))
note  <- sprintf("R package version %s", Version)
header <- citHeader(
    paste0("To cite package '", pack,"' in publications use:") )
textVersion <- paste0(
    "Jordan Adamson ",
    paste0( "(", year, "). "),
    paste0(pack,": ", Title, ". "),
    note, ".")

bibref <- bibentry(
    bibtype = "Manual",
    key = pack, 
    title = paste0("{",pack,"}: ", Title),
    textVersion = textVersion,
    author = authors_at_r,
    header = header, 
    year = year,
    month = month,
    note = note, 
    url = URL)
print(bibref, style = "citation")


writeLines(
    paste(format(bibref, "R"),
    collapse = "\n\n"),
    con=paste0(idir, "/CITATION"))
#------------------------------------------------------------------
##################
# License
################## 

options( devtools.desc.license='MIT + file LICENSE' )
devtools::use_mit_license(packg)
#writeLines( c( paste0("YEAR: ", year), "COPYRIGHT HOLDER: Jordan Adamson"), con=paste0( packg, "/LICENSE") )

package.setup.safe(packg, check=T)
