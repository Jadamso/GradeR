#########################
# Assign Readings
#########################

hdir <- path.expand("~/Desktop/Teaching/IntroMacro/Current/")
rdir <- paste0(hdir, "/Readings/Discussants/")

grades   <- read.csv( paste0(hdir, "Records/ClassList.csv") )

students <- grades[, c("First.Name", "Last.Name")]
students <- apply(students, 1, paste, collapse=" ")
#students <- students[ students!="    Points Possible"]
students <- c(students, "Addition, Late")


#-------------------------------------------------------------------
##################
# Randomly Assign Student Groups
##################
library(GradeR)
v <- length(students)  ## number of students
w <- 14  ## number of weeks
p <- 03  ## number of times each person presents
n <- 06  ## number of people per group
g <- 09  ## number of groups
m <- 06  ## number of presenters ## same as number of groups
a <- 10


set.seed(1)
DISCUSSANT_TABLE <- students2weeks.matrix2.no.dup(
    v=v, w=w, p=p, a=NA)

#-------------------------------------------------------------------
##################
# Assign Student Groups in Latex
##################

#library(Matrix)
#image(as(DISCUSSANT_TABLE,"sparseMatrix"))

DISCUSSANTS <- students2weeks.format(
    discussant_table=DISCUSSANT_TABLE,
    students=students)

students2weeks.print(DISCUSSANTS, w=w, rdir=rdir)



#-------------------------------------------------------------------
##################
# First week mix-up
##################

DISCUSSANTS <- read.csv( paste0(rdir, "/Discussant.csv") )


