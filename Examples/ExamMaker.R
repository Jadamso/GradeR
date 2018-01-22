#########################
# Exam Maker
#########################
## Code Directory
library(GradeR)

message("After printing:
    manually mix the exams
    ie. (A,B,A,B,..)")
message("When Distributing the Exams:
    distribute from left to right
    do not break the order")

message("Mapping Between Answers between versions is currently not yet supported, i.e.
        Exam1 Ques.1, Ans=A  maps to
        Exam2 Ques.4, Ans=C")
#-------------------------------------------------------------------
##################
# Inputs
################## 

## Input Directory
hdir <- path.expand("~/Desktop/Teaching/IntroMacro/Current/Exams/Questions/")

rdir <- paste0(hdir,"ReadingQuestions/")
ldir <- paste0(hdir,"LectureQuestions/")


## All Questions from Directories
l_files <- system( paste0("ls ", ldir) , intern=TRUE)
r_files <- system( paste0("ls ", rdir) , intern=TRUE)

#-------------------------------------------------------------------
##################
# Output Locations
################## 

odir  <- path.expand("~/Desktop/Teaching/IntroMacro/Current/Exams/")
lodir <- paste0(odir,"RandLectureQuestions/")
rodir <- paste0(odir,"RandReadingQuestions/")

dir.create(lodir)
dir.create(rodir)

#-------------------------------------------------------------------
##################
# Randomize Questions for all Files
################## 

sapply(l_files,
    FUN=rand_fun,
    latex.dir.in=ldir,
    latex.dir.out=lodir,
    do.randomize.question=FALSE,
    n.test=2)

sapply(r_files,
    FUN=rand_fun,
    latex.dir.in=rdir,
    latex.dir.out=rodir,
    n.test=2)


# R -e 'source( ~"/Desktop/Teaching/IntroMacro/2017_Spring/Exams/Questions/LectureQuestions/ExamMaker.R")'

system('cd  ~/"Desktop/Teaching/IntroMacro/Current/Exams/FinalExam"  && ExamMaker FinalExam -u')
