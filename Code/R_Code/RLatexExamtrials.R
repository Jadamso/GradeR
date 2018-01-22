#library(RndTexExams)
#library(GradeR)

## Copy Teaching Files to Trial Directory for Experimentation
latex.trial <- path.expand("~/Desktop/Teaching/IntroMacro/Current/Exams/Questions")
latex.in <- "/tmp/Trials/"
system( paste("cp -r ", latex.trial, latex.in) )

# f.in <- system.file("extdata","MyRandomTest_examdesign.tex",package="RndTexExams")
f.in <- paste0(latex.in, "LectureQuestions/TaxQuestions.tex")
my.test <- GradeR::rte.analyze.tex.file(f.in)


n.question <- max(my.test$df.questions$n.question)

# Builds pdfs
rdn.exam <- RndTexExams::rte.build.rdn.test(
    list.in = my.test,
    f.out = "RdnTest_",
    n.test = 1,
    n.question=n.question,
    latex.dir.out=tempdir())


#q.dir <- path.expand("~/Desktop/Teaching/IntroMacro/2017_Spring/Exams/LectureQuestions")
#system( paste0("ls ", q.dir) , intern=TRUE)
