#########################
# GRADES
#########################

hdir<-path.expand("~/Desktop/Teaching/IntroMacro/Current/Records/")

#source("~/Desktop/Common/R_Code/ParallelComputation.R")
#DISCUSSANTS <- read.csv( paste0(hdir, "/Discussant.csv") )

#------------------------------------------------------------------
##################
# Import Records
##################

## Local CSV
grades <- read.csv( paste0(hdir, "/ClassList.csv") )
rownames(grades) <- NULL

## Formatting
#grades <- grades[grades$Section=="???",]
#grades[ grades$Student!="    Points Possible",]

#------------------------------------------------------------------
##################
# Class Weights
##################

student <- c("Student") #c("LastName", "FirstName")

cuts <- as.data.frame( rbind( 
    c(90, "A"),
    c(80, "B"),
    c(70, "C"),
    c(60, "D"),
    c(50, "F")))
names(cuts) <- c("score", "letter")

weight <- as.data.frame( rbind(
    c(ARscore, .05),
    c(Discuss, .10),
    c(Exam1,   .20),
    c(Exam2,   .20),    
    c(Exam3,   .20),
    c(Final,   .25)))
names(weight) <- c("assignment", "weight")
  

#------------------------------------------------------------------
##################
# Calculating Total grades
##################


#Reading Assignments, 10%
readings <- sapply( c("D0", "D1", "D5", "AR"),
    grep,
    colnames(grades))
AR <- grades[ , unlist(readings)]
ARscore <- rowSums(AR, na.rm=TRUE)/(ncol(AR)-1)

#Homework Assignments, 15%
HW      <- grades[ ,grep("HW", colnames(grades) )]
HWtot   <- rep(1, ncol(HW) )
#HWtot  <- sapply( strsplit(names(HW),"\\."), function(e) as.numeric(e[[2]]) ) 
HWscore <- rowSums(HW, na.rm=TRUE)/(sum(HWtot) )

## Raw Exam Scores + Bonus
#Exam 1, 15%
# Exam1 <-  c( grades[ ,grep("Exam1", colnames(grades) )] + 3 )/ 33
Exam1 <- c(
    grades[ , colnames(grades)=="Exam1"] +
    grades[ , colnames(grades)=="Exam1Bonus"] ) / 33
#Exam 2, 20%
#Exam2 <-  c( grades[ ,grep("Exam2", colnames(grades) )] + 0 )/ 30
Exam2 <- c( grades[ , colnames(grades)=="Exam2"] ) / 30
#Exam 3, 20%
Exam3 <-  c(
    grades[ , colnames(grades)=="Exam3"] +
    grades[ , colnames(grades)=="Exam3Bonus"] )/ 35

#Final Exam (Comprehensive), 20%
Final <- c( grades[ ,colnames(grades)=="FinalExam"] )/ 76 

#------------------------------------------------------------------
##################
# Fixes 
##################

# ??? missed Exam 3
# manually replaced with final
# (53/76)*35 -> x


#------------------------------------------------------------------
##################
# Calculating distribution of grades
##################



#grade_plot(paste0(hdir,"Exam1.pdf") , Exam1*100, cuts)
#grade_plot(paste0(hdir,"Exam2.pdf") , Exam2*100, cuts)
#grade_plot(paste0(hdir,"Exam3.pdf") , Exam3*100, cuts)
#grade_plot(paste0(hdir,"FinalExam.pdf") , Final*100, cuts)

#------------------------------------------------------------------
##################
## Break Score
##################

grades$BREAKscore <- score_calc(head(weight, -1))
#(ARscore*.05 + HWscore*.05 + Exam1*.2 + Exam2*.25 + Exam3*.25) / (.05 + .05 + .2 + .25 + .25)

BREAKscore <- letter_calc(grades$BREAKscore, cuts)
table(BREAKscore)

grades$Exempt <- BREAKscore == "A"

## List
grades[ order(grades$BREAKscore),
    c(student, "Exempt", "BREAKscore", "Exam3", "Exam2", "Exam1")]
#grades[ ,c("Student", "Exempt", "BREAKscore", "Exam3")]

#------------------------------------------------------------------
##################
## Total Score
##################

grades$TOTscore    <- score_calc( weight )

# (ARscore*.05 + HWscore*.05 + Exam1*.2 + Exam2*.25 + Exam3*.25 + Final*.2) / (.05 + .05 + .2 + .25 + .25 + .2)

## Scaling
grades$LetterGrade <- letter_calc(grades$TOTscore, cuts)
grades$LetterGrade[ grades$Exempt==TRUE] <- "A"


## List
grades[ order(grades$TOTscore),c(student, "TOTscore", "LetterGrade")]

grades[ ,c(student, "TOTscore", "LetterGrade")]


# Student Viewing
# cbind(grades[,student], TOTscore, HWscore, ARscore, Exam1, Exam2)

