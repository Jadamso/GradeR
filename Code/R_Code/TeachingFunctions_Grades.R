#------------------------------------------------------------------
##################
#' Calculate Grades
##################
#' 
#' @param weight_df number of groups
#' @param verbose print output
#' 
#' @return
#' 
# @details
# @examples
#' @export


score_calc <- function(weight_df, verbose=TRUE){

    if(verbose){
        message("GG must be data.frame of 'assignments' and 'weight'")
        message("categories must be in the current environment")
    }

    weight_score <- apply( weight_df, 1, function( rowi ){ 
        gi <- get(rowi[1])
        wi <- as.numeric(rowi[2])
        gi*wi
    } )

    score <- rowSums(weight_score)/ sum(weight_df$weight)
    return(score)
}


#------------------------------------------------------------------
##################
#' Calculate Letter Grades
##################
#' 
#' @param weight_df number of groups
#' @param n number of people per group
#' @param w number of weeks
#' @param v number of students
#' 
#' @return
#' 
# @details
# @examples
#' @export

letter_calc <- compiler::cmpfun( function(
    scores,
    cuts,
    percent=FALSE){
    
    cut( scores*(100^(1-percent)),
        breaks=c(100, cuts$score),
        labels=rev(cuts$letter),
        right=FALSE,
        ordered_result=TRUE)
})

#------------------------------------------------------------------
##################
#' Calculate Grades
##################
#' 
#' @param pdfname name of pdf file
#' @param score matrix of class scores
#' @param cuts cuttoff points for letter grades

#' 
#' @return
#' 
# @details
# @examples
#' @export

grade_plot <- compiler::cmpfun( function(
    pdfname,
    score,
    cuts,
    breaks=seq(50,100,by=1),
    ymin=NA,
    ymax=NA){

    #tikzDevice::tikz( paste0(hdir, "Exam2.tex" ) )
    pdf( pdfname )
    
        ## Distribution of Grades
        h <- hist(score,
            breaks=breaks,
            col=rgb(0,0,0,.1),
            main="Frequency of Class Scores",
            xlab="Score (%)",
            ylab="# Students")
            
        ## CutPoints for Letter Grades
        ymax <-ifelse(is.na(ymax), max(h$counts), ymax)
        ymin <-ifelse(is.na(ymin), 0, ymin)
        for(i in 1:nrow(cuts) ) {
            segments(x0=cuts[i,1], y0=ymin, y1=ymax, col="red")
            text( x=cuts[i,1], y=ymin, #pos=1, #-1/30*ymax
                cuts[i,2], adj=c(0,1), col="red")
        }
        
        ## Mean Score
        m <- round( mean(score, na.rm=TRUE) )
        abline(v=m, lty=2, col="blue")
        text(x=m, y=ymax,
            paste0("mean\n",m),
            adj=c(.5,.5), col="blue")

    dev.off()
})


