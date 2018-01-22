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
    cuts){

    #tikzDevice::tikz( paste0(hdir, "Exam2.tex" ) )
    pdf( pdfname )
    
        ## Distribution of Grades
        h <- hist(score,
            breaks=50,
            col=rgb(0,0,0,.1),
            main="Frequency of Class Scores",
            xlab="Score (%)",
            ylab="# Students")
            
        ## CutPoints for Letter Grades
        for(i in 1:nrow(cuts) ) {
            abline(v=cuts[i,1], col="red")
            text( x=cuts[i,1], y=-0.1, cuts[i,2], adj=0, col="red")
        }
        
        ## Mean Score
        m <- round( mean(score, na.rm=TRUE) )
        abline(v=m, lty=2, col="blue")
        text(x=m, y=max(h$counts),
            paste0("m=",m),
            adj=c(0.5,-0.5), col="blue")

    dev.off()
})


