#------------------------------------------------------------------
##################
#' Randomly Assign Groups to Student Discussants
##################
#' 
#' @param g number of groups
#' @param n number of people per group
#' @param w number of weeks
#' @param v number of students
#' 
#' @return
#' 
# @details
# @examples
#' @export

groups2student <- function(g=9, n=6, w=14, v=40) {
    groups <- c(rep(1:g, ceiling(n) ))
    SEED   <- as.data.frame( sapply( 1:w, function(i){
        set.seed(i)
        seed  <- sample(groups, v)
    }) )
    names(SEED) <- paste0( "Group_", 1:w)
    SEED
}

#------------------------------------------------------------------
##################
#' Randomly Assign Student Discussants
##################
#' 
#' @param p number of times each person presents
#' @param v number of students
#' 
#' @return
#' 
# @details
# @examples
#' @export

random.matrix <- compiler::cmpfun( function(
    p,
    v) {
    
    sapply( seq( p ) , function(i) sample.int(v, v) )
    
})


#' Randomly Assign Student Discussants w/o duplicates
#' @rdname random.matrix
#' @export

random.matrix.no.dup <- compiler::cmpfun(function(
    p,
    v){
    
    repeat {
        mat  <- random.matrix(p, v)
        dups <- t(apply(mat, 1, duplicated))
        if ( any(dups) == FALSE){ break }
    }
    
    return(mat)
    
})

#nn <- mnormt::mnormt (d, p, r)
#nn <- mvtnorm::mvtnorm (d, r)
#quantile( nn, probs=seq(0,1,length.out=v))
## random multivariate uniform matrix

#------------------------------------------------------------------
##################
#' Randomly Assign Student Discussants to Weeks
##################
#' 
#' @param students matrix of students
#' @param w number of weeks
#' @param p number of times each person presents
#' @param v number of students
#' 
#' @return
#' 
# @details
# @examples
#' @export

students2weeks.matrix <- compiler::cmpfun( function(
    students,
    w=14,
    p=NA,
    v=NA) {

    if( is.na(v) ){ v <- length(students) }
    if( is.na(p) ){ p <- ceiling(v/w) }


    discussants <- random.matrix(p,v)

    ldw <- ceiling(length(discussants)/w) 
    groups <- sort( rep(1:w, ldw )[seq(discussants)] )

    cat(paste0("Number of Discussant presentations: ", p, " \n") )
    cat("Discussants per week:")
    print( table(groups))

    discussants1 <- split(discussants, groups[order(discussants)] )

    ## By Week
    discussants2 <- sapply( seq(discussants1), function(i){
        d0 <- discussants1[[i]]
        d1 <- as.matrix( students[d0] )
        d2 <- cbind(d1, i)
    })

    ## By Student Name
    discussants3 <- as.data.frame( do.call("rbind", discussants2))
    discussants3$i   <- as.numeric(discussants3$i)
    discussant_table <- table(discussants3)

    return(discussant_table)
})

#------------------------------------------------------------------
##################
#' Randomly Assign Student Discussants to Weeks without duplicates
##################
#' 
#' @rdname students2weeks.matrix
#' @export

students2weeks.matrix.no.dup <- compiler::cmpfun( function(
    students,
    w=14,
    p=NA,
    v=NA){
    
    repeat {
        mat  <- students2weeks.matrix(students, w=w, p=p, v=v)
        dups <- mat > 1
        if ( any(dups) == FALSE){ break }
    }
    return(mat)
})

#------------------------------------------------------------------
##################
#' Randomly Assign Student Discussants
##################
#' 
#' @param w number of weeks
#' @param p number of times each person presents
#' @param v number of students
#' @param a
#' 
#' @return a vector
#' 
# @details
# @examples
#' @export


students2weeks.matrix2 <- compiler::cmpfun( function(
    v,
    w,
    p=NA,
    a=2){

    if( is.na(p) ){ p <- ceiling(v/w) }

    cat( paste0("Number of Discussant presentations: ", p, " \n") )
    cat("Discussants per week:")

    weeks <- table( sort( rep(1:w, ceiling(p*v/w) )[seq(p*v)] ) )
    print( weeks )


    mat <- matrix(0, nrow=v, ncol=w)

    for(i in 1:ncol(mat)){
        if(i==1){
            probs <- rep(p,v)
        } else  if(i==2) {
            ## Noone goes first 2 weeks twice
            probs <- 1-rowSums( as.matrix(mat[,1]))
        } else {
            probs <- p-rowSums( as.matrix(mat[,1:(i-1)]))
            probs <- probs^a ## a is the dispersion parameter
            ## higher a, means greater equidispersoin
        }

        si <- sample.int(v,weeks[[i]], prob=probs)
        mat[si,i] <- 1 
    }

    return(mat)
})

#------------------------------------------------------------------
##################
#' Randomly Assign Student Discussants
##################
#' 
#' @rdname students2weeks.matrix2
#' @export


students2weeks.matrix2.no.dup <- compiler::cmpfun( function(
    v,
    w,
    p=NA,
    a=2){
    
    repeat {
        mat  <- tryCatch(
            students2weeks.matrix2(w=w, p=p, v=v),
            error = function(e) "Fail" )
        if ( mat[[1]] != "Fail"){ break }
    }
    return(mat)
})
#------------------------------------------------------------------
##################
#' Formatting Table for Export
##################
#' 
#' @param discussant_table table to format
#' @param students matrix of students
#' @param write_file write table to csv? default NA
#' 
#' @return a vector
#' 
# @details
# @examples
#' @export


students2weeks.format <- compiler::cmpfun( function(
    discussant_table,
    students,
    write_file=NA){

    rownames(discussant_table) <- students
    colnames(discussant_table) <- seq(ncol(discussant_table))

    weekly_table <- apply(discussant_table, 1, function(i){ 
        which(i==1) })
    weekly_table <- apply(weekly_table, 2, sort)

    weekly_table <- as.data.frame( t(weekly_table) )
    names(weekly_table) <- paste0("Discuss", 1:ncol(weekly_table))

    ## Save Output
    if( !is.na(write_file) ){
        utils::write.csv( weekly_table, write_file)
        return(NA)
    } else {
        return(weekly_table)
    }

})


#------------------------------------------------------------------
##################
#' Write A Latex Table for Each Week
##################
#' 
#' @param discussants list/matrix of discussants
#' @param w number of weeks
#' @param rdir directory to write discussants to
#' 
#' @return a vector
#' 
# @details
# @examples
#' @export


students2weeks.print <- compiler::cmpfun( function(
    discussants,
    w=13,
    rdir){
    
    for( week in  1:w ) {

        d_id <- which(discussants==week, arr.ind=TRUE)[,1]
        discussant_week <- rownames(discussants[ d_id, ])
        
        week_name   <- paste0(rdir, "Week", week, ".tex")
        out_table <- stargazer::stargazer(
            as.matrix(discussant_week),
            out=NULL,
            title="Student Discussants")
        out_table <- sub("c}", "l} ", out_table)
        
        writeLines( out_table, week_name)
    }
    
})


