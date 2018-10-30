#------------------------------------------------------------------
##################
## Download Experiment
##################
# http://faculty.cbpp.uaa.alaska.edu/jmurphy/handda/handda.html#handda

#message( "SEE ALSO
# http://www.chapman.edu/research-and-institutions/economic-science-institute/academics-research/software.aspx")

#cd ~/"Desktop/Teaching/Econ 212 (Introduction to Macro)/2016/Lectures/11.Prices/"
#wget http://faculty.cbpp.uaa.alaska.edu/jmurphy/handda/student_instructions.doc
#wget http://faculty.cbpp.uaa.alaska.edu/jmurphy/handda/buyers20.xls http://faculty.cbpp.uaa.alaska.edu/jmurphy/handda/sellers20.xls

## Manually export xls to csv
## Panes > Page Only
## PageLayout > Single Page
## Magnification > Fit Visible

#for(i in 2:21){
#    BUYi    <- xls2csv( BFILE, sheet=i)
#    BFILEi  <- paste0(fdir, "BUYERS/buyers20_",i,".csv")
#    write.csv(BUYi, BFILEi)
#}

#------------------------------------------------------------------
##################
# Market Setup Functions
##################

#' function to assign benefits/costs 
#' @param scale,res are parameters for value=Q*scale+res 
#' 
#' @return vector
#' 
# @details
# @examples
#' @export
value_assign <- compiler::cmpfun( function(base, scale, res) {
    base*scale + res} )


#' function to setup market 
#' @param scale,res are parameters for value_assign 
#' @param buyers,sellers are sequence of buyer and seller ID's
#' @param units sequence of units for each buyer or seller
#'
#' @return vector
#' 
# @details
# @examples
#' @export
market <- compiler::cmpfun( function(buyers, sellers, units,
    cscale=2, bscale=2){

    BUYERS  <- do.call( rbind,
        lapply( units, function(i) { cbind(buyers, i)} ) )
    colnames(BUYERS) <- c("Buyers", "Qd")
    
    SELLERS <- do.call( rbind,
        lapply( units, function(i) { cbind(sellers, i)} ) )
    colnames(SELLERS) <- c("Sellers", "Qs")

    heterogeneity <- round( (buyers/max(buyers))*1 , 2)

    COSTS    <- c( sapply( units, value_assign,
        scale=cscale, res=heterogeneity ) )
    BENEFITS <- c( sapply( rev(units), value_assign,
        scale=bscale, res=heterogeneity ) )

    MARKET <- data.frame(BUYERS, SELLERS, BENEFITS, COSTS )
    return(MARKET)
})



#' function to setup multiple markets
#' @param bscales,cscales are parameters for value_assign 
#' @param buyers,sellers,units are parameters for market
#' @param periods is a sequence of market ID's (trading rounds)
#'
#' @return vector
#' 
# @details
# @examples
#' @export
experiment <- compiler::cmpfun( function(
    periods, buyers, sellers,
    units, bscales, cscales){
    
    lapply(periods, function(i){
        market(buyers, sellers, units,
            bscale=bscales[i], cscale=cscales[i])
    } )
})

#------------------------------------------------------------------
##################
# Print Worksheet Functions
##################

#' format printable worksheet for sellers
#' @param EXPERIMENT output from experiment function
#'
# @return
#' 
# @details
# @examples
#' @export
experiment_sellers <- compiler::cmpfun(function(EXPERIMENT) {

    EXPERIMENT_SELLERS <- lapply( unique(EXPERIMENT[[1]]$Sellers),
        function(i){
        
        experiment_seller <- lapply(EXPERIMENT, function(MARKETi){
            marketi <- MARKETi[ MARKETi$Sellers==i, "COSTS"]
            valuei  <- rbind(
                Costs=marketi,
                Price="______",
                Earnings="______" )
            colnames(valuei) <- paste0("Unit", units)
            return(valuei)
        })
        
        names(experiment_seller) <- paste0("Period ",
            seq(EXPERIMENT) )
            
        return(experiment_seller)
    })
    
    names(EXPERIMENT_SELLERS) <- paste0("Seller ", sellers)
    return(EXPERIMENT_SELLERS)
})


#' format printable worksheet for buyers
#' @param EXPERIMENT output from experiment function
#'
# @return
#' 
# @details
# @examples
#' @export
experiment_buyers <- compiler::cmpfun(function(EXPERIMENT) {

    EXPERIMENT_SELLERS <-lapply( unique(EXPERIMENT[[1]]$Buyers),
        function(i){
        
        experiment_seller <- lapply(EXPERIMENT, function(MARKETi){
            marketi <- MARKETi[ MARKETi$Buyers==i, "BENEFITS"]
            valuei  <- rbind(
                Benefits=marketi,
                Price="______",
                Earnings="______" )
            colnames(valuei) <- paste0("Unit", units)
            return(valuei)
        })
        
        names(experiment_seller) <- paste0("Period ",
            seq(EXPERIMENT) )
        
        return(experiment_seller)
    })
    
    names(EXPERIMENT_SELLERS) <- paste0("Buyer ", sellers)
    return(EXPERIMENT_SELLERS)
})



#' format printable worksheet for buyers or sellers in latex
#' @param EXPERIMENT output from experiment function
#' @param MESSAGES instructions to give to students
#'
# @return
#' 
# @details
# @examples
#' @export
tabi <- compiler::cmpfun( function(
    EXPERIMENTERS, MESSAGES){
    TABI <- lapply( seq(EXPERIMENTERS), function(i){
        expi <- EXPERIMENTERS[[i]]
        tabi <- stargazer::stargazer( expi ,
            title=names(expi),
            font.size="large",
            single.row = FALSE
        )
        tabi <- gsub("\\\\label",
            "\\\\vspace{-\\\\baselineskip} \\\\label" , tabi)
        tabi <- gsub("caption", "caption*", tabi)
        tabn <- c(
            paste0(" \\Huge{ \\textbf{",
                names(EXPERIMENTERS)[[i]], "} } \\\\"),
            MESSAGES,
            "\\large",
            tabi,
            "\\clearpage \n"
        )
        return(tabn)
    })
    names(TABI) <- names(EXPERIMENTERS)
    return(TABI)
})


#' write printable worksheet for buyers or sellers in latex
#' @param TABI output from tabi function
#' @param TABname name of *.tex document ("BUYERS" or "SELLERS")
#' @param folddir name of folder to write to
#'
# @return
#' 
# @details
# @examples
#' @export
outputTABLE <- compiler::cmpfun( function(
    folddir, TABname, TABI){

    mfile <- paste0( folddir, "/", TABname, ".tex")

    TFILES <- lapply( seq(TABI), function( i ){
        tabi <- TABI[[i]]
        tname <- gsub(" ","", names(TABI)[[i]] )
        tfile <- paste0(folddir, "/", tname, ".tex")
        writeLines(tabi, tfile)
        tfile
    } )

    ## Add Opening to Latex Document
    system( paste0(
        "echo  -e '\
        \\\\RequirePackage{ASetupEssay}\
        \\linespread{1.5} \
        \\\\begin{document} \
        ' &> ",
        mfile
    ) )

    ## Copy in Latex Tables
    lapply(TFILES, function(tfile) {
        system( paste0("cat ", tfile," &>> ", mfile) )
        system( paste0("rm ", tfile) )
    })

    ## Add Ending to Latex Document
    system( paste0(
        "echo  -e ' \
        \\\\end{document} \
        ' &>> ",
        mfile
    ))
})


