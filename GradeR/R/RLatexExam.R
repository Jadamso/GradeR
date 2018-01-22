#---------------------------------------------------------------------------
##################
# Latex File Import Functions
##################

#' helper commands to rte.analyze.tex.file to eliminate coded out lines
#' @param str.in
#' 
#' @return logical TRUE if line coded out
#' 
# @details
# @examples
#' @export
my.test.comment <- compiler::cmpfun( function(str.in) {
    first.letter <- stringr::str_sub(str.in, 1, 1)
    if (first.letter == "%") { return(TRUE) } else { return(FALSE)}
})

#' import and analyze a latex file
#' @param f.in character string for input file
#' 
#' @return exam
#' 
# @details
# @examples
#' @export
rte.analyze.tex.file <- compiler::cmpfun( function(f.in, bank=TRUE){
    cat("\nrte: Changing LaTeX file into dataframe...")

    my.text <- stringi::stri_read_lines(f.in)
    Encoding(my.text) <- "UTF-8"

    idx <- sapply(my.text, FUN=my.test.comment, USE.NAMES=FALSE)
    my.text <- my.text[!idx]

    if(bank){
        my.text <- c(
            " ","\\begin{questions}",
            my.text,
            "\\end{questions}"," " )
    }

    out <- RndTexExams::engine.analyze.class.exam(my.text)
    out$examclass <- "exam"

    return(out)
} )

#---------------------------------------------------------------------------
##################
# Latex Export
##################

#' 
#' @param n.cases.correct.answers
#' @param q.answers
#' @param i.answers
#' 
#' @return
#' 
# @details
# @examples
#' @export

i.n.cases.correct.answers <- compiler::cmpfun( function(
    n.cases.correct.answers,
    q.answers,
    i.cases
    ){

    for (i.cases in seq(n.cases.correct.answers) ) {
        idx <- which(stringr::str_detect(
            q.answers,
            stringr::fixed(paste0("[", i.cases, "]")))
            )
        q.answers[idx] <- stringr::str_replace_all(
            string = q.answers[idx], 
            pattern = stringr::fixed(
                paste0("[", i.cases, "]")),
            replacement = "")
    }

    return(q.answers)
} )

#' 
#' @param case.now
#' @param q.answers
#' @param str.pattern.correct
#' @param str.pattern.choice
#' 
#' @return
#' 
# @details
# @examples
#' @export

q.answers.cases.fun <- compiler::cmpfun( function(
    case.now,
    q.answers,
    str.pattern.correct,
    str.pattern.choice
    ){

    idx.correct.switch <- which(stringr::str_detect(q.answers, 
        stringr::fixed(paste0("[", case.now, "]"))))
    n.cases.correct.answers <- sum(stringr::str_detect(q.answers, 
        "\\[.*?\\]"))
    if (is.na(n.cases.correct.answers)) 
        n.cases.correct.answers <- 1

    q.answers <- i.n.cases.correct.answers(
        n.cases.correct.answers,
        q.answers,
        i.cases)

    idx.fix <- which(stringr::str_detect(q.answers, 
      stringr::fixed(str.pattern.correct)))
    q.answers[idx.fix] <- sub(
        pattern = str.pattern.correct, 
        x = q.answers[idx.fix],
        replacement = str.pattern.choice, 
        fixed = T)
    q.answers[idx.correct.switch] <- sub(
        pattern = str.pattern.choice, 
        x = q.answers[idx.correct.switch],
        replacement = str.pattern.correct, 
        fixed = T)

    return(q.answers)
} )


#' 
#' @param my.tex.file
#' @param f.temp.tex
#' @param bank
#' @param exam.class
#' @param str.pattern.end.mchoice
#' @param my.last.part
#' @param qtext
#' @param Qend=TRUE
#'
#' @return
#' 
# @details
# @examples
#' @export

latexout <- compiler::cmpfun( function(
    my.tex.file,
    f.temp.tex,
    bank,
    exam.class,
    str.pattern.end.mchoice,
    my.last.part,
    qtext,
    Qend=TRUE
    ){

    if (exam.class == "exam") {
        my.tex.file <- paste0(
            my.tex.file,
            "\n",
            paste(qtext,collapse = "\n"),
            "\n")
    }

    my.tex.file <- paste0(
        my.tex.file,
        "\n",
        str.pattern.end.mchoice, 
        "\n",
        my.last.part)

    if ( bank) { 
        my.tex.file <- gsub(
            "\\\\begin\\{questions\\}",
            "",
            my.tex.file )
        my.tex.file <- gsub(
            "\\\\end\\{questions\\}",
            "",
            my.tex.file )
    }

    if(Qend){
        my.tex.file <- gsub(
            "\\\\end\\{choices\\}",
            "\\\\end\\{choices\\}  \\}",
            my.tex.file )
    }
    stringi::stri_write_lines(
        my.tex.file,
        fname = f.temp.tex, 
        encoding = "UTF-8")
} )

#'
#' @param i.test
#' @param f.out
#' @param n.test
#' @param n.question
#' @param bank=TRUE
#' @param Qend=TRUE
#' @param latex.dir.out
#' @param do.randomize.questions 
#' @param do.randomize.answers
#' @param list.in
#' @param l.def
#' @param verbose
#' 
#' @return
#' 
# @details
# @examples
#' @export

i.test.fun <- compiler::cmpfun( function(
    i.test,
    f.out,
    n.test,
    n.question,
    bank=TRUE,
    Qend=TRUE,
    latex.dir.out="latexOut", 
    do.randomize.questions=TRUE, 
    do.randomize.answers=TRUE,
    list.in,
    l.def,
    verbose=FALSE
    ){

    if(verbose){
        cat("\nrte: Type of exam template:", exam.class)
        cat("\nrte: Number of mchoice questions:", nrow(df.questions))
        cat("\nrte: Building Test #", i.test, "...", sep = "")
    }

    df.questions <- list.in$df.questions
    df.answers <- list.in$df.answers
    my.preamble <- list.in$my.preamble
    my.last.part <- list.in$my.last.part
    my.begin.mchoice.line <- list.in$my.begin.mchoice.line
    exam.class <- list.in$examclass
    str.pattern.correct <- l.def$str.pattern.correct
    str.pattern.choice <- l.def$str.pattern.choice
    str.pattern.end.mchoice <- l.def$str.pattern.end.mchoice
    str.pattern.end.question <- l.def$str.pattern.end.question 

    my.temp_preamble <- sub(
        x = my.preamble,
        pattern = "\\newcommand{\\myversion}{}", 
        replacement = sprintf(
            "\\newcommand{\\myversion}{%s}", 
            i.test),
        fixed = TRUE)
    f.temp.tex <- paste0(latex.dir.out,"/",f.out, i.test,".tex")

    if (do.randomize.questions) {
        my.rdn.idx.question <- sample(df.questions$n.question)
    } else {
        my.rdn.idx.question <- df.questions$n.question
    }

    my.tex.file <- paste0(my.temp_preamble, "\n", my.begin.mchoice.line)
    correct.answer.original <- character()
    correct.answer.rnd <- character()
    n.version <- c()
    n.possible.versions <- c()

    for (i.q in seq(1, n.question)) {

        q.now <- my.rdn.idx.question[i.q]
        q.text <- as.character(df.questions$main.text[df.questions$n.question == 
            q.now])
        q.answers <- df.answers[df.answers$n.question == 
            q.now, ]$text.answer
        n.answers <- length(q.answers)
        n.cases <- RndTexExams::rte.get.n.cases(
            paste(q.text, q.answers, collapse = "\n") )

        if (do.randomize.answers) {
            case.now <- sample(seq(n.cases), 1)
            my.rdn.idx.answers <- sample(seq(n.answers))
        } else {
            case.now <- 1
            my.rdn.idx.answers <- 1:n.answers
        }


        if (n.cases > 1) {
            q.answers <- q.answers.cases.fun(
            case.now= case.now,
            q.answers=q.answers,
            str.pattern.correct=str.pattern.correct,
            str.pattern.choice=str.pattern.choice)
        }

        idx.correct.original <- which(stringr::str_detect(
            q.answers, 
            stringr::fixed(str.pattern.correct)))

        out.list <- rte.The.Randomizer(
            q.text,
            q.answers, 
            case.now,
            my.rdn.idx.answers)

        full.question <- out.list$full.question
        case.now <- out.list$case.now
        idx.correct.rnd <- which(stringr::str_detect(
            out.list$q.answers.rnd, 
            stringr::fixed(str.pattern.correct)))

        correct.answer.original <- c(correct.answer.original, 
            letters[idx.correct.original])
        correct.answer.rnd <- c(correct.answer.rnd, letters[idx.correct.rnd])
        n.version <- c(n.version, case.now)
        n.possible.versions <- c(n.possible.versions, n.cases)


        my.tex.file <- paste0(
            paste0(my.tex.file, "\n", full.question),
            "\n",
            str.pattern.end.question, 
            "\n")
    }

    latexout(
        my.tex.file=my.tex.file,
        f.temp.tex=f.temp.tex,
        bank=bank,
        Qend=Qend,
        exam.class=exam.class,
        str.pattern.end.mchoice=str.pattern.end.mchoice,
        my.last.part=my.last.part,
        qtext=list.in$df.questions.not.mchoice$q.text)

    df.out <- data.frame(
        n.test = rep(i.test, n.question),
        n.question = seq(1, n.question),
        rnd.idx.questions = my.rdn.idx.question, 
        n.version = n.version,
        n.possible.versions= n.possible.versions, 
        correct.answer.original = correct.answer.original, 
        correct.answer.rnd = correct.answer.rnd)

    cat("Done")
    return( df.out )
} )

#' 
#' @param list.in
#' @param f.out
#' @param n.test
#' @param n.question
#' @param bank=TRUE
#' @param latex.dir.out="latexOut"
#' @param do.randomize.questions=TRUE
#' @param do.randomize.answers=TRUE
#' 
#' @return
#' 
# @details
# @examples
#' @export

rte.build.rdn.test <- compiler::cmpfun( function(
    list.in,
    f.out,
    n.test,
    n.question,
    bank=TRUE,
    latex.dir.out="latexOut", 
    do.randomize.questions=TRUE, 
    do.randomize.answers=TRUE) {

    if (n.question > nrow(list.in$df.questions)) {
        stop("The number of mchoice questions in test (n.question) is higher than the number of mchoice questions found in dataframe")
    }

    cat("Done")

    l.def <- RndTexExams::rte.get.classes.def(list.in$examclass)

    df.out <- lapply( seq(1, n.test),
        FUN=i.test.fun,
        f.out=f.out,
        n.test=n.test,
        n.question=n.question,
        bank=bank,
        latex.dir.out = latex.dir.out, 
        do.randomize.questions = do.randomize.questions, 
        do.randomize.answers = do.randomize.answers,
        list.in=list.in,
        l.def=l.def           
    )

    if(n.test >1){
        df.out <- do.call("rbind",df.out)
    } else {
        df.out <- df.out[[1]]
    }

    df.answer.wide <- data.table::dcast(
        data = data.table::data.table(df.out), 
        formula = n.test ~ n.question,
        fun.aggregate = function(x) return(x), 
        value.var = "correct.answer.rnd",
        fill = NA)

    answer.matrix <- as.matrix(
        as.data.frame(df.answer.wide)[, c(1+ seq(n.question))] )
    rownames(answer.matrix) <- paste("Version", seq(n.test))

    list.out <- list(
        df.answer.wide = df.answer.wide,
        answer.matrix = answer.matrix, 
        df.answer.long = df.out)

    return(list.out)
} )


#---------------------------------------------------------------------------
##################
#' Batch Latex Export
##################
#' 
#' @param lfile
#' @param latex.dir.in
#' @param latex.dir.out
#' @param n.test
#' @param do.randomize.questions
#' @param do.randomize.answers
#' 
#' @return
#' 
# @details
# @examples
#' @export

rand_fun <- compiler::cmpfun( function(
    lfile,
    latex.dir.in,
    latex.dir.out,
    n.test=1,
    do.randomize.questions=TRUE, 
    do.randomize.answers=TRUE
    ){
    
    cat("\n")
    print( lfile)

    f.in       <- paste0(latex.dir.in, lfile)
    my.test    <- rte.analyze.tex.file(f.in)
    n.question <- max(my.test$df.questions$n.question)

    f.out    <- gsub(".tex","",lfile)
    rdn.exam <- rte.build.rdn.test(
        list.in = my.test,
        f.out = f.out,
        n.test = n.test,
        n.question=n.question,
        latex.dir.out=latex.dir.out,
        do.randomize.questions=do.randomize.questions,
        do.randomize.answers=do.randomize.answers)
    return(lfile)
} )

