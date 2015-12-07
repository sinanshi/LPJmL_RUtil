#' This function maybe useless, we can use read.csv(comment.char) instead
getCommentsLines <- function(file, comment_type = "!"){
    # assume that lines of comment exceed 300 lines in not circumstance.
    first_lines <- readLines(file, n=300)
    comment_line <- grep(paste("^", comment_type, sep = ""), first_lines)
    if (length(comment_line) == 0)
        return(0)
    else{
        if (comment_line[1] != 1){
            stop("Error: comments should start from the first line of the
                                  file.", comment_line)
        }
        if (!identical(comment_line,
                                              seq(min(comment_line),
                                                  max(comment_line)))){
            stop("Error: comment lines are not appearing contigious from
                                  beginning of the file.")
        }
        return(max(comment_line))
    }
}
