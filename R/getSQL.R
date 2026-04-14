#' Read a SQL File into a Single String
#'
#' Reads a SQL script from disk and returns it as a single character string,
#' converting line-end `--` comments to block comments so that line endings
#' inside multi-line strings do not truncate the query.
#'
#' @param filepath Path to the `.sql` file to read.
#'
#' @return A character string containing the full SQL query.
#'
#' @export
getSQL <- function(filepath) {
  con <- file(filepath, "r")
  sql.string <- ""

  while (TRUE) {
    line <- readLines(con, n = 1, encoding = "UTF-16")

    if (length(line) == 0) {
      break
    }

    line <- gsub("\\t", " ", line)

    if (grepl("--", line) == TRUE) {
      line <- paste(sub("--", "/*", line), "*/")
    }

    sql.string <- paste(sql.string, line)
  }

  close(con)
  return(sql.string)
}
