#' A Random Lorem Ipsum
#'
#' @param nchars number of characters. One of the two params should be left NULL.
#' @param nwords number of words to return. One of the two params should be left NULL.
#'
#' @importFrom attempt stop_if_all
#'
#' @return a text
#'
#' @export
#' @examples
#' random_text(nchars = 100)
#' random_text(nwords = 100)
random_text <- function(nchars = NULL, nwords = NULL){
  stop_if_all(
    c(nchars, nwords),
    is.null,
    "Please enter a nchars or nwords"
  )
  stop_if_all(
    list(nchars, nwords),
    ~ !is.null(.x),
    "You can't chose both nchars and nwords"
  )

  if (!is.null(nchars)){
    res <- substr(
      shinipsum::lorem,
      1,
      nchars
    )
  } else {
    res <- paste(shinipsum::lorem_words[1:nwords], collapse = " ")
  }
  res
}

