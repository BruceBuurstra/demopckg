#' report_p
#'
#' `report_p` reports p-values in APA style
#'
#' @param p A value to return
#' @return Returns the p-value \code{p} in APA style
#' @examples
#'
#' report_p(1) # returns 1000
#'
#' @export

report_p <- function(p, digits = 3) {
  if (p < 0) stop("p cannot be less than 0")
  if (p > 1) stop("p cannot be greater than 1")
  if (p < .001) return("p < .001")

  p_round <- round(p, digits) %>%
    as.character() %>%
    # omit leading zero for APA-style
    stringr::str_replace("0.", ".") %>%
    # pad right with zeros
    stringr::str_pad(digits+1, "right", 0)

  p_string <- paste("p =", p_round)

  return(p_string)
}

report_p(1)
