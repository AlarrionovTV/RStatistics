#' List add
#'
#' This function adds 42 to elements of list.
#' @param listInput array of price arrays
#' @keywords cointegration
#' @export
#' @examples
#' listAdd()

listAdd = function(listInput) {
  a = as.numeric(listInput[[1]])
  b = as.numeric(listInput[[2]])
  list(a + 42, b + 42)
}