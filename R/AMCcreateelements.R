#' Title
#'
#' @param element
#' @param shuffle
#'
#' @return
#' @export
#'
#' @examples
AMCcreateelements <- function(element = "general", shuffle = T, sections = F) {


  #ELEMENT

 # if (shuffle == T) {
#    codeelement <- function(x) {
#      ifelse(x==""| is.na(x), "Default", paste("\\insertgroup{", x, "}\n",
  #                                             sep = "")) } else

  # Create function that wraps "element" and initial code in LaTeX-AMC code
  codeelement <- function(x) {
    if (shuffle == T) {
      paste("\\shufflegroup{", x, "}\n", "\\insertgroup{", x, "}\n", sep = "")
    } else {
      ifelse(x==""| is.na(x), "Default", paste("\\insertgroup{", x, "}\n",
                                               sep = ""))
    }

  }

  # Apply that function to the list of elements
  listofelement <- lapply(X = element, FUN = codeelement)

  #Return to vector
  vectorofelement <- unlist(listofelement)

  #return(listofelement)
  uniqueelements <- unique(vectorofelement)

  message("===| Copy and paste in main .tex file  |===")
  message(paste(uniqueelements))

}
