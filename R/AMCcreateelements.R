#' Generate AMC LaTeX question group ("element") codes
#'
#' @param element A character value or vector of question groups ("elements") to generate input code for. Default is "general".
#' @param shufflequestions A logical value or vector to indicate whether to shuffle questions inside a question group. Defaults to TRUE.
#' @param sections A character value or vector to indicate whether to create a new LaTeX section for each element (defaults to TRUE).
#' @param output A character value to indicate how to output the LaTeX commands. Use "message" (default) to get a console message that can be directly copy-and-pasted to the LaTeX mais file. Use "list" to get a list object. Use "file" to output to a .tex file (the path can be changed with the "filepath" command).
#' Defaults to "message
#' @param filepath A character value with the file path for the .tex file to be created (defaults to "elements.tex").
#' @param append A logical value indicating if the code should be appended (append=TRUE) to an existing .tex file. Defaults to FALSE, thus overwriting the file.
#' @param messages A logical to indicate whether instructions should be output (defaults to TRUE).
#'
#' @return Commands to add the question groups in AMC-LaTeX code.
#' @export
#'
#' @examples #To output a message (not visible in documentation)
#' AMCcreateelements(c(1:4))
#'
#' #To output a list
#' AMCcreateelements(c(1:4), output = "list")
#'
#' #Duplicates are automatically removed
#' AMCcreateelements(rep(1:3, 5), output = "list")
#'
#' #To cancel shuffling
#' AMCcreateelements(c(1:4), output = "list", shufflequestions = FALSE)
#'
#' #To remove sections at each element
#' AMCcreateelements(c(1:4), sections = FALSE, output = "list")
#'
#' #To add sections for only last element
#' AMCcreateelements(c(1:4),
#' sections = c(FALSE,FALSE,FALSE,TRUE),
#' output = "list")
AMCcreateelements <- function(element = "general", shufflequestions = TRUE, sections = TRUE, output = "message", filepath = "elements.tex", append = FALSE, messages = TRUE) {


  #Remove duplicate elements
  element <- unique(element)


  # Create function that wraps "element" and initial code in LaTeX-AMC code
  codeelement <- function(x, shufflequestions, sections) {
    if (shufflequestions == T) {
      if (sections == T) {
        paste("\\section*{", x, "}\n","\\shufflegroup{", x, "}\n", "\\insertgroup{", x, "}\n", sep = "")
    } else {
        paste("\\shufflegroup{", x, "}\n", "\\insertgroup{", x, "}\n", sep = "")
      }
    } else {
      if (sections == T) {
       ifelse(x==""| is.na(x), "Default", paste("\\section*{",x,"}\n","\\insertgroup{", x, "}\n", sep = ""))
    } else {
       ifelse(x==""| is.na(x), "Default", paste("\\insertgroup{", x, "}\n", sep = ""))
      }
    }

  }

  # Apply that function to the list of elements
  listofelement <- mapply(x = element, shufflequestions = shufflequestions, sections = sections, FUN = codeelement, SIMPLIFY = T)

  #Return to vector
  vectorofelement <- unlist(listofelement)

  #return(listofelement)
  uniqueelements <- vectorofelement

  #
  if (output == "message") {
    message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",
            "%%%%%%%%%| List of elements |%%%%%%%%%\n",
            "%%% (copy & paste after questions) %%%\n",
            "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",
            paste(uniqueelements)
            )
  } else if(output == "list"){
    return(uniqueelements)
  } else if (output == "file"){
    if (messages == TRUE){
    message("File written to \"", paste(basename(filepath)), "\".\n",
            "%%%%%%%%%%%%%%%%%%%%%%\n",
            "%%%| Instructions |%%%\n",
            "%%%%%%%%%%%%%%%%%%%%%%\n",
            "%-Make sure that the created file is the main AMC project folder.",
            "\n-Point to the written file in the main .tex file (usually \"source.tex\"), using \"\\input{",
            paste(basename(filepath)), "}\". \n",
            "-%Note : Ultimately, the questions should be defined first before compiling (use the AMCcreatequestions() function for this).")}
    write(uniqueelements, filepath, append = append)
  }


}
