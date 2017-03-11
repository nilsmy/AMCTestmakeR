#' AMCcreatequestions
#'
#' @param question A character value or vector containing the questions.
#' @param correctanswers A character (value, vector) containing the correct answers. A vector (or list) of character vectors can also be passed, in the case of multiple correct answers.
#' @param incorrectanswers A character (value, vector) containing the wrong answers. A vector (or list) of character vectors can also be passed, in the case of multiple wrong answers.
#' @param element A character value or vector to define the category of the entire set of questions (character value) or of each question (character vector). Defaults to "general.
#' @param code A character value or vector to identify each question (note that AMC requires each code to be unique in a questionnaire). Defaults to "Q1", "Q2", "Q3", etc. (the prefix "Q" can be changed with the "codeprefix" argument).
#' @param codeprefix A character value to be used to generate automatically question codes, when not provided with the "code" argument.
#' @param writefile A logical value indicating whether a .tex file should be created.
#' @param filepath A character value containing the file path for the .tex file to be created (defaults to "questions.tex").
#' @param multicols A numeric (or numeric vector) indicating the desired number of columns for the presentation of the correct and incorrect answers (note that the package multicols must be used in the final ".tex" document). Defaults to 1, which does not require the LaTeX multicols package.
#'
#' @return
#' @export
#'
#' @examples
#' #Creating a simple question:
#'
#' AMCcreatequestions("general",code,question,goodanswer1,list(wronganswer1, wronganswer2, wronganswer3, wronganswer4), multicols = multicols)
#'
AMCcreatequestions <- function(question, correctanswers, incorrectanswers, element = "general", code = paste(codeprefix,c(1:length(question)), sep=""), codeprefix = "Q", writefile = F, filepath = "questions.tex", multicols=1) {

  #ELEMENT

  # Create function that wraps "element" and initial code in LaTeX-AMC code
  codeelement <- function(x) {
    ifelse(x==""| is.na(x), "Default", paste("\\element{", x, "}\n", sep = ""))
  }

  # Apply that function to the list of elements
  listofelement <- lapply(X = element, FUN = codeelement)

  #Return to vector
  vectorofelement <- unlist(listofelement)

  #CODE

  # Create function that wraps "code" in LaTeX-AMC code
  codecode <- function(x) {
    ifelse(x==""| is.na(x), "Default", paste("{\\begin{question}{", x, "}\n", sep = ""))
  }

  # Apply that function to the list of codes
  listofcode <- lapply(X = code, FUN = codecode)

  #Return to vector
  vectorofcode <- unlist(listofcode)


  #QUESTION

  # Create function that wraps "question" in LaTeX-AMC code
  codequestion <- function(x, multicols) {
    if(multicols > 1){
      ifelse(x==""| is.na(x), "Default", paste(x, "\n \\begin{multicols}{",multicols,"}\\AMCBoxedAnswers\\begin{choices}\n", sep = ""))
    } else{
      ifelse(x==""| is.na(x), "Default", paste(x, "\n \\AMCBoxedAnswers\\begin{choices}\n", sep = ""))
    }
  }

  # Apply that function to the list of questions
  listofquestion <- mapply(x = question, multicols = multicols, FUN = codequestion)

  #Return to vector
  vectorofquestion <- unlist(listofquestion)



  # Create function that wraps good answers (if non empty) in LaTeX-AMC code
  codegoodanswer <- function(x) {
    ifelse(x==""| is.na(x), "", paste("\\correctchoice{", x, "}\n", sep = ""))
  }
  # Apply that function to the list of good answers
  arrayofcodedcorrectanswers <- sapply(FUN = codegoodanswer, X = correctanswers, simplify = "array")

  # Create function that wraps wrong answers (if non empty) in LaTeX-AMC code
  codewronganswer <- function(x) {
    ifelse(x==""| is.na(x), "", paste("\\wrongchoice{", x, "}\n", sep = ""))
  }




  # Apply that function to the list of wrong answers
  arrayofcodedincorrectanswers <- sapply(FUN = codewronganswer, X = incorrectanswers, simplify = "array")


  # #Create a vector to adds the closing code (different if multicols)
  # if(multicols < 2 ){
  #   vectorofclosingcode <- rep("\\end{choices}\\end{question}\n}\n \n", length(question))
  # }
  # else{
  #   vectorofclosingcode <- rep("\\end{choices}\\end{multicols}\\end{question}\n}\n \n", length(question))}
  #
  #

  # Create function that creates the closing code
  closequestion <- function(x) {
    if(x > 1){
      "\\end{choices}\\end{question}\n}\n \n"
      } else{
      "\\end{choices}\\end{multicols}\\end{question}\n}\n \n"}
  }


  # Apply that function to the list of questions
  vectorofclosingcode <- lapply(X = multicols, FUN = closequestion)

  #Return to vector
  vectorofquestion <- unlist(listofquestion)





  # Bind the code into a dataset
  bindedcode <- cbind(vectorofelement, vectorofcode, vectorofquestion,arrayofcodedcorrectanswers,arrayofcodedincorrectanswers, vectorofclosingcode)

  # Create list of questions
  texfile <- apply(bindedcode, 1, paste, collapse=" ")

  #If writefile is TRUE, write to latex document with name "filepath"
  if(writefile==T){
    write(texfile, filepath)
  } else {
    return(texfile)
  }

}
