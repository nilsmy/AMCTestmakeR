#' Generates AMC LaTeX question codes
#'
#' @param question A character value or vector containing the questions.
#' @param correctanswers A character (value, vector) containing the correct answers. A vector (or list) of character vectors can also be passed, in the case of multiple correct answers.
#' @param incorrectanswers A character (value, vector) containing the wrong answers. A vector (or list) of character vectors can also be passed, in the case of multiple wrong answers.
#' @param element A character value or vector to define the category of the entire set of questions (character value) or of each question (character vector). Defaults to "general.
#' @param code A character value or vector to identify each question (note that AMC requires each code to be unique in a questionnaire). Defaults to "Q1", "Q2", "Q3", etc. (the prefix "Q" can be changed with the "codeprefix" argument).
#' @param codeprefix A character value to be used to generate automatically question codes, when not provided with the "code" argument.
#' @param writefile A logical value indicating whether a .tex file should be created.
#' @param append A logical value indicating if the code should be appended (append=TRUE) to an existing .tex file. Defaults to FALSE, thus overwriting the file.
#' @param filepath A character value containing the file path for the .tex file to be created (defaults to "questions.tex").
#' @param multicols A numeric (or numeric vector) indicating the desired number of columns for the presentation of the correct and incorrect answers (note that the LaTeX environment multicols must be called in the main ".tex" document for more than 1 columns). Defaults to 1, which does not require the LaTeX multicols environnment.
#' @param messages A logical to indicate whether instructions should be output (defaults to TRUE).
#'
#' @return A character value or vector containing AMC LaTeX code for questions and answers.
#' @export
#'
#' @examples
#' #Creating a single question:
#'
#' AMCcreatequestions("How much is $1+1$?",2,list("3", "11"))
#'
#' #Presenting answers in multiple columns (LaTeX environment 'multicols' is used)
#'
#' AMCcreatequestions("How much is $1+1$?",2,list("3","11"),multicols = 2)
#'
#' #Creating an entire questionnaire from a dataset of questions
#' ## Defining the questions (don't forget to escape R special characters)
#' question <- c("How much is $1+1$ ?", "How much is $1 \\times 1$ ?",
#'   "How much is $\\frac{1}{2}$ ?")
#'   correct <- c(2,1,0.5)
#'   incorrect1 <- c(3,4,10)
#'   incorrect2 <- c(1,3,100)
#'   incorrect3 <- c(4,8,NA) #Empty values (NA and "") are skipped
#'
#' ## Generating the AMC LaTeX code
#' AMCcreatequestions(
#'   question = question,
#'   correctanswers = correct,
#'   incorrectanswers = list(incorrect1,incorrect2,incorrect3))
#'
#' #Changing the code prefix from "Q" to "MATH"
#'
#' AMCcreatequestions(
#'   question = question,
#'   correctanswers = correct,
#'   incorrectanswers = list(incorrect1,incorrect2,incorrect3),
#'   codeprefix = "MATH")
#'
AMCcreatequestions <- function(question, correctanswers, incorrectanswers, element = "general", code = paste(codeprefix,c(1:length(question)), sep=""), codeprefix = "Q", writefile = F, filepath = "questions.tex", append = F, multicols=1, messages = T, listelements = T) {

  #ELEMENT

  # Create function that wraps "element" and initial code in LaTeX-AMC code
  codeelement <- function(x) {
    ifelse(x==""| is.na(x), "Default", paste("\\element{", x, "}\n", sep = ""))
  }

  # Apply that function to the list of elements
  listofelement <- lapply(X = element, FUN = codeelement)

  #Return to vector
  vectorofelement <- unlist(listofelement)

  #Get list of unique elements (used later)
  #uniqueelements <- unique(vectorofelement)

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
    if(multicols > 1.1){
      paste(x, "\n \\begin{multicols}{",multicols,"}\\AMCBoxedAnswers\\begin{choices}\n", sep = "")
    } else{
      paste(x, "\n \\AMCBoxedAnswers\\begin{choices}\n", sep = "") }
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
    if(x < 2){
      "\\end{choices}\\end{question}\n}\n \n"
      } else{
      "\\end{choices}\\end{multicols}\\end{question}\n}\n \n"}
  }


  # Apply that function to the list of questions
  vectorofclosingcode <- lapply(X = multicols, FUN = closequestion)

  #Return to vector
  vectorofquestion <- unlist(listofquestion)





  # Bind the code into a dataset
  if(length(question)==1){
  bindedcode <- cbind(vectorofelement, vectorofcode, vectorofquestion,paste(arrayofcodedcorrectanswers, collapse = " "),paste(arrayofcodedincorrectanswers, collapse = " "), vectorofclosingcode)
  } else {
  bindedcode <- cbind(vectorofelement, vectorofcode, vectorofquestion,arrayofcodedcorrectanswers,arrayofcodedincorrectanswers, vectorofclosingcode) }

  # Create list of questions
  texfile <- apply(bindedcode, 1, paste, collapse=" ")

  #If writefile is TRUE, write to latex document with name "filepath"
  if(writefile==T){
    if (messages == T) {
      message("File written to \"", paste(basename(filepath)), "\".")
      message(length(texfile), " questions created.")
      message("\n===| Instructions |===")
      message("-Make sure that the created file is the main AMC project folder. \n-Point to the written file in the main .tex file (\"groups.tex\"), using \"\\input{", paste(basename(filepath)), "}\". \n-Insert the question elements using \"\\insertgroup{element}\" in the main .tex file. \n-Use (\"\\shufflegroup{element}\") before it if you want to shuffle the questions order.")
    }
    if (listelements == T) {
      message("\n===| Elements |===")
      message("To insert the elements (question groups), use:")
      message()
    }
    write(texfile, filepath, append = append)
  } else {
    return(unname(texfile))
  }

}
