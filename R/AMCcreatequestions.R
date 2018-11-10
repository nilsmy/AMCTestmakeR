#' Generate AMC LaTeX question codes in the console, in a LaTeX file, or as a vector.
#'
#' @param question A character value or vector containing the questions.
#' @param correctanswers A character (value, vector) containing the correct answer. A vector (or list) of character vectors can be passed, in the case of multiple correct answers.
#' @param incorrectanswers A character (value, vector) containing the wrong answer. A vector (or list) of character vectors can be passed, in the case of multiple wrong answers.
#' @param element A character value or vector to define the category of the entire set of questions (character value) or of each question (character vector). Defaults to "general.
#' @param code A character value or vector to identify each question (note that AMC requires each code to be unique in a questionnaire). Defaults to "Q1", "Q2", "Q3", etc. (the prefix "Q" can be changed with the "codeprefix" argument).
#' @param codeprefix A character value to be used to generate automatically question codes, when not provided with the "code" argument.
#' @param append A logical value indicating if the code should be appended (append=TRUE) to an existing .tex file. Defaults to FALSE, thus overwriting the file.
#' @param filepath A character value with the file path for the .tex file to be created (defaults to "questions.tex").
#' @param multicols A numeric (or numeric vector) indicating the desired number of columns for the presentation of the correct and incorrect answers (note that the LaTeX environment multicols must be called in the main ".tex" document for more than 1 columns). Defaults to 1, which does not require the LaTeX multicols environnment.
#' @param messages A logical to indicate whether instructions should be output (defaults to TRUE).
#' @param listelements A logical to indicate whether instructions should be output (use the AMCcreateelements() function for more options).
#' @param output A character value to indicate what type of output is desired. "message" (default) outputs the questions as a message (suitable for direct copy-and-paste from console into LaTeX file). "file" outputs a questions .tex file (which can then be pointed to in the main LaTeX document). "list" and "vector" output a character vector of questions (suitable for further manipulation in R).
#' @param questiontype A character value or vector to indicate the type of all questions (character value) or of each (character vector) question. Use "single" for single-choice, and "multiple" for multiple-answer. So far open questions are not supported.
#' @param scoringcorrect A numeric value or vector to indicate the scoring for the correct answer(s). Defaults to 1.
#' @param scoringincorrect A numeric value or vector to indicate the scoring for an incorrect answer(s). Defaults to 0.
#' @param scoringnoresponse A numeric value or vector to indicate the scoring for non-responding. Defaults to 0.
#' @param scoringincoherent A numeric value or vector to indicate the scoring for incoherent answer(s) (e.g. two boxes checked for a single-answer questionnaire). Defaults to 0.
#' @param scoringbottom A numeric value or vector to indicate the minimum score for the question(s). Especially useful when attributing negative points to incorrect answers in a multiple-answer questionnaire, to ensure students do not lose too many points on one question. Defaults to 0.
#' @param shuffleanswersonce A logical value to indicate whether to shuffle answers for each question directly in the LaTeX code (useful if the answers are not randomized by examinee by AMC). Defaults to TRUE.
#'
#' @return A character value or vector (output = "list" or "vector"), a copy-and-pastable message (output = "message") or a LaTeX .tex file (output = "file") containing AMC LaTeX code for questions and answers.
#' @export
#'
#' @examples
#' #Creating a single question
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
AMCcreatequestions <- function(question,
                               correctanswers,
                               incorrectanswers,
                               element = "general",
                               code = paste(codeprefix,c(1:length(question)), sep=""),
                               codeprefix = "Q",
                               output = "message",
                               filepath = "questions.tex",
                               questiontype = "single",
                               append = F,
                               multicols=2,
                               messages = T,
                               listelements = T,
                               scoringcorrect = 1,
                               scoringincorrect = 0,
                               scoringnoresponse = 0,
                               scoringincoherent = scoringincorrect,
                               scoringbottom = scoringincorrect,
                               shuffleanswersonce = T
                               ) {



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
  codecode <- function(x, questiontype, scoringcorrect, scoringincorrect, scoringnoresponse, scoringincoherent, scoringbottom) {
    if (questiontype == "single") {
      questiontypetext <- "question"
    }
    if (questiontype == "multiple") {
      questiontypetext <- "questionmult"
    }
    ifelse(x==""| is.na(x), "Default", paste("{\\begin{",questiontypetext,"}{", x,
                                             "}\\scoring{b=",scoringcorrect,",",
                                             "m=",scoringincorrect, ",",
                                             "v=",scoringnoresponse, ",",
                                             "e=",scoringincoherent, ",",
                                             "b=", scoringbottom,
                                             "}\n", sep = ""))
  }

  # Apply that function to the list of codes
  listofcode <- mapply(x = code, questiontype= questiontype, scoringcorrect = scoringcorrect, scoringincorrect=scoringincorrect, scoringnoresponse=scoringnoresponse, scoringincoherent=scoringincoherent, scoringbottom=scoringbottom, FUN = codecode)

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
  closequestion <- function(x, questiontype) {
    if (questiontype == "single") {
      questiontypetext <- "question"
    }
    if (questiontype == "multiple") {
      questiontypetext <- "questionmult"
    }
    if (x < 2) {
      paste("\\end{choices}\\end{",questiontypetext,"}\n}\n \n", sep="")
      } else{
      paste("\\end{choices}\\end{multicols}\\end{",questiontypetext,"}\n}\n \n", sep="")}
  }


  # Apply that function to the list of questions
  vectorofclosingcode <- mapply(x = multicols, questiontype = questiontype, FUN = closequestion)

  #Return to vector
  vectorofquestion <- unlist(listofquestion)













  # Bind the code into a dataset
  if(length(question)==1){
    # For one question, create vector of all answers
    vectorofallanswersforonequestion <- c(arrayofcodedcorrectanswers, arrayofcodedincorrectanswers)

    # Shuffle them if shuffleanswersonce = TRUE
    if (shuffleanswersonce == T) {
      vectorofallanswersforonequestion <- sample(x = vectorofallanswersforonequestion,
                                                 size = length(vectorofallanswersforonequestion),
                                                 replace = F)
    }

    #Bind code
  bindedcode <- cbind(vectorofelement, vectorofcode, vectorofquestion,paste(vectorofallanswersforonequestion, collapse = " "), vectorofclosingcode)
  } else {
    # Bind together answers in array to randomize them in R
    arrayofallanswersformultiplequestions <- cbind(arrayofcodedcorrectanswers,arrayofcodedincorrectanswers)

    # Shuffle them if shuffleanswersonce = TRUE
    if (shuffleanswersonce == T) {
      shuffled <- datamatrix <- arrayofallanswersformultiplequestions
      for (i in 1:nrow(datamatrix)) { shuffled[i, ] <- sample(datamatrix[i, ]) }
      arrayofallanswersformultiplequestions <- shuffled
    }

  bindedcode <- cbind(vectorofelement, vectorofcode, vectorofquestion,arrayofallanswersformultiplequestions, vectorofclosingcode) }

  # Create list of questions
  texfile <- apply(bindedcode, 1, paste, collapse=" ")

  if (output == "message") {
    message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",
            "%%%%%%%%%| List of questions |%%%%%%%%%%\n",
            "%%% (copy & paste in main .tex file) %%%\n",
            "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n",
            paste(texfile))
  }


  #If writefile is TRUE, write to latex document with name "filepath"
  if(output=="file"){
    write(texfile, filepath, append = append)
    if (messages == T) {
      message("File successfully written to \"", paste(basename(filepath)), "\".\n",
              length(texfile), " questions created.\n\n",
              "%%%%%%%%%%%%%%%%%%%%%%\n",
              "%%%| Instructions |%%%\n",
              "%%%%%%%%%%%%%%%%%%%%%%\n",
              "%-Place the created .tex file in the AMC project folder. \n-In the main .tex file (usually, \"source.tex\") point to the created file using \"\\input{", paste(basename(filepath)), "}\".")
    }
  }
        #Append list of elements to questions file
    if (listelements == "file") {
      #create path for list of elements
      elementfilepath <- paste(dirname(filepath),"/elements.tex", sep ="")
      AMCTestmakeR::AMCcreateelements(element = element, output = "file", filepath = elementfilepath, append = F, messages = messages)
      if (messages == T) {
      message("%%%%%%%%%%%%%%%%%%%%%%")
      message("\n%-Note: The list of elements was written at ", elementfilepath)
      message("\n%-Make sure the question elements are inserted with \"\\insertgroup{element}\" after the questions. Use AMCcreateelement() function for more options).")
      }
    }


    #Show list of elements in message
    if (listelements == T) {
      AMCTestmakeR::AMCcreateelements(element = element)
      if (messages == T) {
        #message("\n%-Note: Use the function AMCcreateelements() for more options.\n")
        }
    }

  if (output == "list") {
    #return(unname(texfile))
    return(vectorofquestion)
  }

  if (output == "vector") {
    #return(unname(texfile))
    return(vectorofquestion)
  }


  #FOR TESTS
  if (output == "test") {
    #return(unname(texfile))
    return(arrayofallanswersformultiplequestions)
  }


  }


