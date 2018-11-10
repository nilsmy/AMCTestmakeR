
#' Create a full Auto-Multiple-Choice test with a main .tex file (\code{groups.tex}), a file for questions (\code{questions.tex}), a file for elements (\code{elements.tex}.
#'
#' @param question A character value or vector containing the questions.
#' @param correctanswers A character (value, vector) containing the correct answer. A vector (or list) of character vectors can also be passed, in the case of multiple correct answers.
#' @param incorrectanswers A character (value, vector) containing the wrong answer. A vector (or list) of character vectors can also be passed, in the case of multiple wrong answers.
#' @param element A character value or vector to define the category of the entire set of questions (character value) or of each question (character vector). Defaults to "general.
#' @param code A character value or vector to identify each question (note that AMC requires each code to be unique in a questionnaire). Defaults to "Q1", "Q2", "Q3", etc. (the prefix "Q" can be changed with the "codeprefix" argument).
#' @param codeprefix A character value to be used to generate automatically question codes, when not provided with the "code" argument.
#' @param multicols A numeric (or numeric vector) indicating the desired number of columns for the presentation of the correct and incorrect answers (note that the LaTeX environment multicols must be called in the main ".tex" document for more than 1 columns). Defaults to 1, which does not require the LaTeX multicols environnment.
#' @param questiontype A character value or vector to indicate the type of all questions (character value) or of each (character vector) question. Use "single" for single-choice, and "multiple" for multiple-answer. So far open questions are not supported.
#' @param scoringcorrect A numeric value or vector to indicate the scoring for the correct answer(s). Defaults to 1.
#' @param scoringincorrect A numeric value or vector to indicate the scoring for an incorrect answer(s). Defaults to 0.
#' @param scoringnoresponse A numeric value or vector to indicate the scoring for non-responding. Defaults to 0.
#' @param scoringincoherent A numeric value or vector to indicate the scoring for incoherent answer(s) (e.g. two boxes checked for a single-answer questionnaire). Defaults to 0.
#' @param scoringbottom A numeric value or vector to indicate the minimum score for the question(s). Especially useful when attributing negative points to incorrect answers in a multiple-answer questionnaire, to ensure students do not lose too many points on one question. Defaults to 0.
#' @param shufflequestions A logical value or vector to indicate whether to shuffle questions inside a question group. Defaults to TRUE.
#' @param shuffleanswers A logical value or vector to indicate whether to shuffle answers per examinee. Defaults to TRUE. If set to FALSE, it is recommended to shuffle once for all examinee with shuffle the answers once with 'shuffleanswersonce = TRUE'.
#' @param shuffleanswersonce A logical value to indicate whether to shuffle answers for each question directly in the LaTeX code (useful if the answers are not randomized by examinee by AMC). Defaults to TRUE.
#' @param sections A character value or vector to indicate whether to create a new LaTeX section for each element (defaults to TRUE).
#' @param filepath A character value indicating the path for the main .tex file output. Most often, in AMC, it is \code{source.tex} (default), but in some examples it's named \code{groups.tex}, for example. Note that the other created files (\code{questions.tex} and \code{elements.tex} will we written in the folder of this file).
#' @param messages A logical value to indicate whether to output messages and reports (default is TRUE).
#' @param title A character value indicating a title for the test (default is "Test").
#' @param fontsize A numeric value to indicate the font size of the output document. Default is 10. Note: Above 12 pt, the LaTeX package "extarticle" is automatically used in lieu of "article".
#' @param instructions A logical value to add a block of preliminary instructions to the students (for example, how to fill the questionnaire). Defaults to TRUE.
#' @param paper A character value indicating what type of paper to use. Default is "letter", but "a4" can also be used.
#' @param identifier A character value indicating what to ask for to pair the exam sheets. The default is "Name", but other values like "Student ID Number" may be more appropriate.
#' @param separateanswersheet A logical value to indicate whether to use a separate answer sheet. Defaults to FALSE.
#' @param answersheettitle A character value to indicate the title of the separate answer sheet. Defaults to "Answer sheet".
#' @param answersheetinstructions A logical or character value to add default (TRUE), remove (FALSE) or customize (character value) instructions given on the separate answer sheet. Default is TRUE, which indicates that the students shall answer on the answer sheet.
#' @param twosided A logical value to indicate whether the exam will be printed two sided. This is notably important when printing on a separate answer sheet, to have the answer sheet printed on a separate page. Defaults to TRUE.
#' @param lettersinsidebox A logical value to indicate whether to put letters inside boxes. Defaults to FALSE.
#' @param box A logical value to indicate whether to box the questions and answers, to ensure that they are always presented on the same page. Defaults to TRUE.
#' @param facilitatemanualadd A logical indicating whether to add LaTeX code to facilitate adding questions and elements manually. If TRUE, creates .tex files where questions and elements can be input manually without changing the main files. Defaults to FALSE.
#'
#' @return Writes 3 .tex documents (\code{source.tex}, \code{questions.tex} and \code{elements.tex})) for direct use in Auto-Multiple-Choice.
#' @export
#'
#' @examples
#' # Create all LaTeX files
#'
#' \dontrun{
#'  AMCcreatetest(
#'  # Arguments passed to AMCcreatequestions()
#'  question = "How much is $1+1$?",
#'  correctanswers = 2,
#'  incorrectanswer = list("3", "11", "4"),
#'  # Arguments passed to AMCcreateelements()
#'  shufflequestions = T,
#'  sections = F,
#'  # Part used for test options
#'  title = "Exam", #Custom title
#'  fontsize = 11, #change fontsize
#'  identifier = "ID Number", #change identifier
#'  twosided = F, #print in one sided
#'  instructions = T, #show an instructions block to students
#'  separateanswersheet = F, #use a separate answer sheet
#'  answersheettitle = "Respond Here", #Change answer sheet title
#'  answersheetinstructions = "Fill the boxes" #Answer sheet instructions
#'   )}
#'
AMCcreatetest <- function(question,
                          correctanswers,
                          incorrectanswers,
                          element = "general",
                          code = paste(codeprefix,c(1:length(question)), sep=""),
                          codeprefix = "Q",
                          questiontype = "single",
                          multicols=2,
                          scoringcorrect = 1,
                          scoringincorrect = 0,
                          scoringnoresponse = 0,
                          scoringincoherent = scoringincorrect,
                          scoringbottom = scoringincorrect,
                          shufflequestions = T,
                          shuffleanswers = T,
                          shuffleanswersonce = T,
                          sections = T,
                          title = "Test",
                          filepath = "source.tex",
                          messages = T,
                          fontsize = 10,
                          instructions = T,
                          paper = "letter",
                          identifier = "Name",
                          separateanswersheet = F,
                          answersheettitle = "Answer sheet",
                          answersheetinstructions = T,
                          twosided = T,
                          lettersinsidebox = F,
                          box = T,
                          facilitatemanualadd = T) {

  #Name file path
  filepathname <- paste(dirname(filepath), sep="")

  if(filepathname == "."){
    filepathname <- getwd()
  }

  # shorten argument for letter paper
  if(paper == "letter") {
    paper <- "letterpaper"
  }

  # Shorten argument for a4
  if(paper == "a4") {
    paper <- "a4paper"
  }

  # Use extarticle as library for fontsize > 12 ::: next: check <10 ?
  if(fontsize > 12) {
    articlelibrary <- "extarticle"
  } else {articlelibrary <- "article"}

  # Create file paths
  filepathgroups <- filepath
  filepathquestions <- paste(dirname(filepath), "/questions.tex", sep="")
  filepathelements <- paste(dirname(filepath), "/elements.tex", sep="")


  #Create header block
  headerblocknonseparate <- c("\\namefield{\\fbox{\n",
                   "  \\begin{minipage}{.5\\linewidth}\n",
                   "  %Identifier:\n",
                   paste(identifier," :\n\n", sep =""),
                   "  \\vspace*{.5cm}	\n",
                   "  %\\dotfill	\n",
                   "  \\vspace*{1mm}	\n",
                   "  \\end{minipage}\n}}	\n")

  #Create header block
  headerblockseparate <- c("\\namefield{\\fbox{\n",
                              "  \\begin{minipage}{.5\\linewidth}\n",
                              "  %Identifier:\n",
                              paste(identifier," :\n\n", sep =""),
                              "  \\vspace*{.5cm}	\n",
                              "  %\\dotfill	\n",
                              "  \\vspace*{1mm}	\n",
                              "  \\end{minipage}\n}}	\n")



  # If instructions == T, add a priliminary block

  if(instructions == T){
      instructionblock <- c("\n",
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",
      "%% INSTRUCTIONS TO STUDENTS %%\n",
      "%%   UNCOMMENT AS NEEDED    %%\n",
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n",
      "\\section*{Instructions}	\n",
      "\n",
      "\\begin{itemize}	\n",
      "%\\item Points are \\underline{not} deduced for incorrect answers.%, and most questions are independent from one another, so try to answer all the questions, even if you hesitate.\n",
      "%\\item The total exam is graded over XX points.\n",
      "%\\item There is \\underline{always} one and \\underline{only} one correct answer.\n",
      "%\\item All the questions are presented in randomized order and are independent from each other.\n",
      "\\item \\underline{Fill} -- don't cross -- with a dark color pencil the box corresponding to what you think is the correct answer, leaving the others blank. Use an eraser to correct any mistake.\n",
      "%\\item If you think you made a mistake, circle your \\emph{entire} final answer (make your final answer as clear as you can): The exam will be both graded by computer and checked by your instructors to ensure accuracy.\n",
      "\\item Do not write or draw around or in the black circles and bar codes on the corners and top of each page.\n",
      "%\\item For short answer questions, write your answers in the answer box provided. Leave the grey part blank.\n",
      "\\end{itemize}\n\n",
      "\\hrule \\vspace{3mm}",
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n")
  } else {
    if (instructions == "all") {
      instructionblock <- c("\n",
                            "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",
                            "%% INSTRUCTIONS TO STUDENTS %%\n",
                            "%%   UNCOMMENT AS NEEDED    %%\n",
                            "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n",
                            "\\section*{Instructions}	\n",
                            "\n",
                            "\\begin{itemize}	\n",
                            "\\item Points are \\underline{not} deduced for incorrect answers.%, and most questions are independent from one another, so try to answer all the questions, even if you hesitate.\n",
                            "%\\item The total exam is graded over XX points.\n",
                            "\\item There is \\underline{always} one and \\underline{only} one correct answer.\n",
                            "\\item All the questions are presented in randomized order and are independent from each other.\n",
                            "\\item \\underline{Fill} -- don't cross -- with a dark color pencil the box corresponding to what you think is the correct answer, leaving the others blank. Use an eraser to correct any mistake.\n",
                            "\\item If you think you made a mistake, circle your \\emph{entire} final answer (make your final answer as clear as you can): The exam will be both graded by computer and checked by your instructors to ensure accuracy.\n",
                            "\\item Do not write or draw around or in the black circles and bar codes on the corners and top of each page.\n",
                            "\\item For short answer questions, write your answers in the answer box provided. Leave the grey part blank.\n",
                            "\\end{itemize}\n\n",
                            "\\hrule \\vspace{3mm}",
                            "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n")
    } else {
        if (instructions == F) {
           instructionblock <- ""
      } else {
        instructionblock <- instructions
      }
    }

  }




  if(separateanswersheet == F) {
    #Separate answers
    separateanswer1 <- ""
    clearpagetext <- "\\clearpage"
    answersheettext <- ""
  }

  if(separateanswersheet == T) {
    #Separate answers
    separateanswer1 <- ",separateanswersheet"
      if (twosided == T) {
        clearpagetext <- "\\AMCcleardoublepage" ## Good if recto-verso
      } else {
        clearpagetext <- "\\clearpage" ## Good if recto-verso
      }

    #Auto add answer sheet instructions
      if (answersheetinstructions == TRUE) {
        answersheetinstructionstext <- c("\\bf\\em Answers must be given exclusively on this sheet:\n",
                                     "Answers given on the other sheets will be ignored.\n")
      } else {
        if (answersheetinstructions == FALSE) {
          answersheetinstructionstext <- ""
        } else {
          answersheetinstructionstext <- answersheetinstructions
        }
      }  #Remove answer sheet instructions


    #Define Answersheet header
    answersheettext <- c("\n \\AMCformBegin \n",
                         "%%% Answer sheet header %%%\n",
                         "{\\large\\bf ", answersheettitle,":}\n",
                         "\\hfill ", headerblockseparate,
                         "\\begin{center}\n",
                         answersheetinstructionstext,
                         "\\end{center}\n",
                         "%%% End of answer sheet header %%%\n",
                         "\\AMCform"
                         )

  }




  # OPTION BOX
  if (box == T) {
    useboxpackage <- "box,"
  } else {
    useboxpackage <- ""
  }

  # OPTION letters inside box
  if (lettersinsidebox == T) {
    lettersinsideboxcode <- "insidebox,"
  } else {
    lettersinsideboxcode <- ""
  }

  # OPTION no shuffle
  if (shuffleanswers == T) {
    shuffleanswerscode <- "noshuffle,"
  } else {
    shuffleanswerscode <- ""
  }


  # OPTION facilitate manual add
  if (facilitatemanualadd == T) {
    manualaddquestionscode <- "\n %Manually add questions in the separate file	\n \\input{manuallyaddedquestions.tex}\n"
    manualaddelementscode <- "\n %Manually add question groups (elements) in the separate file	\n \\input{manuallyaddedelements.tex}\n"
    facilitatemanualaddmessage <- c("\n- manuallyaddedquestions.tex (manually add questions here)","\n- manuallyaddedelements.tex (manually add questions here)")
    # Write the additional files for questions
    filemanuallyaddedquestions <- paste("%Place your additional questions below (or leave blank but keep the file)\n",
                                        "%Note : AMCTestmakeR is set to NOT erase your additional questions if you rerun it.\n",
                                        sep ="")
    filepathmanuallyaddedquestions <- paste(dirname(filepath), "/manuallyaddedquestions.tex", sep="")
    write(filemanuallyaddedquestions, filepathmanuallyaddedquestions, append = T)
    # Write the additional files for elements
    filemanuallyaddedelements <- paste("%Place your additional question groups (elements) below (or leave blank but keep the file)\n",
                                       "%Note : AMCTestmakeR is set to NOT erase your additional questions if you rerun it.\n",
                                        sep ="")
    filepathmanuallyaddedelements <- paste(dirname(filepath), "/manuallyaddedelements.tex", sep="")
    write(filemanuallyaddedelements, filepathmanuallyaddedelements, append = T)
  } else {
    manualaddquestionscode <- ""
    manualaddelementscode <- ""
    facilitatemanualaddmessage <- ""

  }


  listoriginaltex <- c(paste("\\documentclass[",paper,",",fontsize,"pt]{",articlelibrary,"}	\n", sep =""),
                       "\n",
                       "\\usepackage{multicol}	\n",
                       "\\usepackage[utf8x]{inputenc}	\n",
                       "\\usepackage[T1]{fontenc}	\n",
                       "\\usepackage{amsmath}	\n",
                       "\\usepackage[",useboxpackage,lettersinsideboxcode, shuffleanswerscode,
                       "completemulti",separateanswer1,"]{automultiplechoice}	\n",
                       "\n",
                       "\\renewcommand{\\rmdefault}{\\sfdefault}	\n",
                       "\n",
                       "%Tweak margins here if desired",
                       "%\\geometry{hmargin=3cm,headheight=2cm,headsep=.3cm,footskip=1cm,top=3.5cm,bottom=2.5cm}\n",
                       "\n",
                       "\\usepackage{titlesec}	\n",
                       "\n",
                       "%Format section titles with horizontal lines	\n",
                       "\\titleformat{\\section}	\n",
                       "{\\hrule\\center\\normalfont\\normalsize\\bfseries}{\\thesection.}{1em}{}[{\\vspace{1mm}\\hrule}]	\n",
                       "\n",
                       "\\renewcommand{\\thesection}{\\Alph{section}} 	\n",
                       "\\renewcommand{\\thesubsection}{\\thesection.\\Roman{subsection}}	\n",
                       "\n",
                       "\\AMCrandomseed{1527384}	\n",
                       "\n",
                       "%Takes the questions from the questions.tex file\n",
                       "\\input{questions.tex}",manualaddquestionscode,
                       "\n",
                       "\\begin{document}	\n",
                       "\n",
                       "%Vertical space between answers	\n",
                       "%\\AMCinterBrep=.2ex	\n",
                       "\n",
                       "\\baremeDefautS{mz=1}	\n",
                       "\n",
                       "\\onecopy{10}{	\n",
                       "\n",
                       "\\vspace*{.5cm}	\n",
                       "\\begin{minipage}{.4\\linewidth}	\n",
                       "\\centering	\n",
                       "%Uncomment to insert logo image	\n",
                       "%\\includegraphics[height=2cm,width=4cm,keepaspectratio]{logo.png}             	\n",
                       "\n",
                       "%Title	\n",
                       "\\large\\bf ", title ," \\vspace*{1mm}	\n",
                       "\\end{minipage}\n",
                       headerblocknonseparate,
                       "\n",
                       instructionblock,
                       "\n",
                       "\n",
                       "%Takes the elements list from the elements.tex file\n",
                       "\\input{elements.tex}",
                       manualaddelementscode,
                       "\n",
                       clearpagetext," \n",
                       "\n",
                       answersheettext,
                       "\n",
                       "}\n",
                       "% This test was created with AMCTestmakeR for R.\n\n\n",
                       "\\end{document}	\n")


collapsedlist <- paste(listoriginaltex, sep = "", collapse = "")


  # Write the groups.tex
  write(collapsedlist, filepathgroups, append = F)

  # Create list of questions through AMCcreatequestions
  AMCcreatequestions(question = question,
                     correctanswers = correctanswers,
                     incorrectanswers = incorrectanswers,
                     element = element,
                     code = code,
                     codeprefix = codeprefix,
                     output = "file",
                     filepath = filepathquestions,
                     questiontype = questiontype,
                     append = F,
                     multicols = multicols,
                     messages = F,
                     listelements = F,
                     scoringcorrect = scoringcorrect,
                     scoringincorrect = scoringincorrect,
                     scoringnoresponse = scoringnoresponse,
                     scoringincoherent = scoringincoherent,
                     scoringbottom = scoringbottom,
                     shuffleanswersonce = shuffleanswersonce
                     )

  # Create list of elements through AMCcreatelements
  #(verbose but more options)
  AMCcreateelements(element = element,
                    shufflequestions = shufflequestions,
                    sections = sections,
                    output = "file",
                    append = F,
                    messages = F,
                    filepath = filepathelements)




# Report message
if (messages == T){
message("The following files were successfully written to ",
        filepathname,
        ":\n- ",
        basename(filepath),
        "\n- questions.tex",
        "\n- elements.tex",
        #Adds additional messages
        facilitatemanualaddmessage,
        "\n\nPut all these files in your AMC project folder and use AMC to compile them."
        )

}




}

