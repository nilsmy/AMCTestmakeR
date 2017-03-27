
#' Create a full Auto-Multiple-Choice test with a file for questions,
#'
#' @param ... Arguments passed to AMCcreatequestions (see documentation).
#' @param filepath A character value indicating the path for the main .tex file output (most often, in AMC, it is "groups.tex", which is the default of the function). Note that the other created files ("questions.tex" and "elements.tex" will we written in the folder of this file).
#' @param messages A logical value to indicate whether to output messages and reports (default is TRUE).
#' @param title A character value indicating a title for the test (default is "Test").
#' @param output A character value indicating what the function should return. "file" (default) writes files, "questions" return questions as a message.
#' @param fontsize A numeric value to indicate the font size of the output document. Default is 10. Note: Above 12 pt, the LaTeX package "extarticle" is automatically used in lieu of "article".
#' @param instructions A logical value to add a block of preliminary instructions to the students (for example, how to fill the questionnaire). Defaults to TRUE.
#' @param paper A character value indicating what type of paper to use. Default is "letter", but "a4" can also be used.
#' @param identifier A character value indicating what to ask for to pair the exam sheets. The default is "Name", but other values like "Student ID Number" may be more appropriate.
#'
#' @return A .tex document
#' @export
#'
#' @examples
AMCcreatetest <- function(..., title = "Test", filepath = "groups.tex", messages = T, output = "file", fontsize = 10, instructions = T, paper = "letter", identifier = "Name") {


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

  # Use extarticle as library for fontsize > 12
  if(fontsize > 12) {
    articlelibrary <- "extarticle"
  } else {articlelibrary <- "article"}

  if(fontsize < 9) {
    articlelibrary <- "extarticle"
  } else {articlelibrary <- "article"}


  # Create file paths
  filepathgroups <- filepath
  filepathquestions <- paste(dirname(filepath), "/questions.tex", sep="")

  # If instructions == T, add a priliminary block
  instructionblock <- ""
  if(instructions == T){
  instructionblock <- c("\n",
  "%%% INSTRUCTIONS TO STUDENTS, UNCOMMENT AS NEEDED	\n",
  "\\section*{Preliminary notes}	\n",
  "\n",
  "\\begin{itemize}	\n",
  "%  \\item Points are \\underline{not} deduced for incorrect answers.%, and most questions are independent from one another, so try to answer all the questions, even if you hesitate.	\n",
  "%  \\item The total exam is graded over XX points.	\n",
  "% \\item There is \\underline{always} one and \\underline{only} one correct answer.	\n",
  "% \\item All the questions are presented in randomized order and are independent from each other.	\n",
  "% \\item \\underline{Fill} -- don't cross -- with a dark color pencil the box corresponding to what you think is the correct answer, leaving the others blank. Use an eraser to correct any mistake.	\n",
  "%    If you think you made a mistake, circle your \\emph{entire} final answer (make your final answer as clear as you can): The exam will be both graded by computer and checked by your instructors to ensure accuracy.	\n",
  "\\item Do not write or draw around or in the black circles and bar codes on the corners and top of each page.	\n",
  "%        \\item For short answer questions, write your answers in the answer box provided. Leave the grey part blank.	\n",
  "\\end{itemize}	\n",
  "%%%%%%%%%%%%%%%%%%%%%%%	\n",
  "\n")
  }


  listoriginaltex <- c(paste("\\documentclass[",paper,",",fontsize,"pt]{",articlelibrary,"}	\n", sep =""),
                       "\n",
                       "\\usepackage{multicol}	\n",
                       "\\usepackage[utf8x]{inputenc}	\n",
                       "\\usepackage[T1]{fontenc}	\n",
                       "\\usepackage{amsmath}	\n",
                       "\\usepackage[box,completemulti]{automultiplechoice}	\n",
                       "\n",
                       "\\renewcommand{\\rmdefault}{\\sfdefault}	\n",
                       "\n",
                       "%\\geometry{hmargin=2.5cm,headheight=1.5cm,headsep=.2cm,footskip=0.8cm,top=2.5cm,bottom=2.5cm}\n",
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
                       "\\namefield{\\fbox{\\begin{minipage}{.5\\linewidth}	\n",
                       "%Identifier:\n",
                       paste(identifier," :\n", sep =""),
                       "\\vspace*{.5cm}	\n",
                       "%\\dotfill	\n",
                       "\\vspace*{1mm}	\n",
                       "\\end{minipage}}}	\n",
                       "\n",
                       instructionblock,
                       "\n",
                       "%Take the questions from the questions.tex file\n",
                       "\\input{questions.tex}",
                       "\n",
                       "%Take the elements list from the elements.tex file\n",
                       "\\input{elements.tex}",
                       "\n",
                       "\\clearpage	\n",
                       "}\n",
                       "\n",
                       "\\end{document}	\n")


collapsedlist <- paste(listoriginaltex, sep = "", collapse = "")

if(output == "file"){
  # Create list of questions and elements through AMCcreatequestions
  AMCcreatequestions(output = "file", listelements = "file", filepath = filepathquestions, messages = F, append = F, ...)
  # Write the groups.tex
  write(collapsedlist, filepathgroups, append = F)
# Report message
if (messages == T){
message("All files (", basename(filepath),", questions.tex, elements.tex) were successfully written to ",filepathname, ".")
}
}



}




