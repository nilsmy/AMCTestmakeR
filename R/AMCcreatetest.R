
#' Title
#'
#' @param title
#'
#' @return
#' @export
#'
#' @examples
AMCcreatetest <- function(title = "Test") {


  originaltex <- "\\documentclass[letterpaper,10pt]{article}	\n
%To create version with more than 12pt text	\n
  %\\documentclass[letterpaper,14pt]{extarticle}	\n
  \n
  \\usepackage{multicol}	\n
  \\usepackage[utf8x]{inputenc}	\n
  \\usepackage[T1]{fontenc}	\n
  \\usepackage{amsmath}	\n
  \n
  \\usepackage[box,completemulti]{automultiplechoice}	\n
  \n
  \n
  \\renewcommand{\\rmdefault}{\\sfdefault}	\n
  \n
  %\\geometry{hmargin=2.5cm,headheight=1.5cm,headsep=.2cm,footskip=0.8cm,top=2.5cm,bottom=2.5cm}	\n
  \n
  \\usepackage{titlesec}	\n
  \n
  % Format section titles with horizontal lines	\n
  \\titleformat{\\section}	\n
  {\\hrule\\center\\normalfont\\normalsize\\bfseries}{\\thesection.}{1em}{}[{\\vspace{1mm}\\hrule}]	\n
  \n
  \n
  \\renewcommand{\\thesection}{\\Alph{section}} 	\n
  \\renewcommand{\\thesubsection}{\\thesection.\\Roman{subsection}}	\n
  \n
  \\AMCrandomseed{1527384}	\n
  \n
  \n
  \\begin{document}	\n
  \n
  %Vertical space between answers	\n
  %\\AMCinterBrep=.2ex	\n
  \n
  \n
  \\baremeDefautS{mz=1}	\n
  \n
  % Point here to the question file(s)	\n
  %\\input{questions.tex}	\n
  \n
  \\onecopy{10}{	\n
  \n
  \\vspace*{.5cm}	\n
  \\begin{minipage}{.4\\linewidth}	\n
  \\centering	\n
  % Uncomment to insert logo image	\n
  %\\includegraphics[height=2cm,width=4cm,keepaspectratio]{logo.png}             	\n
  \n
  % Title	\n
  \\large\\bf Title of the exam \\vspace*{1mm}	\n
  \\end{minipage}	\n
  \\namefield{\\fbox{\\begin{minipage}{.5\\linewidth}	\n
  % Identifier (change to name if needed)	\n
  %ID Number:	\n
  Name:	\n
  \\vspace*{.5cm}	\n
  %\\dotfill	\n
  \\vspace*{1mm}	\n
  \\end{minipage}}}	\n
  \n
  \n
  %%% INSTRUCTIONS TO STUDENTS, UNCOMMENT AS NEEDED	\n
  \n
  \\section*{Preliminary notes}	\n
  \n
  \\begin{itemize}	\n
  %  \\item Points are \\underline{not} deduced for incorrect answers.%, and most questions are independent from one another, so try to answer all the questions, even if you hesitate.	\n
  %  \\item The total exam is graded over XX points.	\n
  % \\item There is \\underline{always} one and \\underline{only} one correct answer.	\n
  % \\item All the questions are presented in randomized order and are independent from each other.	\n
  % \\item \\underline{Fill} -- don't cross -- with a dark color pencil the box corresponding to what you think is the correct answer, leaving the others blank. Use an eraser to correct any mistake.	\n
  %    If you think you made a mistake, circle your \\emph{entire} final answer (make your final answer as clear as you can): The exam will be both graded by computer and checked by your instructors to ensure accuracy.	\n
  \\item Do not write or draw around or in the black circles and bar codes on the corners and top of each page.	\n
  %        \\item For short answer questions, write your answers in the answer box provided. Leave the grey part blank.	\n
  \n
  \\end{itemize}	\n
  \n
  %%%%%%%%%%%%%%%%%%%%%%%	\n
  \n
  \n
  \n
  \n
  \n
  \n
  \\clearpage	\n
  \n
  }	\n
  \n
  \\end{document}	\n"



  listoriginaltex <- c("\\documentclass[letterpaper,10pt]{article}	\n",
                       "%To create version with more than 12pt text	\n",
                       "%\\documentclass[letterpaper,14pt]{extarticle}	\n",
                       "\n",
                       "\\usepackage{multicol}	\n",
                       "\\usepackage[utf8x]{inputenc}	\n",
                       "\\usepackage[T1]{fontenc}	\n",
                       "\\usepackage{amsmath}	\n",
                       "\n",
                       "\\usepackage[box,completemulti]{automultiplechoice}	\n",
                       "\n",
                       "\n",
                       "\\renewcommand{\\rmdefault}{\\sfdefault}	\n",
                       "\n",
                       "%\\geometry{hmargin=2.5cm,headheight=1.5cm,headsep=.2cm,footskip=0.8cm,top=2.5cm,bottom=2.5cm}	\n",
                       "\n",
                       "\\usepackage{titlesec}	\n",
                       "\n",
                       "% Format section titles with horizontal lines	\n",
                       "\\titleformat{\\section}	\n",
                       "{\\hrule\\center\\normalfont\\normalsize\\bfseries}{\\thesection.}{1em}{}[{\\vspace{1mm}\\hrule}]	\n",
                       "\n",
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
                       "\n",
                       "\\baremeDefautS{mz=1}	\n",
                       "\n",
                       "% Point here to the question file(s)	\n",
                       "%\\input{questions.tex}	\n",
                       "\n",
                       "\\onecopy{10}{	\n",
                       "\n",
                       "\\vspace*{.5cm}	\n",
                       "\\begin{minipage}{.4\\linewidth}	\n",
                       "\\centering	\n",
                       "% Uncomment to insert logo image	\n",
                       "%\\includegraphics[height=2cm,width=4cm,keepaspectratio]{logo.png}             	\n",
                       "\n",
                       "% Title	\n",
                       "\\large\\bf Title of the exam \\vspace*{1mm}	\n",
                       "\\end{minipage}	\n",
                       "\\namefield{\\fbox{\\begin{minipage}{.5\\linewidth}	\n",
                       "% Identifier (change to name if needed)	\n",
                       "%ID Number:	\n",
                       "Name:	\n",
                       "\\vspace*{.5cm}	\n",
                       "%\\dotfill	\n",
                       "\\vspace*{1mm}	\n",
                       "\\end{minipage}}}	\n",
                       "\n",
                       "\n",
                       "%%% INSTRUCTIONS TO STUDENTS, UNCOMMENT AS NEEDED	\n",
                       "\n",
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
                       "\n",
                       "\\end{itemize}	\n",
                       "\n",
                       "%%%%%%%%%%%%%%%%%%%%%%%	\n",
                       "\n",
                       "\n",
                       "\n",
                       "\n",
                       "\n",
                       "\n",
                       "\\clearpage	\n",
                       "\n",
                       "}	\n",
                       "\n",
                       "\\end{document}	\n")


}




