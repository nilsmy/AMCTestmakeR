
<!-- README.md is generated from README.Rmd. Please edit that file -->
`AMCTestmakeR` provides functions to be used with the free Optical Mark Recognition (OMR) software Auto Multiple Choice.

It's main purpose is to facilitate working with R and AMC in parallel, but it can also be used to transform a spreadsheet into an AMC questionnaire easily.

So far, the features are limited to generating AMC-LaTeX code questions for Multiple Choice Questionnaires (single and multiple answer). Hopefully, it's how most people use Auto Multiple Choice.

Install the library
===================

From CRAN
---------

Install the library with `install.packages("AMCTestmakeR")`, and load it with:

``` r
library(AMCTestmakeR)
```

From Github
-----------

If you don't have `devtools`, install it with `install.packages("devtools")` first.

Install the development version with `devtools::install_github("nilsmy/AMCTestmakeR")`.

Load the library
================

``` r
library(AMCTestmakeR)
```

Basic Use : Generating questions
================================

Generating code for one question
--------------------------------

Let's say that we have a simple question to add: - How much is 1 + 1? - The correct answer is 2 (if you didn't get this one, you're probably at the wrong place) - The incorrect ones are 3 and 11

``` r
AMCcreatequestions(
  question = "How much is $1+1$?",
  correctanswers = 2,
  incorrectanswers = list(3, 11))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%| List of questions |%%%%%%%%%%
%%% (copy & paste in main .tex file) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\element{general}
 {\begin{question}{Q1}\scoring{b=1,m=0,v=0,e=0,b=0}
 How much is $1+1$?
 \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
 \correctchoice{2}
 \wrongchoice{11}
 \wrongchoice{3}
 \end{choices}\end{multicols}\end{question}
}
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%| List of elements |%%%%%%%%%
%%% (copy & paste after questions) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section*{general}
\shufflegroup{general}
\insertgroup{general}
```

Or, more simply `AMCcreatequestions("How much is $1+1$?",2,list(3, 11))`.

Writing questions to a .tex file
--------------------------------

R escapes different characters than LaTeX, so doing a copy-and-paste of the console output will require than you tweak things a bit.

Instead of doing that, I recommend to use the optional argument `writefile = TRUE` to write the generated code into a file. The default creates a `questions.tex` file in the working directory, but you can indicate another path with `filepath`, and append to an existing file -- rather than overwriting the existing file -- with `append = TRUE`).

Generating code for multiple questions
--------------------------------------

If you have an entire questionnaire to generate, the `AMCcreatequestions` can use vectors for many of its arguments.

Let's first create 3 questions, putting the questions and answers in vectors.

``` r
question <- c("How much is $1+1$ ?",
              "How much is $1 \\times 1$ ?",
              "How much is $\\frac{1}{2}$ ?")
correct <- c(2,1,0.5)
incorrect1 <- c(3,4,10)
incorrect2 <- c(1,3,100)
incorrect3 <- c(4,8,NA)
```

Note that the third question has only 2 incorrect answers: `AMCTestmakeR` will simply skip missing values (`NA` and `""`).

``` r
AMCcreatequestions(question = question,
   correctanswers = correct,
   incorrectanswers = list(incorrect1,incorrect2,incorrect3))
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> %%%%%%%%%| List of questions |%%%%%%%%%%
#> %%% (copy & paste in main .tex file) %%%
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> 
#> \element{general}
#>  {\begin{question}{Q1}\scoring{b=1,m=0,v=0,e=0,b=0}
#>  How much is $1+1$ ?
#>  \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
#>  \correctchoice{2}
#>  \wrongchoice{1}
#>  \wrongchoice{3}
#>  \wrongchoice{4}
#>  \end{choices}\end{multicols}\end{question}
#> }
#>  
#> \element{general}
#>  {\begin{question}{Q2}\scoring{b=1,m=0,v=0,e=0,b=0}
#>  How much is $1 \times 1$ ?
#>  \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
#>  \wrongchoice{8}
#>  \wrongchoice{4}
#>  \correctchoice{1}
#>  \wrongchoice{3}
#>  \end{choices}\end{multicols}\end{question}
#> }
#>  
#> \element{general}
#>  {\begin{question}{Q3}\scoring{b=1,m=0,v=0,e=0,b=0}
#>  How much is $\frac{1}{2}$ ?
#>  \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
#>   \wrongchoice{100}
#>  \wrongchoice{10}
#>  \correctchoice{0.5}
#>  \end{choices}\end{multicols}\end{question}
#> }
#> 
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> %%%%%%%%%| List of elements |%%%%%%%%%
#> %%% (copy & paste after questions) %%%
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> \section*{general}
#> \shufflegroup{general}
#> \insertgroup{general}
```

Like before, copy-paste is not optimal, as R escapes different characters than LaTeX. Using `writefile = TRUE` is more convenient to take care of this and translate R text into LaTeX (see above for details). Also, consider using the function `AMCcreatetest()` to handle the full test creation (described later as *Suggested Workflow 1*).

Additional options
------------------

### Changing the element

The element in AMC corresponds to a group of questions. They can for example correspond to different learning outcomes or chapters of a book. AMC is able to randomize questions within elements.

Provide a character value or vector to the argument `element` to define it. If you provide a value, all questions will have this value as element. If you provide a vector, each question will have its corresponding element.

``` r
AMCcreatequestions(element = c("ADD", "MULT", "DIV"),
   question = question,
   correctanswers = correct,
   incorrectanswers = list(incorrect1,incorrect2,incorrect3))
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> %%%%%%%%%| List of questions |%%%%%%%%%%
#> %%% (copy & paste in main .tex file) %%%
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> 
#> \element{ADD}
#>  {\begin{question}{Q1}\scoring{b=1,m=0,v=0,e=0,b=0}
#>  How much is $1+1$ ?
#>  \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
#>  \wrongchoice{4}
#>  \wrongchoice{1}
#>  \correctchoice{2}
#>  \wrongchoice{3}
#>  \end{choices}\end{multicols}\end{question}
#> }
#>  
#> \element{MULT}
#>  {\begin{question}{Q2}\scoring{b=1,m=0,v=0,e=0,b=0}
#>  How much is $1 \times 1$ ?
#>  \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
#>  \correctchoice{1}
#>  \wrongchoice{4}
#>  \wrongchoice{3}
#>  \wrongchoice{8}
#>  \end{choices}\end{multicols}\end{question}
#> }
#>  
#> \element{DIV}
#>  {\begin{question}{Q3}\scoring{b=1,m=0,v=0,e=0,b=0}
#>  How much is $\frac{1}{2}$ ?
#>  \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
#>  \wrongchoice{100}
#>   \correctchoice{0.5}
#>  \wrongchoice{10}
#>  \end{choices}\end{multicols}\end{question}
#> }
#> 
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> %%%%%%%%%| List of elements |%%%%%%%%%
#> %%% (copy & paste after questions) %%%
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> \section*{ADD}
#> \shufflegroup{ADD}
#> \insertgroup{ADD}
#> \section*{MULT}
#> \shufflegroup{MULT}
#> \insertgroup{MULT}
#> \section*{DIV}
#> \shufflegroup{DIV}
#> \insertgroup{DIV}
```

The default element is `general`.

### Changing the question codes

In AMC, each question should have a unique code.

The code can be provided in `AMCTestmakeR` through the argument `code` (like for the `element` argument, a character value or vector can be used).

``` r
AMCcreatequestions(code = c("ADD1", "MULT1", "DIV1"),
   question = question,
   correctanswers = correct,
   incorrectanswers = list(incorrect1,incorrect2,incorrect3))
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> %%%%%%%%%| List of questions |%%%%%%%%%%
#> %%% (copy & paste in main .tex file) %%%
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> 
#> \element{general}
#>  {\begin{question}{ADD1}\scoring{b=1,m=0,v=0,e=0,b=0}
#>  How much is $1+1$ ?
#>  \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
#>  \wrongchoice{3}
#>  \wrongchoice{4}
#>  \wrongchoice{1}
#>  \correctchoice{2}
#>  \end{choices}\end{multicols}\end{question}
#> }
#>  
#> \element{general}
#>  {\begin{question}{MULT1}\scoring{b=1,m=0,v=0,e=0,b=0}
#>  How much is $1 \times 1$ ?
#>  \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
#>  \wrongchoice{4}
#>  \wrongchoice{3}
#>  \wrongchoice{8}
#>  \correctchoice{1}
#>  \end{choices}\end{multicols}\end{question}
#> }
#>  
#> \element{general}
#>  {\begin{question}{DIV1}\scoring{b=1,m=0,v=0,e=0,b=0}
#>  How much is $\frac{1}{2}$ ?
#>  \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
#>  \wrongchoice{100}
#>  \wrongchoice{10}
#>  \correctchoice{0.5}
#>   \end{choices}\end{multicols}\end{question}
#> }
#> 
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> %%%%%%%%%| List of elements |%%%%%%%%%
#> %%% (copy & paste after questions) %%%
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> \section*{general}
#> \shufflegroup{general}
#> \insertgroup{general}
```

A lazy version of this is, instead of codes, to input a code prefix with the `codeprefix` argument. Unique codes will be generated by the function by incrementing numbers after the prefix.

``` r
AMCcreatequestions(codeprefix = "MATH",
   question = question,
   correctanswers = correct,
   incorrectanswers = list(incorrect1,incorrect2,incorrect3))
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> %%%%%%%%%| List of questions |%%%%%%%%%%
#> %%% (copy & paste in main .tex file) %%%
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> 
#> \element{general}
#>  {\begin{question}{MATH1}\scoring{b=1,m=0,v=0,e=0,b=0}
#>  How much is $1+1$ ?
#>  \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
#>  \wrongchoice{1}
#>  \wrongchoice{3}
#>  \correctchoice{2}
#>  \wrongchoice{4}
#>  \end{choices}\end{multicols}\end{question}
#> }
#>  
#> \element{general}
#>  {\begin{question}{MATH2}\scoring{b=1,m=0,v=0,e=0,b=0}
#>  How much is $1 \times 1$ ?
#>  \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
#>  \wrongchoice{4}
#>  \wrongchoice{3}
#>  \correctchoice{1}
#>  \wrongchoice{8}
#>  \end{choices}\end{multicols}\end{question}
#> }
#>  
#> \element{general}
#>  {\begin{question}{MATH3}\scoring{b=1,m=0,v=0,e=0,b=0}
#>  How much is $\frac{1}{2}$ ?
#>  \begin{multicols}{2}\AMCBoxedAnswers\begin{choices}
#>   \wrongchoice{100}
#>  \correctchoice{0.5}
#>  \wrongchoice{10}
#>  \end{choices}\end{multicols}\end{question}
#> }
#> 
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> %%%%%%%%%| List of elements |%%%%%%%%%
#> %%% (copy & paste after questions) %%%
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> \section*{general}
#> \shufflegroup{general}
#> \insertgroup{general}
```

Once the questions are ready...
===============================

When your questions are ready and the `AMCcreatequestions()` gives a satisfactory result. I suggest to directly use `AMCcreatetest()` to create the other .tex files and to have a fully working test easily (*Workflow 1*).

Workflow 1: Creating the test files from scratch with `AMCcreatetest()`
-----------------------------------------------------------------------

`AMCTestmakeR` can create a test from scratch with the function `AMCcreatetest()`. It creates 3 .tex files (groups.tex, questions.tex, elements.tex) that can be directly used in the AMC project folder.

The first arguments of this function are passed to the `AMCcreatequestions()` function (see above for how to use it). The rest of the arguments are used to set test options (like `fontsize`, `separateanswersheet`, `title`, `identifier`, etc.). See the function documentation for a full list of options. If you don't pass any option (except for the questions of course), you should have a useable -- albeit not customized -- test.

``` r
AMCcreatetest("How much is $1+2$?",2,list("3", "11"))
```

### More options

Separate answer sheets, font size, title, instructions, etc.

``` r
AMCcreatetest(
  #This part is passed to the AMCcreatequestions() function:
  question = "How much is $1+1$?",
  2,
  list("3", "11"),
  #The next part is passed to AMCcreateelements():
  shuffle = T,
  sections = T,
  #The last part is for general test options:
  title = "Exam", #Custom title
  paper = "a4", #change the paper for a4
  fontsize = 11, #change fontsize
  identifier = "ID Number", #change identifier
  twosided = F, #print in one sided
  instructions = "Don't respond here.", #show an instructions block
  separateanswersheet = T, #use a separate answer sheet
  answersheettitle = "Respond Here", #Change answer sheet title
  answersheetinstructions = "Fill the boxes."#Answer sheet instructions
)
```

When working on the questions, I suggest to work using `AMCcreatequestions()` with the default output as notes (to check the result without opening a separate .tex file). Once your questions are ready, I suggest to switch to `AMCcreatequestions()`, using the same beginning arguments, and changing the rest.

Workflow 2: Doing things manually with your own template and `AMCcreateelements()`
----------------------------------------------------------------------------------

If you want to customize more, you can do things step by step. If doing that, I highly recommend starting by reading the AMC documentation.

When using `AMCcreatequestions()` to create a questionnaire in AMC, I suggest to create, with `writefile = TRUE`, the questions in a separate questions file (e.g. `questions.tex`) in your AMC project folder.

From there, in your main .tex document (usually, that's named `groups.tex` by AMC), add `\input{questions.tex}` at the beginning (but still after your `\begin{document}`).

Then, where you want to place the different `elements`, in your main .tex, add `\insertgroup{element}` for each of them. Before the `\insertgroup{}` command, you can use `\shufflegroup{element}` to shuffle the questions within the element.

### The `AMCcreateelements()` function

If you have many elements in your document, and therefore many `\insertgroup{}` (and `\shufflegroup{}`) to insert, you may want to use the function `AMCcreateelements()` function. It will show as a console message (which you can, this time, easily copy-and-paste into your main .tex document) the commands to insert (and shuffle, if desired, through the `shufflequestions` argument) the elements:

``` r
AMCcreateelements(element = c("ADD", "MULT", "DIV"), shufflequestions = T, sections = T)
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> %%%%%%%%%| List of elements |%%%%%%%%%
#> %%% (copy & paste after questions) %%%
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> \section*{ADD}
#> \shufflegroup{ADD}
#> \insertgroup{ADD}
#> \section*{MULT}
#> \shufflegroup{MULT}
#> \insertgroup{MULT}
#> \section*{DIV}
#> \shufflegroup{DIV}
#> \insertgroup{DIV}
```

Note that, if the same element is input multiple times (which often happens if you pass to this function the same vector of elements as the one used in `AMCcreatequestions()`), it is not a problem, since only unique values are output:

``` r
AMCcreateelements(element = c("MATH", "MATH", "MATH", "STAT"), shufflequestions = F, sections = F)
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> %%%%%%%%%| List of elements |%%%%%%%%%
#> %%% (copy & paste after questions) %%%
#> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#> \insertgroup{MATH}
#> \insertgroup{STAT}
```

Future features
===============

Auto multiple choice s a great freeware that is able to do a lot more that what `AMCTestmakeR` helps for, so I will try to add the most helpful features here soon. This software feels the In any case, I strongly encourage to read the documentation of how to use LaTeX in Auto Multiple Choice to get a sense of its many possibilities.
