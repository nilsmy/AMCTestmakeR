## ------------------------------------------------------------------------
library(AMCTestmakeR)

## ------------------------------------------------------------------------
AMCcreatequestions(question = "How much is $1+1$?",
                   correctanswers = 2,
                   incorrectanswers = list(3, 11))

## ------------------------------------------------------------------------
question <- c("How much is $1+1$ ?",
              "How much is $1 \\times 1$ ?",
              "How much is $\\frac{1}{2}$ ?")
correct <- c(2,1,0.5)
incorrect1 <- c(3,4,10)
incorrect2 <- c(1,3,100)
incorrect3 <- c(4,8,NA)

## ------------------------------------------------------------------------
AMCcreatequestions(question = question,
   correctanswers = correct,
   incorrectanswers = list(incorrect1,incorrect2,incorrect3))

## ------------------------------------------------------------------------
AMCcreatequestions(element = c("ADD", "MULT", "DIV"),
   question = question,
   correctanswers = correct,
   incorrectanswers = list(incorrect1,incorrect2,incorrect3))

## ------------------------------------------------------------------------
AMCcreatequestions(code = c("ADD1", "MULT1", "DIV1"),
   question = question,
   correctanswers = correct,
   incorrectanswers = list(incorrect1,incorrect2,incorrect3))

## ------------------------------------------------------------------------
AMCcreatequestions(codeprefix = "MATH",
   question = question,
   correctanswers = correct,
   incorrectanswers = list(incorrect1,incorrect2,incorrect3))

