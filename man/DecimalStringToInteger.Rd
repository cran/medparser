\name{DecimalStringToInteger}
\alias{DecimalStringToInteger}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{DecimalStringToInteger}

\description{Converts string into integer}

\usage{DecimalStringToInteger(string)}

\arguments{
  \item{string}{Character to be converted into an integer}
}

\value{
  \item{integer}{An integer}
}
\examples{
file_path <- system.file("extdata", "ExampleMPCOutput.txt", package = "medparser")
allText = readLines(file_path)
B_index = 1+grep("B:", allText[-1], value = FALSE)
Left_LeverPressBout = DecimalStringToInteger(PullStringFromIndex(allText,B_index+1))
}

\author{
Olivia Ortelli, Tony Colarusso
}
