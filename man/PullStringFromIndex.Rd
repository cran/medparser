\name{PullStringFromIndex}
\alias{PullStringFromIndex}
\title{PullStringFromIndex}

\description{Pulls indices from MPC arrays}

\usage{PullStringFromIndex(allText, index)}

\arguments{
  \item{allText}{Character string containing MPC file}
  \item{index}{A number specifying the index where the array or value of interest begins in the MPC file}
  }

\value{
  \item{stringValue}{A character string following the index start}
}
\examples{
file_path <- system.file("extdata", "ExampleMPCOutput.txt", package = "medparser")
allText = readLines(file_path)
B_index = 1+grep("B:", allText[-1], value = FALSE)
Left_LeverPressBout = DecimalStringToInteger(PullStringFromIndex(allText,B_index+1))
}
\author{Olivia Ortelli, Tony Colarusso}
