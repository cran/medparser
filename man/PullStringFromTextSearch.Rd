\name{PullStringFromTextSearch}
\alias{PullStringFromTextSearch}
\title{PullStringFromTextSearch}
\description{Pulls string following specified text}
\usage{
PullStringFromTextSearch(allText, text)
}
\arguments{
  \item{allText}{Character string containing MPC file}
  \item{text}{A character string where matches are searched for}
}
\value{
  \item{stringValue}{A character string where matches are searched for}
  }
\examples{
file_path <- system.file("extdata", "ExampleMPCOutput.txt", package = "medparser")
allText = readLines(file_path)
SubjectID = PullStringFromTextSearch(allText, "Subject:")
}

\author{
Olivia Ortelli, Tony Colarusso
}
