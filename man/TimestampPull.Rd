\name{TimestampPull}
\alias{TimestampPull}
\title{TimestampPull
}
\description{Pulls timestamps in H:M:S format
}
\usage{
TimestampPull(allText, HMSindex)
}
\arguments{
  \item{allText}{Character string containing MPC file}
  \item{HMSindex}{A number specifying the index to pull from the H, M, and S arrays}
}
\value{
  \item{HMS}{A single character combining the values from the H, M, and S arrays to read a complete time stamp (H:)}
}
\examples{
file_path <- system.file("extdata", "ExampleMPCOutput.txt", package = "medparser")
allText = readLines(file_path)
C_index = 1+grep("C:", allText[-1], value = FALSE)
H_index = 1+grep("H:", allText[-1], value = FALSE)
M_index = 1+grep("M:", allText[-1], value = FALSE)
NumberOfTimestampsRecorded = (M_index-1)-(H_index+1)
firsttimestamp = TimestampPull(allText,1)
}
\author{
Olivia Ortelli, Tony Colarusso
}
