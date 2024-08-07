% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatInput.R
\name{formatInput}
\alias{formatInput}
\title{formatInput}
\usage{
formatInput(remindmifPath, magpiemifPath, blackmagiccDir = NULL)
}
\arguments{
\item{remindmifPath}{file path for the reference REMIND emissions}

\item{magpiemifPath}{file path for the MAgPIE emissions}

\item{blackmagiccDir}{directory to save intermediate inputs used by MAGICC}
}
\value{
a composite data.frame containing the scenario's MAgPIE emissions and reference REMIND emissions
}
\description{
converts MAgPIE emissions data, combining it with user-selected REMIND emissions, into the format
necessary to run MAGICC7
}
\examples{
  \dontrun{
    x <- formatInput(remindmifPath, magpiemifPath, blackmagiccDir)
  }
}
\author{
Michael Crawford
}
