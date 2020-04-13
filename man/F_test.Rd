\name{F_test}
\alias{F_test}
\encoding{UTF-8}
\title{Test F}

\description{This fuction performs the F-Test about R-Squared.}

\usage{F_test(R_Squared, Observations, Significance_Level = 0.05)}

\arguments{
  \item{R_Squared}{It indicates the goodness of fit, naturally it's a number between zero and one.}
  
  \item{Observations}{It indicates the number of statistical observations.}
  
  \item{Significance_Level}{It indicates the significance level of the test, in other words the probability to refuse a true hypothesis (H0). There are only three possible values: 0.05, 0.01, 0.001}
}

\value{
  \item{Statistical Test:}{This value derives from the following formula: (R_Squared*(Observations-2))/(1-R_Squared)}
  \item{Critical Value:}{This particular value is calculated on the basis of the table of Fisher's distribution.}
  \item{Decision:}{It indicates the result of the test and it can be of two types: "Accept H0" or "Refuse H0".}
}

\author{AntonioP-R}

\note{For further information, or if there is a malfunction, please contact the author through the following email address: \email{antoniop.r0033@gmail.com}}

\section{Warning}{This function gives back three different types of error: the first error occurs when "Significance_Level" is different from default values (0.05, 0.01, 0.001), the second error occurs when "Observations" aren't "numeric" type or when they are smaller than three, while the last error occurs when the "R_Squared" isn't between zero and one.}

\examples{
#write the function
F_test(R_Squared = 0.90,Observations = 20, Significance_Level = 0.01)
}
