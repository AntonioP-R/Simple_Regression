\name{SimpleLinearRegression}
\alias{SimpleLinearRegression}
\encoding{UTF-8}
\title{Simple Linear Regression}

\description{This function is used to calculate a Simple (Linear) Regression. If you need more information, you can find them in the section "details".}

\usage{SimpleLinearRegression(Xvar, Yvar, Regression_Plot = FALSE, Residuals_Plot = FALSE)}

\arguments{
  \item{Xvar}{It indicates the indipendent variables, naturally it is a numeric vector.}
  \item{Yvar}{It indicates the dependent variables.}
  \item{Regression_Plot}{If you set it on "TRUE", the function will show a regression plot. It's an optional argument.}
  \item{Residuals_Plot}{It shows two plots: the first shows the residuals' distribution, while the second shows the standardized residuals. Also this argument is optional.}
}

\details{The results of this function are calculate through the method of least squares. Moreover, in addition to the main results, it's possible to obtain two different types of plot: regression plot and residuals plot.}

\value{
  \item{Slope}{It represents the slope of regression line.}
  \item{Intercept}{It represents the point where the regression line intersects y-axis.}
  \item{R_Squared}{It represents the goodness of fit of the regression model. It can be calculated through Pearson's correlation coefficent raised to the square.}
  \item{MSE}{It is the acronym of "Mean Squared Error". It can be calculated through the average of squared residuals; generally, the predictions are less reliable when this value (MSE) is higher.}
  \item{RMSE}{It is the acronym of "Root Mean Squared Error". It can be calculated through the square root of MSE. Also in this case the logic is the same of the previous argument, the only difference between the two values is that in this case the unit of measure isn't raised to the square.}
}

\author{AntonioP-R}

\note{For further information, or if there is a malfunction, please contact the author through the following email address: \email{antoniop.r0033@gmail.com}}

\section{Warning}{This function gives back two different types of error: the first error occurs when input data aren't "numeric" type, while the second occurs when input data don't have the same size.}

\examples{
#write the data
x<-c(1,9,6,4)
y<-c(1,2,5,6)

#use the function
model<-SimpleLinearRegression(x,y,FALSE,FALSE)
print(model)
}

