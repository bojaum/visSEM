% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sem_checker_plot.R
\encoding{UTF-8}
\name{sem_checker_plot}
\alias{sem_checker_plot}
\title{Plots the assumptions of linear equations}
\usage{
sem_checker_plot(x, spatial=FALSE, what=1)
}
\arguments{
\item{x}{a data.frame}

\item{spatial}{logical if TRUE evaluate spatial autocorrelation}

\item{what}{choose the regression to be evaluated}
}
\value{
a plot
}
\description{
This functions plots the result of sem_checker.
}
\examples{
\donttest{
## Not run:

#Create or load a matrix
vals<-c(0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0)
mat<-matrix(data=vals, nrow=4, ncol=4, byrow = T)
colnames(mat)<-c("cover", "firesev", "age", "rich")
rownames(mat)<-c("cover", "firesev", "age", "rich")

#Creates the psem object based on the matrix
sem_thinker(x=mat, dat=keeley)

##End
}
}
\author{
Anderson Medina
}
\keyword{sem}
