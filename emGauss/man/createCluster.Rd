\name{fetch_tag_from_Rd}
\alias{fetch_tag_from_Rd}
\title{Access tag elements from R's Rd file}
\source{
\url{http://stackoverflow.com/questions/17909081/access-elements-from-rs-rd-file}
}

\description{
The function estimates the mean and variance of the k groups of the underlying inputdata y. 
}

\usage{
createCluster(y, k, method = 'binbased')
}
\arguments{
\item{y}{a  (nx2) matrix (nx2) First column: number of bin, Second column: number of observation per bin}
\item{k}{a numeric value - number of components (fitted distributions)}
\item{method} {the method to be used for finding the k groups of data. For more information see Details }
}
\value{
a 2xk matrix with mean and variance per bin
}


\details{
For the input parameter method are two options available, namely binbased and quantiles. 
binbased: the group calculation is based on bins. As a result of that each group has the same number of bins 
quantiles: the group calculation is based on the observed values per bin. As a result of that each group has the same number of observations. 
}

\examples{
v <- c(2, 4, 5,6,5,2,2, 1, 1, 2,  2, 1,6,7,8,7,6, 5, 2,1)

data <- data.frame(name = 1:length(v)+5, v)
data  <- as.matrix(data)

createCluster(as.matrix(data), 2, method = 'binbased')
}

\author{
Thomas J. Leeper <thosjleeper@gmail.com>
}