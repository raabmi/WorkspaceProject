\name{EM_Gauss}
\alias{em.gauss}
\title{EM Algorithm with Normals}


\description{
The function fits a mixed distribution of k normals on the binned input data y by using the EM Algorithm.
}

\usage{
em.gauss(y, mu, sigma2, pi, alpha, beta, epsilon=0.000001, ecoff.quantile=0.01, max.iter = 1000)
}
\arguments{
\item{y}{ a numeric vector First element: number of resistent observations}
\item{mu}{a numeric vector of startvalues for the means mu}
\item{sigma2} {a numeric vector of startvalues for the variances sigma2}
\item{pi} {a numeric vector of startvalues for the mixing proportions pi }
\item{alpha} {a numeric number for the alpha of the inverse gamma distribution for penalization }
\item{beta} {a numeric number for the beta of the inverse gamma distribution for penalization }
\item{epsilon} {a numeric number of the stopping critera }
\item{ecoff.quantile} {a numeric number, which defines the quantile of the rightest distribution }
\item{max.iter} {a numeric number, maximum number of iterations of the EM-Algorithm }
}

\value{
a list with a vector of means, variances, pis and ecoff (defined quantile of the rightest distribution)
}


\details{

The rightest density is defined as the distribution, where the sum of the pis firstly is greater than 0.3.
Therefore, the distribution get ordered by their mean in ascending order and then the pis are commulated of the right.

In order to avoid, that one of the sigma2 converges to 0, the likelihood get penalized in dependence of sigma2.
In detail, the penalization term follows a inverse gamma distribution with parameter alpha and beta. 

}

\examples{
v <- c(2, 4, 5,6,5,2,2, 1, 1, 2,  2, 1,6,7,8,7,6, 5, 2,1)

data <- data.frame(name = 1:length(v)+5, v)
data  <- as.matrix(data)

createCluster(as.matrix(data), 2, method = 'binbased')
}

\author{
Angelika Moder, Michaela Raab, Nicole Halmdienst, Wolfgang Rannetbauer, Sebastian Schrey
}