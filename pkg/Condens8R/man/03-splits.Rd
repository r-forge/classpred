\name{splitters}
\alias{registerSplitter}
\alias{findSplit}
\alias{evalSplit}

\title{Bimodal Splitting of Data Sets}
\description{
  Functions to split a data set into two parts.
}
\usage{
registerSplitter(tag, description, FUN)
findSplit(data, metric = "euclidean", algorithm = names(splitters),
  LR = c("L", "R"))
evalSplit(split, data, metric, tool = c("sw", "ssq"))
}
\arguments{
  \item{tag}{
    A character string representing a short abbreviation used to identify
    and eventually call the associated "splitter" functoin that can
    dichtomize the data.
  }
  \item{description}{
    A character string containing the full name of the splitting
    algorithm. Potentially, this can be used in data summaries or plots.
  }
  \item{FUN}{
    A "splitter"; that is, a function that takes a distance matrix as
    input and returns a numeric vector that assigns samples in the data
    set to one of two classes labeled "1" or "2".
  }
  \item{data}{
    The data set to be split into two parts.
  }
  \item{metric}{
    A distance metric. This metric should be one of the values supported
    by the \code{\link[ClassDiscovery]{distanceMatrix}} fuction from the
    \code{ClassDiscovery} package. All metrics computed by the
    \code{\link[stats]{dist}} function are supported.
  }
  \item{algorithm}{
    A tag that uniquely identifies one of the splitting functions that
    have been registered.  Built-in registered "splitters", with their
    tags, include: (hc) agglomeratve hierarchical clustering; (km)
    k-means clustering; (dv) divisive heirarchical clustering, and (ap)
    affinity propagation clustering.
  }
  \item{LR}{
    A character vector of length two denoting the laels to be used for
    the two classes after splitting the data.
  }
  \item{split}{
    A clustering factor, typically the output of a call to the
    \code{findSplit} fuction. 
  }
  \item{tool}{
    The tool/algorithm to be used to evalaute the quality of the
    split. Built-in algorithms include the mean silhouette width (sw) or
    the within-group sum of square distances (ssq).
  }
}
\details{
  The core idea of this package is to construct decision trees that
  simulataneouly learn how to cluster the items in a data set while also
  learning a classifier that can predict the cluster assignments of new
  data. We support four built-in clustering algortihms to learn how to
  split a single data set into two parts:

  \describe{
    \item{hc}{agglomerative hierarchical clustering as implemented in
    the \code{\link[stats]{hclust}} function, followed by
    \code{\link[stats]{cutree}}.}
  \item{km}{\code{\link[stats]{kmeans}} clustering with k = 2.}
  \item{dv}{divisive hierarchical clustering as implemented in the
    \code{\link[cluster]{diana}} function.}
  \item{ap}{affinity propagation clustering, as implemented in the
    \code{\link[apcluster]{apcluster}} function.} 
  }
}
\value{
  The \code{registerSplitter} funciton invisibly returns the tag
  assigned to the new splitter.

  The \code{findSplit} function returns a two-level factor with length
  equal to the number of columns (samples) in the data set.

  The \code{evalSplit} returns a list of length two, containing a
  \code{stat}istic (either the men silhouette width or the within-group
  sum of square distances, depending on the tool being used) along with
  an estimated empirical \code{pv}alue.
}
\author{
  Kevin R. Coombes <krc@silicovore.com>
}
\examples{
nr <- 200 # features
nc <- 60  # samples
set.seed(80123)
comat <- matrix(rnorm(nr*nc, 0, 1), nrow = nr)
dimnames(comat) <- list(paste0("F", 1:nr),
                        paste0("S", 1:nc))
splay <- rep(c(2, -2), each = nc/2)
for(J in 1:30) comat[J, ] <- comat[J, ] + splay
C.hc <- findSplit(comat, algorithm = "hc")
evalSplit(C.hc, comat, "euclid", "sw")
}
\keyword{ multivariate }
