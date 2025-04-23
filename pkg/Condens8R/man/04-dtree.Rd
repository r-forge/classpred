\name{createTree}
\alias{createTree}
\alias{registerPredictor}
\alias{availablePredictors}

\title{Create a Decision tree}
\description{
  Functions to create a binary decision tree with a complex algorithm at
  each nternal node.
}
\usage{
createTree(data, metric, label = "H", pcut = 0.05, N = 100, splitter =
"hc", modeler = "logic")
registerPredictor(tag, description, modeler)
availablePredictors()
}
\arguments{
  \item{data}{
    A data matrix where (numerical) features are rows and samples are
    columns.
  }
  \item{metric}{
    A distance metric. This metric should be one of the values supported
    by either the \code{\link[ClassDiscovery]{distanceMatrix}} function
    from the \code{ClassDiscovery} package or the
    \code{\link[Mercator]{binaryDistance}} function from the 
    \code{Mercator} package. All metrics computed by the
    \code{\link[stats]{dist}} function are included.
  }
  \item{label}{
    Character use to mark the root note. (default is "H" for "head".)
  }
  \item{pcut}{
    Significance cutoff ("alpha") on the empirical p-value.
  }
  \item{splitter}{
    A tag that uniquely identifies one of the splitting functions that
    have been registered.  Built-in registered "splitters", with their
    tags, include: (hc) agglomeratve hierarchical clustering; (km)
    k-means clustering; (dv) divisive heirarchical clustering, and (ap)
    affinity propagation clustering.
  }
  \item{modeler}{
    A tag that uniquely identifies one of the predictor algorithms that
    have been registered.  Built-in registered "predictors", with their
    tags, include: (logic) logic regression; (svm) support vector
    machines; (lr) logitic regression, (rf) random forests, and (tr)
    tail rank.
  }
  \item{N}{Number of random splits used to estimate emoirical p-value.}
  \item{tag}{
    A character string representing a short abbreviation used to identify
    and eventually call the associated "predictor" method that can
    dichtomize the data.
  }
  \item{description}{
    A character string containing the full name of the prediction
    algorithm. Potentially, this can be used in data summaries or plots.
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

  We also support multiple classification algorithms to learna model to
  predict the binary split at the node:

  \describe{
    \item{logic}{A modeler that implemts the "logic regression" approach
    to combine binary predictprs using logical operations, as
    implemented in the \code{logicFS} package.}
    \item{svm}{Support vector machines.}
    \item{lr}{The traditional logistic regression approach.}
    \item{rf}{Random forests.}
    \item{tr}{The "tail-rank" algorithm.}
  }

  To get a list of the available methods, use the functions
  \code{\link{availableSplitters}} and
  \code{\link{availablePredictors}}. You can add your own algorithm with
  the functions \code{\link{registerSplitter}} pr
  \code{registerPredictor}. A "splitter" should accept a distance matrix
  as input and return a clustering vector with labels "1" or "2" for the
  classes. A "predictpr", by contrast, should be an object of the
  \code{\link[Modeler]{Modeler-class}}.
}
\value{
  Returns a \code{BinaryNode} object, which includes a

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
C.hc <- findSplit(comat, splitter = "hc")
evalSplit(C.hc, comat, "euclid", "sw")
}
\keyword{ multivariate }
