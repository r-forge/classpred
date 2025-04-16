\name{learnConstant}
\alias{learnConstant}
\alias{predictConstant}
\alias{makeLeaf}
\title{
  Fit models and make predictions with a constant classifier
}
\description{
  These functions are used to apply the generic train-and-test
  mechanism to a classifier that simply returns the same constant value
  for all inputs. 
}
\usage{
learnConstant(data, status, params, predfun)
predictConstant(newdata, details, status, ...)
makeLeaf(value, status = factor(c("L", "R")))
}
\arguments{
  \item{data}{
    The data matrix, with rows as features ("genes") and columns as the
    samples to be classified.
}
  \item{status}{
    A factor, with two levels, classifying the samples. The length must
    equal the number of \code{data} columns.
}
  \item{params}{
    A list of additional parameters used by the classifier; see Details.
}
  \item{predfun}{
    The function used to make predictions on new data, using the
    trained classifier.  Should always be set to
    \code{predictConstant}.
}
  \item{newdata}{
    Another data matrix, with the same number of rows as \code{data}.
}
  \item{details}{
    A list of additional parameters describing details about the
    particular classifier; see Details.
}
  \item{\dots}{
    Optional extra parameters required by the generic "predict" method.
}  \item{value}{
    The constant value to be returned by the predictor. This should be
    one of the levels of the status vairable.
}
}
\details{
  The input arguments to both \code{learnConstant} and \code{predictConstant}
  are dictated by the requirements of the general train-and-test
  mechanism provided by the \code{\link[Modeler]{Modeler-class}}.

  The "Constant" classifier is designed for use in the decision trees that
  are part of this package. They will be used as the "predictors" in
  leaf nodes, which always make the same prediction on any samples that
  make their way down to that node.

  We provied the \code{makeLeaf} fnction as a convenience to create
  constant classifiers, by specifying their preferred value. (This value
  could also be provided as an extra/optional pafameter to the
  \code{Modeler} constructor.) For must uses, it is easier to simply use
  \code{makeLeaf} and ignore the \code{learnConstant} and
  \code{predictConstant}. The result of fitting the model using
  \code{learnConstant} or \code{makeLeaf} is a member of the
  \code{\link[Modeler]{FittedModel-class}}. 
}
\value{
  The \code{learnConstant} and the \code{makeLeaf} functions return an
  object of the \code{\link[Modeler]{FittedModel-class}}, representing a
  Constant classifier that has been fitted on a training \code{data} set.

  The \code{predictConstant} function returns a factor containing the
  predictions of the model when applied to the new data set, of length
  equal tothe number of columns (samples). 
}
\author{
  Kevin R. Coombes <krc@silicovore.com>
}
\seealso{
  See \code{\link[Modeler]{Modeler-class}} and
  \code{\link[Modeler]{Modeler}} for details about how to train and test
  models. See \code{\link[Modeler]{FittedModel-class}} and
  \code{\link[Modeler]{FittedModel}} for details about the structure of
  the object returned by \code{learnConstant}. 
}
\examples{
# simulate some data
data <- matrix(rnorm(100*20), ncol=20)
status <- factor(rep(c("A", "B"), each=10))

# create a constnt modeler.
fm <- makeLeaf("A", factor(c("A", "B")))

# Make predictions on some new simulated data
predictConstant(data, fm@details, status)
}
\keyword{ classif }
\keyword{ multivariate }
