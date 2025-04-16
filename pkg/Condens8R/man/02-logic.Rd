\name{learnLogic}
\alias{learnLogic}
\alias{predictLogic}
\alias{logicModeler}
\title{
  Fit models and make predictions with a logic regression classifier
}
\description{
  These functions are used to apply the generic train-and-test
  mechanism to a classifier that uses the concept of logic regression.
}
\usage{
learnLogic(data, status, params, pfun)
predictLogic(newdata, details, status, ...)
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
  \item{pfun}{
    The function used to make predictions on new data, using the
    trained classifier.  Should always be set to
    \code{predictLogic}.
}
  \item{newdata}{
    Another data matrix, with the same number of rows as \code{data},
    and in the same order.
}
  \item{details}{
    A list of additional parameters describing details about the
    particular classifier; see Details.
}
  \item{\dots}{
    Optional extra parameters required by the generic "predict" method.
}
}
\details{
  The input arguments to both \code{learnLogic} and \code{predictLogic}
  are dictated by the requirements of the general train-and-test
  mechanism provided by the \code{\link[Modeler]{Modeler-class}}.
  
  The Logic classifier is similar to the logistic regression classifiers
  already included in the \code{Modeler} package. The crucial difference
  is that the logic regression model assumes that all predictors are
  binary variables (thought of as logical vectors) and that logical
  combinations (based on and, or, and not operators) of predictors
  shouod also be considered. Internally, if the trainng data consist of
  continuous data, we use the \code{\link[BimodalIndex]{bimodalIndex}}
  function to dichotomize the trainng data. The same cutpoints are later
  used to dichotomize the test data in the \code{predict} function.

  Feature selection is handled by the bagging routines in the
  \code{logicFS} package. The default call to
  \code{\link[logicFS]{logic.bagging}} sets specific values for
  parameters \code{B=20}, \code{nleaves=10}, \code{rand=54321} and for
  \code{\link[LogicReg]{logreg.anneal.control}}. If you want to add additional
  parameters, you can use the \code{params} parameter in
  \code{learnLogic}.

  The result of fitting the model using \code{learnLogic} is a member of
  the \code{\link[Modeler]{FittedModel-class}}.  In additon to storing
  the prediction function (\code{pfun}) and the training data and status,
  the FittedModel stores those details about the model that are required
  in order to make predictions of the outcome on new data. The
  \code{details} object is appropriate for sending as the second
  argument to the \code{predictLogic} function in order to make
  predictions with the model on new data.  Note that the status vector
  here is the one used for the \emph{training} data, since 
  the prediction function only uses the \emph{levels} of this factor to
  make sure that the direction of the predictions is interpreted
  correctly.
}
\value{
  The \code{learnLogic} function returns an object of the
  \code{\link[Modeler]{FittedModel-class}}, representing a Logic
  classifier that has been fitted on a training \code{data} set.

  The \code{predictLogic} function returns a factor containing the
  predictions of the model when applied to the new data set.
}
\author{
  Kevin R. Coombes <krc@silicovore.com>
}
\seealso{
  See \code{\link[Modeler]{Modeler-class}} and
  \code{\link[Modeler]{Modeler}} for details about how to train and test
  models.  See \code{\link[Modeler]{FittedModel-class}} and
  \code{\link[Modeler]{FittedModel}} for details about the structure of
  the object returned by \code{learnLogic}.
}
\examples{
nr <- 100 # features
nc <- 40  # samples
set.seed(97531)
bimat <- matrix(rbinom(nr*nc, 1, 0.45), nrow = nr)
dimnames(bimat) <- list(paste0("B", 1:nr),
                        paste0("S", 1:nc))
stat <- rbinom(nc, 1, 0.37)

myMod <- learn(logicModeler, bimat, stat)
table(predict(myMod), stat) # on the diagonal
}
\keyword{ classif }
\keyword{ multivariate }
