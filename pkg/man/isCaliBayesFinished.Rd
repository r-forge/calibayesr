%$Date$
%$Author$
\name{isCaliBayesFinished}
\alias{isCaliBayesFinished}

\title{A web-service to determine if calibration has finished}

\description{This function periodically checks calibayes, using the \code{isCaliBayesReady} function to determine whether the calibration has completed.}

\usage{isCaliBayesFinished(wsdl, sid, no_of_checks, maxtime)}



\arguments{
  \item{wsdl}{A valid url that points to the calibayes wsdl file.}
  \item{sid}{A valid calibayes session id.}
  \item{no_of_checks}{The number of times the \code{isCaliBayesReady} function will be called}.
  \item{maxtime}{The maximum number of minutes to check calibayes.}
}

\value{
    \code{isCaliBayesFinished}: Returns a either \code{TRUE} or \code{FALSE} to indicate if calibration is complete. This function also prints times when the last check was carred out.
}

\note{This function is a convience function around the \code{\link{isCaliBayesReady}} function.}


\author{Colin Gillespie}

\seealso{See Also as \code{\link{isCaliBayesReady}}.}
\examples{
    \dontrun{
        wsdl = "http://calibayes2.ncl.ac.uk:81/CalibayesServices1.wsdl"
        sid = "valid_id"
        maxtime = 10 # 10*60 seconds
        no_of_checks = 6 # Check calibayes every 100 seconds, i.e. every (60*10)/6 seconds

        isCaliBayesFinished(wsdl, sid, no_of_checks, maxtime)
    }

}

\keyword{htest}


