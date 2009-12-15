%$Date$
%$Author$
\name{listSimulatorMethods}
\alias{listSimulatorMethods}

\title{A web-service to determine the simulators available}

\description{This functions accesses CaliBayes through a web-service interface to determine the list of simulators available.}

\usage{listSimulatorMethods(wsdl)
}


\arguments{
  \item{wsdl}{A valid url that points to the calibayes wsdl file.}
}

\value{
    \code{listSimulatorMethods}: Returns a list of available simulators. One of these simulators should used in the \code{\link{createSettings}} function.
}
\author{Colin Gillespie}

\seealso{See Also as \code{\link{createSettings}}.}
\examples{
    \dontrun{
        wsdl = "http://calibayes2.ncl.ac.uk:81/CalibayesServices1.wsdl"
        listSimulatorMethods(wsdl)
    }

}

\keyword{htest}


