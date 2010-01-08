%$Date$
%$Author$
\name{calibrate}
\alias{calibrate}
\alias{getPosterior}
\alias{isCaliBayesReady}

\title{The core calibayes webservices}

\description{These functions access CaliBayes through a web-service interface. \code{calibrate} sets off a job, \code{isFinished} determines if a job is finished, and \code{getPosterior} downloads the result.}

\usage{
    calibrate(wsdl, sbml, settings, calibayes, asText=FALSE)
    isCaliBayesReady(wsdl, sid)
    getPosterior(wsdl, sid)
}


\arguments{
  \item{wsdl}{A valid url that points to the CaliBayes wsdl file. The inference engine of CaliBayes can (and should) be downloaded and installed on your local infrastructure. The wsdl url given in the example is only for illustration purposes.}
  \item{sbml}{A valid SBML model.}
  \item{asText}{A logical value indicating that the first argument, \code{sbmlModel}, should be treated as a string to parse, and not the name of a file.}
  \item{settings}{A settings object, created by the \code{\link{createSettings}} function.}  
  \item{calibayes}{A calibayes object, created by the \code{\link{createCalibayes}} function.}
  \item{sid}{A valid calibayes session id.}
}

\value{
    \code{calibrate}: Returns a session id (sid). This session id is valid for any web-service interface to the calibayes system.
    
    \code{isCaliBayesReady}: Checks if the calibration has finished. Returns \code{TRUE} or \code{FALSE}.
    
    \code{getPosterior}: Downloads the results from the CaliBayes installation described by the wsdl file and returns a CaliBayes object.
}

\author{Colin Gillespie}

\seealso{See also \code{\link{createCalibayes}}, \code{\link{createSettings}}  and \code{\link{listSimulatorMethods}}.}
\examples{
    \dontrun{
    library(SBMLModels)
    data(LogisticModel)

    #Just use the first experiment
    yeast_data = YeastGrowth[[1]]
    
    #Plot of the experiment data for the simple decay model
    plot(yeast_data$time, yeast_data$value, xlab="Time", ylab="Population level")
    
    #yeast_data$value are observations with error, 
    #where the error has a Normal distribution with mean 0, and precision tau

    #n increases with the number of parameters
    n = 1000

    #Create prior for the initial species level
    species=data.frame(S=rlnorm(n,3,sqrt(0.02)))

    #Create prior distribution for the measurement error structure
    distributions = c('Gaussian')
    errors = data.frame(S.tau=rlnorm(n,- 11, 1))

    #Create priors for the parameters
    parameters = data.frame(
	                K=rlnorm(n,9,0.2),
	                r=rlnorm(n,1,0.6)
                )

    #Create a prior object
    prior = createCalibayes(parameters, species, distributions, errors, yeast_data)

    #Create settings file
    wsdl = "http://calibayes1.ncl.ac.uk:81/CaliBayesService_v2.wsdl"
    settings = createSettings(wsdl, burn=10, thin=2)
    sbml = LogisticModel
    
    sid = calibrate(wsdl, sbml, settings, prior, asText=TRUE)
    isCaliBayesReady(wsdl, sid)
    
    posterior = getPosterior(wsdl, sid)
    
    plot(posterior)
    compareDistributions(prior, posterior)
    }

}

\keyword{htest}

