%$Date$
%$Author$
\name{calibrate}
\alias{calibrate}
\alias{getPosterior}
\alias{isCaliBayesReady}
\alias{isCaliBayesFinished}

\title{The core calibayes webservices}

\description{These functions access CaliBayes through a web-service interface. \code{calibrate} sets off a job, \code{isCaliBayesReady} determines if a job is finished, and \code{getPosterior} downloads the result.}

\usage{
    calibrate(wsdl,
                sbml, 
                CaliBayes.Settings, 
                CaliBayes.Experiment, 
                CaliBayes.Distribution, 
                asText=FALSE) 
    isCaliBayesReady(wsdl, sid)
    getPosterior(wsdl, sid)
    isCaliBayesFinished(wsdl, sid, no_of_checks, maxtime)
}


\arguments{
  \item{wsdl}{A valid url that points to the CaliBayes wsdl file. The inference engine of CaliBayes can (and should) be downloaded and installed on your local infrastructure. The wsdl url given in the example is only for illustration purposes.}
  \item{sbml}{A valid SBML model.}
  \item{asText}{A logical value indicating that the first argument, \code{sbmlModel}, should be treated as a string to parse, and not the name of a file.}
  \item{CaliBayes.Settings}{A settings object, created by the \code{\link{createCaliBayesSettings}} function.}  
  \item{CaliBayes.Experiment}{An experiment object, created by the \code{\link{createCaliBayesExperiment}} function.}  
  \item{CaliBayes.Distribution}{A CaliBayes Distribution object, created by the \code{\link{createCaliBayesDistribution}} function.}
  \item{sid}{A valid CaliBayes session id.}
  \item{no_of_checks}{The number of times the \code{isCaliBayesReady} function will be called}.
  \item{maxtime}{The maximum number of minutes to check CaliBayes.}

}

\value{
    \code{calibrate}: Returns a session id (sid). This session id is valid for any web-service interface to the CaliBayes system.
    
    \code{isCaliBayesReady}: Checks if the calibration has finished. Returns \code{TRUE} or \code{FALSE}.
       
    \code{isCaliBayesFinished}: Returns a either \code{TRUE} or \code{FALSE} to indicate if calibration is complete. This function also prints times when the last check was carred out.
    
    \code{getPosterior}: Downloads the results from the CaliBayes installation described by the wsdl file and returns a CaliBayes Distribution object.   


}

\author{Colin Gillespie}

\seealso{See also \code{\link{createCaliBayesDistribution}}, \code{\link{createCaliBayesSettings}}  and \code{\link{listSimulatorMethods}}.}
\examples{
    \dontrun{
    library(SBMLModels)
    data(LogisticModel)

    #Just use the first experiment and the first few time points
    yeast_data = YeastGrowth[[1]][1:3,]
    
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

    #Create an Experiment object
    experiments = createCaliBayesExperiment(yeast_data, species)
    #Create a Distribution object
    prior = createCaliBayesDistribution(parameters, species, distributions, errors, FALSE)

    #Create settings file
    wsdl = "http://calibayes1.ncl.ac.uk:81/CaliBayesService_v2.wsdl"
    settings = createCaliBayesSettings(wsdl, burn=10, thin=2)
    sbml = LogisticModel
    
    sid = calibrate(wsdl, sbml, settings, experiments, prior, asText=TRUE)
    
    #A one off check of CaliBayes
    isCaliBayesReady(wsdl, sid)

    #Now check every 100 seconds    
    maxtime = 10 # 10*60 seconds
    no_of_checks = 6 # Check calibayes every 100 seconds, i.e. every (60*10)/6 seconds
    isCaliBayesFinished(wsdl, sid, no_of_checks, maxtime)
   
    
    posterior = getPosterior(wsdl, sid)
    
    plot(posterior)
    compareDistributions(prior, posterior)
    }

}

\keyword{htest}

