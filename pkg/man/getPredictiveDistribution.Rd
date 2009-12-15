%$Date$
%$Author$
\name{getPredictiveDistribution}
\alias{getPredictiveDistribution}
\alias{updatePredictiveResults}


\title{Obtaining the predictive posterior distribution}

\description{Once calibration has completed, the next natural step is to calulate the predictive posterior distribution. That is selecting parameter combinations at random from the posterior results and simulting forward.

These web-services use the basisR functions \code{forwardSimulate}, \code{isForwardSimReady}, and \code{getForwardSimResult}. Currently, there is only one simulator available, the stochastic simulator \code{gillespie2}.
}

\usage{
    getPredictiveDistribution(sbml, max_time, iters, no_of_sims, prior, posterior, asText=FALSE)
    updatePredictiveResults(results)
}


\arguments{
    \item{sbml}{A valid sbml model. It is recommended that the \code{validateSBML} web-service from basisR is used to ensure that the model valid SBML.}
    \item{asText}{logical value indicating that the first argument, \code{sbml}, should be treated as string to parse, and not the name of a file.}
    \item{max_time}{The model is simulated from time 0,...,\code{max_time}, i.e. the final simulation point.}
    \item{iters}{The level of thinning used when storing the output. For example if \code{max\_time}=100000 and \code{iters}=1000, then we store the outputted times t=0, 10, 20, ...., 100000}
    \item{no_of_sims}{The number of parameter combinations that are simulated. Since BASIS use a stochastic simulator, this means that the simulation results for identical parameters will give different results.}
    \item{prior}{A calibayes object, created by the \code{createCalibayes} or \code{loadCalibayes} or returned by the \code{calibrate} function. The prior is used to obtain values of the species initial conditions.}
    \item{posterior}{A calibayes object, created by the \code{createCalibayes},\code{loadCalibayes} or returned by the \code{calibrate} function. The posteriro is used to obtain values of the parameters.}
    \item{results}{A calibayesPredictive object, created by \code{getPredictiveDistribution}.}

  
}

\value{
    \code{getPredictiveDistribution}: Returns a calibayesPredictive object.
    
    \code{updatePredictiveResults}: Returns a calibayesPredictive object.
}

\note{ 
The \code{calibayesPredictive} object contains a number of attributes.

\code{indx}: The row or index selected at random from the prior and posterior. The length of indx will equal \code{no_of_sims}

\code{sids}: A vector of sessions ids for the basis web-service;

\code{isReady}: A vector of Boolean values. \code{TRUE} denotes that the data has been downloaded from the basis system.

\code{results}: A data frame containing the results of the simulations. Initially this entry is empty.
}


\author{Colin Gillespie}

\seealso{See Also as \code{\link{createSettings}}.}
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
wsdl = "http://calibayes2.ncl.ac.uk:81/CalibayesServices1.wsdl"
settings = createSettings(wsdl, burn=10, thin=2)
sbml = LogisticModel

sid = calibrate(wsdl, sbml, settings, prior, asText=TRUE)
isCaliBayesReady(wsdl, sid)

posterior = getPosterior(wsdl, sid)

#Fire off the simulations
rst = getPredictiveDistribution(sbml, 5, 20, 20, posterior, prior, asText=TRUE)


#Each call to updatePredictiveResults checks for any simulations that
#have finished. If a simulation has finished, then it is appended to the
#results data.frame
rst=updatePredictiveResults(rst)
    
    
  }

}

\keyword{htest}


