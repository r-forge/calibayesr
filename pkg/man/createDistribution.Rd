%$Date$
%$Author$
\name{createCaliBayesDistribution}
\alias{createCaliBayesDistribution}
\alias{saveCaliBayesDistribution}
\alias{loadCaliBayesDistribution}

\alias{mean.CaliBayes.Distribution}
\alias{summary.CaliBayes.Distribution}
\alias{plot.CaliBayes.Distribution}

\alias{compareDistributions}


\title{CaliBayes Distribution objects}

\description{These functions enable the creation of the calibayes object that is used by the R web-services. The \code{saveCaliBayesDistribution} function constructs an XML files for other web-service tools to use.}

\usage{
createCaliBayesDistribution(parameters, 
                            species, 
                            distributions, 
                            errors, 
                            check=TRUE)
saveCaliBayesDistribution(filename, CaliBayes.Distribution)
loadCaliBayesDistribution(filename)
compareDistributions(prior, posterior)
}


\arguments{
    \item{parameters}{Optional argument. A data frame containing the prior specification of the estimated parameters. This is technically optional, but in almost all cases, will be needed. If a parameter is not entered in this data frame, but is present in the model, then CaliBayes will use the SBML model value.}
    \item{species}{A data frame containing the prior distribution of the species at time t=0. If a species is not entered in this data frame, but is present in your model, then CaliBayes will use the initial conditions specified in the SBML file and assume no uncertainty exists in the initial conditions.}
    \item{distributions}{A vector specifying the error distribution of the species. Currently the distribution supported by CaliBayes are \code{Gaussian}.}
    \item{errors}{A data frame containing the prior specification of the errors. The numbers of columns must be equal to the number of columns in the species data frame. Each column in the error data frame has the format: \code{species_id.error_parameter} . At present we only support Gaussian error structures, so \code{error_parameter} \code{= tau}.}
    \item{filename}{The filename where the XML string will be written. If \code{filename=stdout()}, the XML string is printed to the screen.}
    \item{CaliBayes.Distribution}{A CaliBayes object, created by the \code{createCaliBayesDistribution} function.}
    \item{prior}{A CaliBayes Distribution object, usually created by the \code{createCaliBayesDistribution} or \code{loadCaliBayesDistribution} function or return from CaliBayes.}
    \item{posterior}{A CaliBayes Distribution object, usually created by the \code{createCaliBayesDistribution} or \code{loadCaliBayesDistribution} function or return from CaliBayes.}
    \item{check}{Indicates whether basic checks should be carried out.}
}

\value{
    \code{createCaliBayesDistribution}: Returns a CaliBayes Distribution object. This object is used by the \code{calibrate} web-service.
    
    \code{saveCaliBayesDistribution}: This function may creates an xml file of the distribution. You should use this function if you want to store your results or if you want to create the CaliBayes XML files.
    
    \code{loadCaliBayesDistribution}: Returns a CaliBayes  Distribution object. This function will not load experimental data.
    }

\author{Colin Gillespie}

\seealso{See also  \code{\link{calibrate}} and \code{\link{createCaliBayesSettings}}.}
\examples{
    #This is an example for the simple decay model
    #The model has a single species, X and a decay reaction 
    #with rate mu*X
    \dontrun{
    library(SBMLModels)
    data(Decay)
    #Plot of the experiment data for the simple decay model
    plot(DecayData$time, DecayData$value, xlab="Time", ylab="Population level")
    
    #Decay$value are observations with error, 
    #where the error has a Normal distribution with mean 0, and precision tau

    #In practice n should be much larger
    n=10

    #Create prior for the initial species level
    species = data.frame(X=rnorm(n, 50, 1))

    #Create prior for the measurement error structure
    distributions = c('Gaussian')
    errors = data.frame(X.tau=rnorm(n, 1, 0.01))

    #Create priors for the parameters
    parameters = data.frame(mu=runif(n,0.01,0.2))

    #Create prior object
    prior = createCaliBayesDistribution(parameters, species, distributions, errors)

    mean(prior)
    summary(prior)
    plot(prior)

    #Save the XML file
    saveCaliBayesDistribution("decay_distributions.xml", prior)
        
    #Load in the distribution file
    prior = loadCaliBayesDistribution("decay_distributions.xml")
    }
}

\keyword{htest}

