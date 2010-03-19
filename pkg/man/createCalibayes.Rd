%$Date$
%$Author$
\name{createCalibayes}
\alias{createCalibayes}
\alias{saveCalibayes}
\alias{loadCalibayes}
\alias{mean.CaliBayes}
\alias{summary.CaliBayes}
\alias{plot.CaliBayes}
\alias{compareDistributions}


\title{Creates a CaliBayes object.}

\description{These functions enable the creation of the calibayes object that is used by the R web-services. The \code{saveCaliBayes} function constructs XML files for other web-service tools to use.}

\usage{
createCalibayes(parameters, species, distributions, errors, experiments)
saveCalibayes(filename, calibayes)
loadCalibayes(filename)
compareDistributions(prior, posterior)
}


\arguments{
    \item{parameters}{Optional argument. A data frame containing the prior specification of the estimated parameters. This is technically optional, but in almost all cases, will be needed. If a parameter is not entered in this data frame, but is present in the model, then CaliBayes will use the model value.}
    \item{species}{A data frame containing the prior distribution of the species at time t=0. If a species is not entered in this data frame, but is present in your model, then CaliBayes will use the initial conditions specified in the SBML file and assume no uncertainty exists in the initial conditions.}
    \item{distributions}{A vector specifying the error distribution of the species. Currently the distribution supported by CaliBayes are \code{Gaussian}.}
    \item{errors}{A data frame containing the prior specification of the errors. The numbers of columns must be equal to the number of columns in the species data frame. Each column in the error data frame has the format: species\_id.error\_parameter . At present we only support Gaussian error structures, so error\_parameter = tau.}
    \item{experiments}{A list of data frames containing the experiments. Each data frame have three columns: time, species and value.}
    \item{filename}{The filename where the XML string will be written. If filename=stdout(), the XML string is printed to the screen.}
    \item{calibayes}{A calibayes object, created by the \code{createCalibayes} function.}
    \item{prior}{A calibayes object, created by the \code{createCalibayes} or \code{loadCalibayes} function.}
    \item{posterior}{A calibayes object, created by the \code{createCalibayes} or \code{loadCalibayes} function.}
}

\value{
    \code{createCalibayes}: Returns a calibayes object. This object is used by the \code{calibrate} web-service.
    
    \code{saveCalibayes}: This function may create two files: filename_distributions.xml containing your prior specifications and filename_experiments.xml containing your experimental data. You should use this function if you want to store your results or if you want to create the CaliBayes XML files.
    
    \code{loadCalibayes}: Returns a calibayes object. This function will not load experimental data.
    }

\author{Colin Gillespie}

\seealso{See also  \code{\link{calibrate}} and \code{\link{createSettings}}.}
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
    prior = createCalibayes(parameters, species, distributions, errors, DecayData)

    mean(prior)
    summary(prior)
    plot(prior)

    #Creates two files: decay_distributions.xml and decay_experiments.xml
    saveCalibayes("decay", prior)
        
    #Load in the distribution file
    prior = loadCalibayes("decay_distributions.xml")
    }
}

\keyword{htest}

