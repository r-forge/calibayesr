%$Date$
%$Author$
\name{createCaliBayesExperiment}
\alias{createCaliBayesExperiment}
\alias{saveCaliBayesExperiment}


\title{CaliBayes Experiment objects}

\description{These functions enable the creation of the CaliBayes Experiment object that is used by the R web-services. The \code{saveCaliBayesExperiments} function constructs a XML file for other web-service tools to use.}

\usage{
createCaliBayesExperiment(experiments, species, check=TRUE)
saveCaliBayesExperiment(filename, CaliBayes.Experiment)

}


\arguments{
    \item{experiments}{A list of data frames containing the experiments. Each data frame has three columns: time, species and value.}
    \item{species}{A data frame containing the prior distribution of the species at time t=0. This value is only need if \code{check=TRUE}.}
    \item{filename}{The filename where the XML string will be written. If \code{filename=stdout()}, the XML string is printed to the screen.}
    \item{CaliBayes.Experiment}{A CaliBayes Experiment object, created by the \code{createCaliBayesExperiment} function.}
    \item{check}{Indicates whether basic checks should be carried out.}
}

\value{
    \code{createCaliBayesExperiment}: Returns a CaliBayes Experiment object. This object is used by the \code{calibrate} web-service.
    
    \code{saveCaliBayesExperiment}: This function may creates an xml file of the experiment. You need this function is you wish to use other web-service clients.
    
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
    #In practice n should be much larger
    n=10

    #Create prior for the initial species level
    species = data.frame(X=rnorm(n, 50, 1))

    #Skip check 2
    experiment = createCaliBayesExperiment(DecayData)
    
    #Check that the species names in the experiment and prior match
    experiment = createCaliBayesExperiment(DecayData, species)    

    #Save the XML
    saveCaliBayesExperiment("decay_experiment.xml", experiment)
    }
}

\keyword{htest}

