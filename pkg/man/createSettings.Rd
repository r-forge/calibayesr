%$Date$
%$Author$
\name{createCaliBayesSettings}
\alias{createCaliBayesSettings}
\alias{saveCaliBayesSettings}


\title{Creates a CaliBayes settings object.}

\description{This function is used to create a tunings file for the CaliBayes system.}

\usage{
createCaliBayesSettings(wsdl, 
                burn=500, 
                thin=50, 
                block=1, 
                simulator="copasi-deterministic",
                wsdl.simulator="internal",
                check=TRUE)
                
saveCaliBayesSettings(filename, CaliBayes.Settings)
}


\arguments{
  \item{wsdl}{A valid url pointing to the CaliBayes wsdl file.}
  \item{burn}{The number of iterations to be discarded from the MCMC run.}
  \item{thin}{For thin=k, every kth iteration is stored.}
  \item{block}{The experimental data block size.}  
  \item{simulator}{The simulator that CaliBayes uses to forward simulate. A list of simulators can be obtained by using the \code{\link{listSimulatorMethods}}}
  \item{wsdl.simulator}{A wsdl file pointing to other simulators that can be used. The default is \code{"internal"}.}
  \item{filename}{The filename where the XML string will be written. If \code{filename=stdout()}, the XML string is printed to the screen.}
  \item{CaliBayes.Settings}{A settings object created by createCaliBayesSettings}
  \item{check}{Indicates whether basic checks should be carried out on your settings.}
}

\value{
    \code{createSettings}: A CaliBayes settings object.
}

\author{Colin Gillespie}

\seealso{See also \code{\link{calibrate}} and \code{\link{createCaliBayesDistribution}}.}

\examples{
    \dontrun{
    wsdl = "http://calibayes1.ncl.ac.uk:81/CaliBayesService_v2.wsdl"
    simulators = listSimulatorMethods(wsdl)
    simulators
    
    #Use the first simulator on the list
    settings = createCaliBayesSettings(wsdl, burn=100, thin=10, simulator=simulators[1])
    
    saveCaliBayesSettings("settings.xml", settings)
    }
}

\keyword{htest}

