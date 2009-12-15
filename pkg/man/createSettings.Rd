%$Date$
%$Author$
\name{createSettings}
\alias{createSettings}
\alias{saveSettings}

\title{Creates a CaliBayes settings object.}

\description{This function is used to create a tunings file for the CaliBayes system.}

\usage{
createSettings(wsdl, 
                burn=500, 
                thin=50, 
                block=1, 
                simulator="copasi-deterministic",
                wsdl.simulator="internal")
                
saveSettings(filename, settings)
}


\arguments{
  \item{wsdl}{A valid url pointing to the CaliBayes wsdl file.}
  \item{burn}{The number of iterations to be discarded from the MCMC run.}
  \item{thin}{For thin=k, every kth iteration is stored.}
  \item{block}{The experimental data block size.}  
  \item{simulator}{The simulator that CaliBayes uses to forward simulate. A list of simulators can be obtained by using the \code{\link{listSimulatorMethods}}}
  \item{wsdl.simulator}{A wsdl file pointing to other simulators that can be used. The default is "internal".}
  \item{filename}{The filename where the XML string will be written. If filename=stdout(), the XML string is printed to the screen.}
  \item{settings}{A settings object created by createSettings}
}

\value{
    \code{createSettings}: A CaliBayes settings object.
}

\author{Colin Gillespie}

\seealso{See also \code{\link{calibrate}} and \code{\link{createCalibayes}}.}

\examples{
    wsdl = "http://calibayes2.ncl.ac.uk:81/CalibayesServices1.wsdl"
    settings = createSettings(wsdl, burn=100, thin=10)
    
    \dontrun{
    saveSettings("settings.xml", settings)
    }
}

\keyword{htest}

