%$Date$
%$Author$
\name{calibayesR-package}
\alias{calibayesR-package}
\alias{calibayesR}
\alias{calibayesr}

\docType{package}

\title{Integration of GRID-based post-genomic data resources through Bayesian calibration of biological simulators}

\description{

The CaliBayes project studies how to overcome the computational difficulties encountered when estimating
parameter values in stochastic kinetic models.  The CaliBayes tools are also suitable for parameter inference in deterministic models observed with
error. The method works essentially through a stochastic comparison of simulated model output obtained from different parameter values with
what is often very noisy biological experimental data.  The project uses Bayesian methods to provide posterior distributions for parameter
values which describe uncertainty about their true values. These distributions provide a more natural representation of knowledge about
parameters than do, for example, point estimates obtained from maximum likelihood or least-squares methods. Also Bayesian methods are
particularly suited to making inferences in complex stochastic biological models using partially observed time course data. Much of biological data is partially observed whether
it consists of a continuous process measured at a few time points or
whether key variables or components in the model are not observed at
all. The modelling framework we use allows for the underlying
biological model to be a deterministic model or a stochastic
model. The observational (stochastic) error model describes the
(random) discrepancies between model outputs and experimental
data. Put together, these models describe a stochastic model for the
experimental data and it is this overall stochastic model that is
calibrated to the data. Another feature of CaliBayes is that it allows
prior information about modelled parameters to be used to optimise
inferences. This information can obtained from, for example, an
analysis of similar data using a simplified model structure and
experimental measurement error. Additionally, distributional
information can be included about the initial levels of model
variables.

The goal of this project is to provide a complete suite of tools
necessary for performing Bayesian parameter inference in biological
models. These include: (i) an R package for formatting experimental
data and the user's prior beliefs about parameter values and initial
conditions of model variables (typically initial species values or
concentrations); (ii) web service tools for forward simulation of
these models (deterministically, stochastically or by using hybrid
methods) and (iii) tools for parameter inference.  This
package also provides an interface to
the CaliBayes web services within the R computing environment. Novel
methods for Bayesian inference in stochastic biological models have
been developed and tested by our group. The
CaliBayes project exploits these techniques, together with significant
associated computing power, and makes them available for public access
via web services.
}

\author{
Maintainer: Colin Gillespie <c.gillespie@ncl.ac.uk>
}
\references{See \url{http://www.calibayes.ncl.ac.uk/}}


\keyword{package}
\keyword{distribution}

\seealso{\code{\link{createCalibayes}}, \code{link{calibrate}}}


