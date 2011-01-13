#Demo for YeastGrowthDemo
library(calibayesR)
library(SBMLModels)
data(LogisticModel)

#Which simulators are available
wsdl = "http://calibayes1.ncl.ac.uk:81/CaliBayesService_v2.wsdl"
listSimulatorMethods(wsdl)

#Create settings file
settings = createCaliBayesSettings(wsdl, burn=10, thin=2, simulator="copasi-deterministic")

#Just use the first experiment and a copy of points for illustration
yeast_data = YeastGrowth[[1]][1:3,]

#Plot of the experiment data for the simple decay model
plot(yeast_data$time, yeast_data$value, xlab="Time", ylab="Population level")

#yeast_data$value are observations with error, 
#where the error has a Normal distribution with mean 0, and precision tau

#The number of particles
#n increases with the number of parameters
n = 100

#Create prior for the initial species level
species=data.frame(S=rlnorm(n,3,sqrt(0.02)))

#Create prior distribution for the measurement error structure
distributions = c('Gaussian')
errors = data.frame(S.tau=rlnorm(n,- 11, 1))

#Create priors for the parameters
parameters = data.frame(K=rlnorm(n,9,0.2), r=rlnorm(n,1,0.6))

#Create a distribution object that contains our prior
prior = createCaliBayesDistribution(parameters, species, distributions, errors, FALSE)
plot(prior)

#Create a Experiment object
experiments = createCaliBayesExperiment(yeast_data, species)

sid = calibrate(wsdl, LogisticModel, settings, experiments, prior, asText=TRUE)

isCaliBayesReady(wsdl, sid)
isCaliBayesFinished(wsdl, sid, 10, 10)

#Retrieve the posterior from CaliBayes
posterior = getPosterior(wsdl, sid)

#Check the output
plot(posterior)

#Compare the posterior to the prior
compareDistributions(prior, posterior)

