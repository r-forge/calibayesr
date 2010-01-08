#Demo for YeastGrowthDemo
library(calibayesR)

library(SBMLModels)
data(LogisticModel)

#Create settings file
wsdl = "http://calibayes1.ncl.ac.uk:81/CaliBayesService_v2.wsdl"
settings = createSettings(wsdl, burn=10000, thin=2)

#Just use the first experiment
yeast_data = YeastGrowth[[1]]

#Plot of the experiment data for the simple decay model
plot(yeast_data$time, yeast_data$value, xlab="Time", ylab="Population level")

#yeast_data$value are observations with error, 
#where the error has a Normal distribution with mean 0, and precision tau

#The number of particles
#n increases with the number of parameters
n = 500

#Create prior for the initial species level
species=data.frame(S=rlnorm(n,3,sqrt(0.02)))

#Create prior distribution for the measurement error structure
distributions = c('Gaussian')
errors = data.frame(S.tau=rlnorm(n,-11, 1))

#Create priors for the parameters
parameters = data.frame(K=rlnorm(n,9,0.2), r=rlnorm(n,1,0.6))

#Create a prior object
prior = createCalibayes(parameters, species, distributions, errors, yeast_data)

sid = calibrate(wsdl, LogisticModel, settings, prior, asText=TRUE)

cat("Check calibayes every thirty seconds\n")
isCaliBayesFinished(wsdl, sid, 20 , 10)


posterior = getPosterior(wsdl, sid)

plot(posterior)
summary(posterior)

compareDistributions(prior, posterior)
