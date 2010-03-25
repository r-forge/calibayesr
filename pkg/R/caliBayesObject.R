################################################
#Private functions
################################################

pri_createCaliBayesDistribution = function(iters, parameters, species, distributions, errors)
{
    y = list()
    y$iters = iters
    y$parameters = parameters
    y$species = species
    y$errors = errors
    y$dist = distributions
    class(y) = "CaliBayes.Distribution"
    return(y)
}

 
    
mean.CaliBayes.Distribution = function(x,...)
{
    x = cbind(x$parameters,  x$species, x$errors)
    mean(x)
    
}

summary.CaliBayes.Distribution = function(object,...)
{
    object = cbind(object$parameters, object$species, object$errors)
    summary(object)
}


plot.CaliBayes.Distribution = function(x, lm=400, rows=4, burnin=1, thin=1, ...)
{	
    d = cbind(x$parameters, x$species, x$errors)
    names = attr(d, "names")
    iters = x$iters

    op = par(mfrow=c(rows, 3), ask=TRUE)
    thining = (seq(0:(dim(d)[1]-1)) %% thin)
    d = d[thining==0,]
        

    for (i in 1:dim(d)[2])
    {
        plot(iters, d[[i]], main=paste("Trace plot for",names[i]),
                ylab="Value", xlab="Iteration",col=4, type='l')

        v = var(d[[i]])
               
        if (v>1e-20)
        {
            acf(d[[i]],lag.max=10,ci=0, main=paste("ACF plot for ",names[i]),col=2)
            den = density(d[[i]])
            plot(den, main=paste("Density for", names[i]), xlab="Value", col=4, lwd=3)
         
        }
        else{
            plot(0,0,type="n",main=paste("ACF plot for ",names[i]),col=2,xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
			text(0,0,"N/A",cex=3.5,col=2)
            plot(median(d[[i]]),1.0/median(d[[i]]),
                    type="h", main=paste("Density for", names[i]),
                    xlab="Value", col=4, lwd=3, ylim=c(0,1.0/median(d[[i]])),
                    ylab="Density"
                )
        }
    }
    par(op)
    NULL
}


    
pri_compareDistributions = function(prior, posterior)
{
    
    names=attr(posterior, "names")   
    for (i in 1:dim(posterior)[2]){
        
        d1=density(posterior[[names[i]]])
        d2=density(prior[[names[i]]])
        plot(d1,
            main=paste("Density for", names[i]),
            xlab="Value", col=4, lwd=3,
            xlim = range(min(d1$x,d2$x), max(d1$x,d2$x)),
            ylim = range(0, max(d1$y,d2$y))
        )
        lines(d2, col=2)
        abline(h=0)
    }
}
        
        

################################################
#Public Calibayes functions
################################################

createCaliBayesSettings = function(wsdl, burn=500, thin=50, block=1, simulator="copasi-deterministic", wsdl.simulator="internal", check=TRUE)
{
    if(check)
        checkSettings(burn, thin, block)
    
    y = list()
    y$burn = burn
    y$thin = thin
    y$block = block
    y$simulator = simulator
    y$wsdl.simulator = wsdl.simulator
    class(y) = "CaliBayes.Settings"
    return(y)
}

createCaliBayesExperiment = function(experiments, species, check=TRUE)
{
    mf = match.call(expand.dots = FALSE)
    m = match(c("species"), names(mf), 0L)
    mf = mf[c(1L, m)]
    if(!is.element("species",names(mf)))
        species = FALSE


    if(is.data.frame(experiments)){
        experiments = list(experiments)
    }
    
    if(check)
        checkExperiments(experiments, species)
      
    y = experiments
    class(y) = "CaliBayes.Experiment"
    return(y)
}
 
createCaliBayesDistribution = function(parameters, species, distributions, errors, check=TRUE)
{
    mf = match.call(expand.dots = FALSE)
    m = match(c("parameters"), names(mf), 0L)
    mf = mf[c(1L, m)]
    if(!is.element("parameters",names(mf)))
        parameters = FALSE

    if(check)
        checkDistribution(parameters, species, distributions, errors)
        
    iters = seq(0, (dim(species)[1]-1))
    y = pri_createCaliBayesDistribution(iters, parameters, species, distributions, errors)
    
    return(y)

}



loadCaliBayesDistribution = function(filename)
{
    calibayes = getDistribution(filename, asText=FALSE)
    return(calibayes)
}


############################################################################################################
#Public Plotting function - compare prior to posterior
############################################################################################################
compareDistributions = function(prior, posterior)
{
    op = par(mfrow=c(2,1), ask=TRUE)
    pri_compareDistributions(prior$parameters, posterior$parameters)
    pri_compareDistributions(prior$errors, posterior$errors)       
    par(op)
    NULL 
}    



############################################################################################################
#Public Save Functions
############################################################################################################


saveCaliBayesExperiment = function(filename, CaliBayes.Experiment)
{
    makeExperiments(filename, CaliBayes.Experiment)       
    return(TRUE)
}

saveCaliBayesDistribution = function(filename, CaliBayes.Distribution)
{
    data = CaliBayes.Distribution     
    if(is.data.frame(data$species))
    {
        makeDistribution(filename, data$parameters, data$species, data$dist, data$errors, FALSE)
    }
  
    return(TRUE)
}

saveCaliBayesSettings = function(filename, CaliBayes.Settings)
{
    settings = CaliBayes.Settings
    makeSettings(filename, settings$burn, settings$thin, settings$block, 
                    settings$simulator, settings$wsdl, FALSE)
    return(TRUE)
}
    




