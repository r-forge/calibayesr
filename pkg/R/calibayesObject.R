################################################
#Private functions
################################################


createCalibayesObject = function(iters, par_df, sp_df, dist, err_df, exp_df)
{
    y = list()
    y$iters = iters    
    y$parameters = par_df
    y$species = sp_df
    y$errors = err_df
    y$dist = dist
    y$experiments = exp_df
    class(y) = "calibayes"
    return(y)
}
 
    
mean.calibayes = function(x,...)
{
    x = cbind(x$parameters,  x$species, x$errors)
    mean(x)
    
}

summary.calibayes = function(object,...)
{
    object = cbind(object$parameters, object$species, object$errors)
    summary(object)
}


plot.calibayes = function(x, lm=400, rows=4, burnin=1, thin=1, ...)
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


createCalibayes = function(parameters, species, distributions, errors, experiments)
{
    mf = match.call(expand.dots = FALSE)
    m = match(c("parameters", "experiments"), names(mf), 0L)
    mf = mf[c(1L, m)]
    if(!is.element("parameters",names(mf)))
        parameters = FALSE

    if(is.element("experiments",names(mf))){
        if(is.data.frame(experiments)){
            experiments = list(experiments)
        }
        checkExperiments(experiments, species)
    }else{
        experiments = NULL
    }    
    checkDistribution(parameters, species, distributions, errors)
    y = createCalibayesObject(seq(0, (dim(species)[1]-1)), parameters, species, distributions, errors, experiments)
    return(y)
}

saveCalibayes = function(filename, calibayes)
{
    data = calibayes     
    if(is.data.frame(data$species))
    {
        dis_filename = paste(filename, '_distributions.xml', sep='')
        makeDistribution(dis_filename, data$parameters, data$species, data$dist, data$errors, FALSE)
    }
    if(is.list(data$experiments))
    {
        exp_filename = paste(filename, '_experiments.xml', sep='')
        makeExperiments(exp_filename, data$experiments, check=FALSE)
    }
  
    return(TRUE)
}

loadCalibayes = function(filename)
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
#Public Settings functions
############################################################################################################
createSettings = function(wsdl, burn=500, thin=50, block=1, simulator="copasi-deterministic", wsdl.simulator="internal")
{
    checkSettings(burn, thin, block)
    
    y = list()
    y$burn = burn
    y$thin = thin
    y$block = block
    y$simulator = simulator
    y$wsdl.simulator = wsdl.simulator
    class(y) = "settings"
    return(y)
}

saveSettings = function(filename, settings)
{
    makeSettings(filename, settings$burn, settings$thin, settings$block, 
                    settings$simulator, settings$wsdl, FALSE)
}
    




