#$Date$
#$Author$


makeExperiments = function(filename=FALSE, experiments, species, check=TRUE)
{ 
    if(check)
    {
        checkExperiments(experiments, species)
    }

    top = newXMLNode("listOfExperiments")
    seq = newXMLNode("sequence", parent=top)
    lapply(experiments, makeExperiment, seq)
    if(is.character(filename)){
        saveXML(top, filename)
        rt_value = TRUE
    }else{
        rt_value = saveXML(top)
    }
    return(rt_value)
}

makeExperiment = function(exp_df, par)
{
    exper = newXMLNode("Experiment", parent=par)
    listof = newXMLNode("listOfObservations", parent=exper)
    
    #Make sure columns are in the correct order!
    switch_cols = data.frame(species = exp_df$species, time = exp_df$time, value = exp_df$value)
    
    #Order by time
    switch_cols = switch_cols[order(switch_cols$time), ]
    apply(switch_cols, 1, makeObservation, listof)
}

makeObservation = function(row, par)
{
    newXMLNode("Observation", parent=par, attrs=c(species=row[[1]], time=as.numeric(row[[2]]), value=as.numeric(row[[3]])))
}


############################################
#Example usage
############################################

#exp1 = data.frame(time = seq(10), species = rep(c('X'), each=10), value=runif(10))
#exp2 = data.frame(time = seq(5), species = rep(c('X','Y'), each=5), value=rep(c(runif(5, 0, 1), runif(5, 10, 12))))
#makeExperiments(stdout(), list(exp1, exp2))

