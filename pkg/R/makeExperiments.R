#$Date$
#$Author$

addXMLTag = function(value, tag)
{
    return(paste("<", tag, ">", value, "</", tag, ">", sep=""))
    
}

makeExperiments = function(filename=FALSE, experiments, species, check=FALSE)
{ 
    if(check)
    {
        checkExperiments(experiments, species)
    }

    rt_values = paste(unlist(lapply(experiments, makeExperiment)), collapse="\n")
    rt_values = addXMLTag(rt_values, "sequence")
    rt_values = addXMLTag(rt_values, "listOfExperiments")


    if(is.character(filename)){
        write(rt_values, filename)
        rt_values = TRUE
    }

    return(rt_values)
}


makeExperiment = function(exp_df)
{
    #Order by time
    exp_df = exp_df[order(exp_df$time), ]
    exp_xml = paste("<Observation species='", exp_df$species, "' time='", exp_df$time, "' value='", exp_df$value, "'/>", sep="", collapse="")
    return(paste("<Experiment>", exp_xml, "</Experiment>", sep=""))
}



############################################
#Example usage
############################################

#exp1 = data.frame(time = seq(10), species = rep(c('X'), each=10), value=runif(10))
#exp2 = data.frame(time = seq(5), species = rep(c('X','Y'), each=5), value=rep(c(runif(5, 0, 1), runif(5, 10, 12))))
#makeExperiments(FALSE, list(exp1, exp2))
