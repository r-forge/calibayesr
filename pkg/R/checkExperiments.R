#$Date$
#$Author$

checkExperiments = function(experiments, species)
{
    
    checkExperimentHeaders(experiments)
    checkExperimentSpecies(experiments, species)
    checkExperimentTime(experiments)
    return(TRUE)
}

checkExperimentHeaders = function(experiments)
{
    message("Checking Experiment column headers ....", appendLF = FALSE)
    headers = lapply(experiments, colnames)
    headers = unique(c(headers, recursive=TRUE))
    correct_headers = c("time", "species", "value")
    bool_headers = is.element(correct_headers, headers)
    
    if(length(headers[!bool_headers]) > 0)
    {
        stop("\n\tError: Incorrect experiment column headers: ",
            paste(headers[!bool_headers], collapse=' '),
            "\n Correct headers are: time, species, value")
    }
    message("OK")
}        

getSpeciesIds = function(experiment)
{
    as.character(unique(experiment$species))
}
 
checkExperimentSpecies = function(experiments, species)
{
    message("Checking Experiment Species ....", appendLF = FALSE)
    experiment_sps = sapply(experiments, getSpeciesIds)
    experiment_sps = unique(c(experiment_sps, recursive=TRUE))

    for(i in 1:length(experiment_sps))
    {
        sp = experiment_sps[i]
        if(!is.element(sp, colnames(species)))
            warning(" No prior found for ", sp, ". This may be correct - are you sure?")           
    }
    message("OK")
}


getTime = function(experiment)
{
    as.character(unique(experiment$time))
}


checkExperimentTime = function(experiments)
{
    message("Checking Experiment Time points ....", appendLF = FALSE)
    exp_tps = lapply(experiments, getTime)
    exp_tps = as.numeric(c(exp_tps,recursive=TRUE))
    if(min(exp_tps)<0)
        stop("\n Negative time points. All time points must be positive")

    message("OK")
}
    
    
    
    
# exp1 = data.frame(time = seq(10), species = rep(c('Z'), each=10), value=runif(10))
#exp2 = data.frame(time = seq(5), species = rep(c('X','Y'), each=5), value=rep(c(runif(5, 0, 1), runif(5, 10, 12))))
#check(list(exp1, exp2)), species)
  
    
    
    
    
