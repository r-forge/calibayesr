#$Date$
#$Author$

#To Do
#Extract the error values and types in getDataFrame


getDistribution = function(xmlData, asText=TRUE)
{

    cali = new.env()
    cali$sp_ids = vector()
    cali$sp_values = vector()
    cali$pa_ids = vector()
    cali$pa_values = vector()

    cali$iterations = vector()
    cali$errors = vector()
    environment(getSpecies) = cali
    environment(getParameters) = cali
    environment(getIterations) = cali
    environment(getErrors) = cali    

    s1 = xmlEventParse(xmlData, handlers = list(Species = getSpecies, Parameter = getParameters, Iteration=getIterations, Error=getErrors),asText=asText)

    sp_ids = environment(s1$Species)$sp_ids
    sp_values = environment(s1$Species)$sp_values

    pa_ids = environment(s1$Parameter)$pa_ids
    pa_values = environment(s1$Parameter)$pa_values

    iterations = environment(s1$Iteration)$iterations
    errors = environment(s1$Error)$errors

    if(length(pa_ids))
    {
        l = length(pa_values)/length(pa_ids)
        par_df = data.frame(parameters = rep(pa_ids, l), values = pa_values)
        par_df = makeParametersDf(par_df, iterations)
    }else{
        par_df = data.frame()
    }
    
    if(length(sp_ids))
    {
        l = length(sp_values)/length(sp_ids)
    
        sp_df = data.frame(species = rep(sp_ids, l), values=sp_values)
        sp_df = makeSpeciesDf(sp_df, iterations)
    
        #TODO: Need to alter for non-Gaussian errors
        #Just need to check the distribution.

        err_df = data.frame(species = rep(sp_ids, l), values=errors)    
        err_df = makeErrorsDf(err_df, iterations)
        dist = rep('Gaussian', dim(err_df)[2])
    }else{
        sp_df = data.frame()
        err_df = data.frame()
    }
    y = pri_createCaliBayesDistribution(iterations, par_df, sp_df, dist, err_df)
    return(y)
}

getSpecies = function(name, attrs, ...){

    id = attrs["id"]
    if(!is.na(id))
        cali[["sp_ids"]] = append(cali[["sp_ids"]], id)
        
    value = attrs["value"]
    if(!is.na(value))
        cali[["sp_values"]] = append(cali[["sp_values"]], as.double(value))

}

getParameters = function(name, attrs, ...){
    id = attrs["id"]
    if(!is.na(id))
        cali[["pa_ids"]] = append(cali[["pa_ids"]], id)
        
    value = attrs["value"]
    if(!is.na(value))
        cali[["pa_values"]] = append(cali[["pa_values"]], as.double(value))

}


getIterations = function(name, attrs, ...){
    iteration = attrs["number"]
    if(!is.na(iteration))
        cali[["iterations"]] = append(cali[["iterations"]], as.double(iteration))
#        assign("iterations", append(iterations, as.integer(iteration)), envir = cali)
}


getErrors = function(name, attrs, ...)
{
    error = attrs["value"]
    if(!is.na(error))
        cali[["errors"]] = append(cali[["errors"]], as.double(error))

}




makeSpeciesDf = function(df_sp, iters)
{
    new_df = df_sp[order(df_sp$species), ] 
    species = unique(new_df$species)

    v = new_df$values
    dim(v) = c(length(iters), length(species))
    
    v = as.data.frame(v)
    colnames(v) = species
    return(v)
}



makeParametersDf = function(df_par, iters)
{
    
    new_df = df_par[order(df_par$parameters), ] 
    parameters = unique(new_df$parameters)

    v = new_df$values
    dim(v) = c(length(iters), length(parameters))
    
    v = as.data.frame(v)
    colnames(v) = parameters
   
    return(v)
}


makeErrorsDf = function(df_er, iters)
{
    #XXX: We only handle Gaussian errors
    #So we don't have to check the XML file for the distributions used
    #Update in the future
    
    new_df = df_er[order(df_er$species), ] 
    species = unique(new_df$species)
    error_ids = sapply(species, paste, ".tau",sep='')
    
    v = new_df$values
    dim(v) = c(length(iters), length(error_ids))
    
    v = as.data.frame(v)
    colnames(v) = error_ids
    return(v)
}







