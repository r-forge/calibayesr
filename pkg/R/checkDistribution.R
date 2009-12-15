#$Date$
#$Author$



checkDistribution = function(parameters, species, distributions, errors)
{
    message("Checking species values...", appendLF=FALSE)
    lapply(species, checkSpeciesValues)
    message("OK")
    
    checkDistributionLength(distributions, species)
    checkDistributionVector(distributions)
    checkErrorSpecies(species, errors)
    checkErrorParameters(distributions, errors)
    checkSize(species)
    if(is.data.frame(parameters))
        checkParameterNames(parameters)

    message("All tests passed")
    
    return(TRUE)

}

checkParameterNames = function(parameters)
{
    message("Checking parameter names...", appendLF=FALSE)
    if(length(unique(colnames(parameters))) != length(colnames(parameters)))
        stop("\n parameter names should be unique")

    message("OK")
}
    
checkSpeciesValues = function(species)
{
    if(length(species[!is.na(species)])== 0)
        return();
    if(length(species[species < 0]))
        warning("Some species have negative values\n")
}


checkDistributionLength = function(distributions, species)
{
    message("Checking distribution and species lengths...", appendLF=FALSE)
    if(length(distributions) != dim(species)[2])
        stop("\n You have ",length(distributions), " distributions, but ",dim(species)[2], " species\n")

    message("OK")
}

checkErrorParameters = function(distributions, errors)
{
    message("Checking error distribution parameters...", appendLF=FALSE)

    split_names = sapply(colnames(errors), strsplit, "\\.")
    err_pars = vector();
    for(i in 1:length(split_names))
    {
        err_pars[i] = split_names[[i]][2]  
    }
    test_pars = is.element(err_pars, 'tau')

    if(length(err_pars[!test_pars]) > 0)
    {
        stop("\n The distribution vector is incorrect. At present we only support Guassian errors, with precision parameter tau.",
            "\n Check the following distribution parameters: ",
            paste(err_pars[!test_pars], collapse=' ')
        )
    }
    message("OK")
}

checkErrorSpecies = function(species, errors)
{
    
    message("Checking species in the error distribution...", appendLF=FALSE)

    sps = colnames(species)
    split_names = sapply(colnames(errors), strsplit, "\\.")
    err_sps = vector();
    for(i in 1:length(split_names))
    {
        err_sps[i] = split_names[[i]][1]  
    }
    test_sp = is.element(err_sps, sps)

    if(length(err_sps[!test_sp]) > 0)
    {
        stop("\n The species ids in the error data frame do not match the species data frame. Check the following species ids: ",
                paste(err_sps[!test_sp], collapse=' ')
        )
    }
    
    message("OK")
}


checkDistributionVector = function(distributions)
{
    message("Checking distributions vector...", appendLF=FALSE)

    allowed_dist = c('Gaussian')
    test_dist = is.element(distributions, allowed_dist)
    if(length(distributions[!test_dist]) > 0)
    {
        stop("\n\ Incorrect distributions specified: ", 
            paste(distributions[!test_dist], collapse=' '),
            "\n Correct distributions are: ",
            paste(allowed_dist), collapse=' ')
    }
    message("OK")

}


checkSize = function(parameters)
{   
    message("Checking number of particles...", appendLF=FALSE)
    
    s = dim(parameters)[1]
    if(s < 10000)
        warning("Your prior only contains ", s, " particles. The recommended number of particles is 10,000." )  
    message("OK")    
}






