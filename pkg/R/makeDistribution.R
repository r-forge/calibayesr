#$Date$
#$Author$

makeDistribution = function(filename=FALSE, parameters, species, distributions, errors, check=FALSE)
{
    #Only parameters are optional
    #Next few lines are over kill, but can be easily expanded.
    mf = match.call(expand.dots = FALSE)
    m = match(c("parameters"), names(mf), 0L)
    mf = mf[c(1L, m)]
    if(!is.element("parameters",names(mf)))
        parameters = FALSE

#    if(!is.element("species",names(mf)))
#        species = FALSE
    
    if(check)
        checkDistribution(parameters, species, distributions, errors)
    
    message("\tAdding in the ListOfVariables...")
    n = dim(species)[1]
    if(is.data.frame(parameters))
    {
        df_pars = fixParameters(parameters)
        xml_str_vars = makeParametersAtListOfVariables(parameters)
        iter_pars = tapply(df_pars$value, df_pars$iters, pastePars)

    } else {#Create an empty strings for below.
        xml_str_vars = ""
        iter_pars = rep("", n)
    }
        
    xml_str_vars = paste(xml_str_vars,makeSpeciesAtListOfVariables(species, distributions))
    xml_str_vars = addXMLTag(xml_str_vars, "sequence")
    xml_str_vars = addXMLTag(xml_str_vars, "ListOfVariables")

    message("\tAdding in the ListOfIterations...may take a while")

    #Now create ListOfIterations
    df_sp = fixSpecies(species)    
    iter_sps = tapply(df_sp$value, paste(df_sp$iters, df_sp$id), pasteSpecies)

    df_er = fixErrors(errors)
    iter_errs = tapply(paste("<Error value='", df_er$value, "'/>", sep=""), paste(df_er$iters,df_er$species) , paste, collapse="")

    xml_str_iters = paste(iter_sps,"<sequence>", iter_errs, "</sequence></Species>", sep="")

    xml_str_iters[df_sp$id==df_sp$id[1]] = paste("<Iteration number='",seq(1, n), "'><sequence>", iter_pars, xml_str_iters[df_sp$id==df_sp$id[1]], sep="")
    xml_str_iters[df_sp$id==df_sp$id[length(df_sp$id)]] = paste(xml_str_iters[df_sp$id==df_sp$id[length(df_sp$id)]], "</sequence></Iteration>", sep="")
    xml_str_iters = paste(xml_str_iters, collapse="")
    xml_str_iters = addXMLTag(xml_str_iters, "ListOfIterations")


    #Combine Iterations and variables
    xml_str = paste(xml_str_vars, xml_str_iters, sep="")
    rt_values = addXMLTag(xml_str, "Distribution")


    if(is.character(filename)){
        message("Writing to file...")
        write(rt_values, filename)
        rt_values = TRUE
    }


    return(rt_values)
}

makeSpeciesAtListOfVariables = function(species, distributions)
{
    xml_str = ""
    if(is.data.frame(species)){
        sps = colnames(species)
        xml_str = paste("<Species id='", sps, "' error='", distributions, "' hasValue='True'/>", sep="", collapse="")
    }
    return(xml_str)
}

makeParametersAtListOfVariables  = function(parameters)
{    
    xml_str = ""
    if(is.data.frame(parameters)){
        pars = colnames(parameters)
        xml_str = paste("<Parameter id='", pars, "'/>", sep="", collapse="")
    }
    return(xml_str)
}

pastePars = function(parameters)
{
    paste("<Parameter value='", parameters, "'/>", collapse="" ,sep="")
}


pasteSpecies = function(species)
{
    paste("<Species value='", species, "'>", collapse="" ,sep="")
}

fixParameters = function(parameters)
{
    values = as.vector(c(parameters, recursive=TRUE))
    no_of_pars = dim(parameters)[2]
    n = dim(parameters)[1]
    names = as.character(colnames(parameters))
    df_pars = data.frame(iters = rep(seq(n),no_of_pars), value = values, id = rep(names, each=n))
    df_pars = df_pars[order(df_pars$iters), ]
    return(df_pars)
}

fixSpecies = function(species)
{
    values = as.vector(c(species,recursive=TRUE))
    no_of_sps = dim(species)[2]
    n = dim(species)[1]
    names = colnames(species)
    df_sp = data.frame(iters = rep(seq(n), no_of_sps), value = values, id = rep(names, each=n))
    df_sp = df_sp[order(df_sp$iters), ]
    return(df_sp)
}

fixErrors = function(errors)
{
    values = as.vector(c(errors, recursive=TRUE))
    no_of_errs = dim(errors)[2]
    n = dim(errors)[1]
    split_names = sapply(colnames(errors), strsplit, "\\.")
    sp_id = vector();er_id = vector();
    for(i in 1:length(split_names))
    {
        sp_id[i] = split_names[[i]][1]  
        er_id[i] = split_names[[i]][2]    
    }
    df_er = data.frame(iters = rep(seq(n),no_of_errs), value = values, id = rep(er_id, each=n),species=rep(sp_id,each=n))
    df_er = df_er[order(df_er$iters),]

    return(df_er)
}

############################################
#Example usage
############################################

#n=2
#parameters=data.frame(mu=runif(n,0.01,0.1),alpha=runif(n,0.1,1))
#species=data.frame(X=rnorm(n,50,1), Y=rnorm(n, 100, 10), Z=rnorm(n, 150, 10))
#distributions = c('Gaussian', 'Gaussian', "Gaussian")
#errors = data.frame(X.tau=rnorm(n,10), X.tau1=rnorm(n,10),Y.p2=rnorm(n, 100))
#
#makeDistribution1(stdout() , parameters, species, distributions, errors)
#n=2
#parameters=data.frame(mu=runif(n,0.01,0.1),alpha=runif(n,0.1,1))
#species=data.frame(X=rnorm(n,50,1), Y=rnorm(n, 100, 10))
#distributions = c('Gaussian', 'Gaussian')
#errors = data.frame(X.tau=rnorm(n,10), X.tau1=rnorm(n,10),Y.p2=rnorm(n, 100))#
#makeDistribution(stdout() , parameters, species, distributions, errors)

