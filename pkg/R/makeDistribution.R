#$Date$
#$Author$


makeDistribution = function(filename=FALSE, parameters, species, distributions, errors, check=TRUE)
{
    #Only parameters are optional
    #Next few lines are over kill, but can be easily expanded.
    mf = match.call(expand.dots = FALSE)
    m = match(c("parameters"), names(mf), 0L)
    mf = mf[c(1L, m)]
    if(!is.element("parameters",names(mf)))
        parameters = FALSE
    
    if(check)
        checkDistribution(parameters, species, distributions, errors)
    
    top=newXMLNode("Distribution")
    var=newXMLNode("ListOfVariables",parent=top)
    seq=newXMLNode("sequence",parent=var)
    
    if(is.data.frame(parameters))
    {
        df_pars = fixParameters(parameters)
        pars = as.character(unique(df_pars$id))
        lapply(pars, makeDistributionParameters, seq)
    }

    df_sp = fixSpecies(species)
    apply(data.frame(colnames(species), distributions, c(species[1,],recursive=T)),  1, makeDistributionSpecies, seq)
    n = dim(species)[1]
    df_er = fixErrors(errors)
    
    lOfIters=newXMLNode("ListOfIterations",parent=top)
    listofnodes = c()
    for(i in 1:n){
        node=newXMLNode("Iteration",attrs=c(number=as.numeric(i)),parent=lOfIters)
        seq1=newXMLNode("sequence",parent=node)
        listofnodes = c(listofnodes, seq1)
    }
    
    if(is.data.frame(parameters))
        apply(df_pars, 1, makeparamnode, listofnodes)
 
    apply(df_sp, 1, makespecnode,listofnodes, df_er)

    if(is.character(filename)){
        saveXML(top, filename)
        rt_value = TRUE
    }else{
        rt_value = saveXML(top)
    }
    return(rt_value)
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

makeDistributionParameters = function(id, seq)
{
    newXMLNode("Parameter", parent=seq, attrs=c(id=id))
}

makeDistributionSpecies = function(row, seq)
{
    if(is.na(row[[3]]))
        newXMLNode("Species", parent=seq,attrs=c(id=row[[1]], error=row[[2]], hasValue='False'))
    else
        newXMLNode("Species", parent=seq,attrs=c(id=row[[1]], error=row[[2]], hasValue='True'))
}

makeparamnode = function(row, listofnodes)
{
    par = listofnodes[[as.numeric(row[1])]]
    newXMLNode("Parameter", attrs=c(value=as.numeric(row[2])), parent=par)
    return()
}

makespecnode = function(row, listofnodes, df_er)
{
    par = listofnodes[[as.numeric(row[1])]]
    if(is.na(row[2]))
    {
        node = newXMLNode("Species", parent=par)
    }else{
        node = newXMLNode("Species", attrs=c(value=as.numeric(row[2])), parent=par)
    }
    errors = df_er[df_er$species == row[3] & df_er$iters == as.numeric(row[1]) ,]
    if(dim(errors)[1] > 0){
        seq=newXMLNode("sequence",parent=node)
        apply(errors, 1, makeerrornode, seq)
    }
}


makeerrornode = function(row, node)
{
    newXMLNode("Error",attrs=c(value=as.numeric(row[2])), parent=node)
}
    


############################################
#Example usage
############################################

#n=10
#parameters=data.frame(mu=runif(n,0.01,0.1),alpha=runif(n,0.1,1))
#species=data.frame(X=rnorm(n,50,1), Y=rnorm(n, 100, 10), Z=rnorm(n, 150, 10))
#distributions = c('Gaussian', 'Gaussian', "Gaussian")
#errors = data.frame(X.tau=rnorm(n,10), X.tau1=rnorm(n,10),Y.p2=rnorm(n, 100))
#
#makeDistribution(stdout() , parameters, species, distributions, errors)










