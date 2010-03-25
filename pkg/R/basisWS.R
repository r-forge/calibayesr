
######################################
#Private functions
######################################

removeXMLHeader = function(sbml)
{
    #Remove the XML heading
    sbml = strsplit(sbml,'\\?>')[[1]][[(length(sbml)+1)]]
    #Remove the leading \n
    if(substr(sbml,1,1)=="\n")
        sbml = substr(sbml, 2, nchar(sbml))
    return(sbml)
}

#getListOfSpeciesIds = function(model)
#{      
#    listOfSpecies = xmlChildren(model)$listOfSpecies
#    sbmlNodes = xmlChildren(listOfSpecies)
#    
#    sp_ids = vector()
#    for(j in 1:length(sbmlNodes))
 #   {
#        node = sbmlNodes[[j]]
#        sp_ids[j] = xmlAttrs(node)["id"]
#    }
#    return(sp_ids)
#}   
  
generateNodeRefs = function(model, prior, posterior)
{
    node_refs = list()


    listOfSpecies = xmlChildren(model)$listOfSpecies
    sbmlNodes = xmlChildren(listOfSpecies)

    IDs2change = colnames(prior$species)
    node_refs = getNodeRefs(node_refs, sbmlNodes, IDs2change)


    listOfParameters = xmlChildren(model)$listOfParameters
    IDs2change = colnames(posterior$parameters)
    sbmlNodes = xmlChildren(listOfParameters)
    node_refs = getNodeRefs(node_refs, sbmlNodes, IDs2change)

    return(node_refs)
}

getNodeRefs = function(node_refs, sbmlNodes, IDs2change)
{
    for(j in 1:length(sbmlNodes))
    {
        node = sbmlNodes[[j]]
        node_id = xmlAttrs(node)["id"]
        if(is.element(node_id, IDs2change))
        {
            node_refs[[node_id]] = NULL
            node_refs[[node_id]] = node
        }
    }
    return(node_refs)
}


######################################
#Public functions
######################################

getPredictiveDistribution = function(sbml, max_time, iters, no_of_sims, prior, posterior, asText=FALSE)
{
    
    sbmlModel = xmlInternalTreeParse(sbml, asText=asText)
    
    
    sbml = xmlChildren(sbmlModel)$sbml
    model = xmlChildren(sbml)$model
    
    node_refs = generateNodeRefs(model, prior, posterior)
     
    sps2change = colnames(prior$species)
    pars2change = colnames(posterior$parameters)
    no_of_parts = dim(prior$parameters)[1]
    
    
    indxs = vector()
    sids = vector()
    for(i in 1:no_of_sims)
    {
        indx = sample(seq(1,no_of_parts), 1, replace=TRUE)
        sp_values = prior$species[indx,]
        for(j in 1:length(sps2change))
        {
            node = node_refs[[sps2change[j]]]
            xmlAttrs(node)["initialAmount"] = round(sp_values[j])
        }
        
        pars_values = posterior$parameters[indx,]
        for(j in 1:length(pars2change))
        {
            node = node_refs[[pars2change[j]]]
            xmlAttrs(node)["value"] = pars_values[j]
        }
        s = toString.XMLNode(sbmlModel)
        sid = forwardSimulate(s, max_time, iters, asText=TRUE)
        sids[i] = sid
        indxs[i] = indx
        message("Simulation ", i, " submitted")
    }
    results = list(sids=sids)
    results$indxs = indxs
    results$isReady = vector(length=length(indxs))
    
    class(results) = "CaliBayesPredictive"
    return(results)
}


updatePredictiveResults = function(results)
{
    rst = results
    rst2get = rst$sids[ !rst$isReady]
    for(i in 1: length(rst2get))
    {
        sid = rst2get[i]
        if(isForwardSimReady(sid))
        {
            message("Getting results for job ", sid)
            sim_rst = getForwardSimResult(sid)
            rst$isReady[i] = TRUE
            sim_df = as.data.frame(sim_rst)
            colnames(sim_df) = c("Time", rst$sp_ids)
            sim_df$sim_no = rep(i, length(sim_df$Time))
            
            if(is.null(rst$simulations))
                rst$simulations = sim_df
            else
                rst$simulations  = rbind(rst$simulations, sim_df)
            
            rst$isReady[i] = TRUE
        }
    }
    return(rst)
}
      

        
