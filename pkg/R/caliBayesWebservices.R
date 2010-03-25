require(SSOAP)

##########################################################
#Private methods
########################################################
getCaliDef = function(wsdl, verbose=TRUE)
{
    cali.wsdl = processWSDL(wsdl)
    cali.def = genSOAPClientInterface(def = cali.wsdl, verbose = verbose)
    return(cali.def)
}


RSubmit = function(from) 
{
    obj = new("rSubmit")
    obj@inputs = from
    obj
}

# isReady
pri_isReady= function(from) 
{
    obj = new("isReady")
    obj@sessionId = from
    obj
}


# getResult
getResult= function(from) 
{
    obj = new("getResult")
    obj@sessionId = from
    obj
}

# discard
Discard= function (from) 
{
    obj = new("discard")
    obj@sessionId = from
    obj
}

getSessions= function (from) 
{
    obj = new("getSessions")
    obj@sessionId = from
    obj
}


# simMethods
simMethods = function(from) 
{
    obj = new("getAvailableSimMethods")
    obj
}

pri_getUpdate = function(from)
{
   obj = new("getExecReport")
   obj@sessionId = from
   obj
}


##########################################################
#Public methods
########################################################

calibrate = function(wsdl, sbml, CaliBayes.Settings, CaliBayes.Experiment, CaliBayes.Distribution, asText=FALSE) 
{
    
    
    if(!asText)
        sbml = toString.XMLNode(xmlInternalTreeParse(sbml))
    
    #Remove the XML heading
    sbml = strsplit(sbml,'\\?>')[[1]][[2]]
    
    message("Making SOAP interface...")
    cali.def = getCaliDef(wsdl, FALSE)
    message("Finished SOAP interface\n\n")
    
    message("Making Settings XML...")
    settings = CaliBayes.Settings
    tuningXML = makeSettings(FALSE, settings$burn, settings$thin, settings$block, settings$simulator, settings$wsdl, FALSE)

    message("Making Experiment XML...")
    expDataXML = makeExperiments(FALSE, CaliBayes.Experiment)    

    message("Making Distribution XML...")
    distributionXML  = makeDistribution(FALSE, 
                                        CaliBayes.Distribution$parameters, 
                                        CaliBayes.Distribution$species, 
                                        CaliBayes.Distribution$dist, 
                                        CaliBayes.Distribution$errors, 
                                        FALSE)
    
    
    req = paste("<doc>", sbml, expDataXML, tuningXML, distributionXML, "</doc>", sep="")
    req = gsub('\n','',req)
    
    setAs("character", "rSubmit", RSubmit,  where=globalenv())
    message("Sending SOAP message to CaliBayes...")

    rst = cali.def@functions$rSubmit(req)
    return(rst@sessionId)
}


getUpdate = function(wsdl, sid)
{
    cali.def = getCaliDef(wsdl, FALSE)

    setAs("character", "getExecReport", pri_getUpdate,  where=globalenv())
    rst = cali.def@functions$getExecReport(sid)
    return(rst@report)
}


isCaliBayesReady = function(wsdl, sid)
{
    cali.def = getCaliDef(wsdl, FALSE)

    setAs("character", "isReady", pri_isReady,  where=globalenv())
    param = c(sid)
    rst = cali.def@functions$isReady(param)
    return(rst@status)
}

getPosterior = function(wsdl, sid)
{
    r = isCaliBayesReady(wsdl, sid)
    if(!r)
    {
        cat('Calibration has not finished\n')
        return(FALSE)
    }
    cali.def = getCaliDef(wsdl, FALSE)   
    setAs("character", "getResult", getResult,  where=globalenv())
    message("Sending request to CaliBayes...")
    rst = cali.def@functions$getResult(sid)
    message("Converting to CaliBayes Objects...")
    xmlString = rst@result
    result = getDistribution(xmlString)
    
    return(result)
}
   
listSimulatorMethods = function(wsdl)
{
    cali.def = getCaliDef(wsdl, FALSE)   
    setAs("character", "getAvailableSimMethods", simMethods, where=globalenv())
    rst = cali.def@functions$getAvailableSimMethods('')
    simulators = vector(length = length(names(rst)))
    for(i in 1:length(names(rst)))
        simulators[i] = rst[[i]]
        
    return(simulators)
}


