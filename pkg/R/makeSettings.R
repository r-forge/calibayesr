#$Date$
#$Author$
makeSettings1 = function(filename=FALSE, 
                        burn=500, 
                        thin=50, 
                        block=1, 
                        simulator="copasi-stochastic",
                        wsdl="internal",
                        check=FALSE)
{

    if(check)
        check = checkSettings(burn, thin, block)
    
    top=newXMLNode("parameterTuning")
    newXMLNode("burn", parent=top, attrs=c(value=burn))
    newXMLNode("thin", parent=top, attrs=c(value=thin))
    newXMLNode("block", parent=top, attrs=c(value=block))
    newXMLNode("simulationMethod", parent=top, attrs=c(value=simulator))
    newXMLNode("simulatorWSDLLocation", parent=top, attrs=c(value=wsdl))

    
    if(is.character(filename)){
        saveXML(top, filename)
        rt_value = TRUE
    }else{
        rt_value = saveXML(top)
    }
    return(rt_value)
}

############################################
#Example usage
############################################
#makeSettings(FALSE, burn=100, thin=10, block = 3)
makeSettings = function(filename=FALSE, 
                        burn=500, 
                        thin=50, 
                        block=1, 
                        simulator="copasi-stochastic",
                        wsdl="internal",
                        check=FALSE)
{

    if(check)
        check = checkSettings(burn, thin, block)

    df = data.frame(nodes = c("burn","thin", "block", "simulationMethod","simulatorWSDLLocation"))
    df$values = c(burn, thin, block, simulator, wsdl)
    
    rt_values = paste("<", df$nodes, " value='", df$values, "'/>", sep="", collapse="")
    rt_values = addXMLTag(rt_values, "parameterTuning")

    if(is.character(filename)){
        write(rt_values, filename)
        rt_values = TRUE
    }

    return(rt_values)
}
    
