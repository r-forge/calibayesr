#$Date$
#$Author$
makeSettings = function(filename=FALSE, 
                        burn=500, 
                        thin=50, 
                        block=1, 
                        simulator="copasi-stochastic",
                        wsdl="internal",
                        check=TRUE)
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


#makeParameterTuning('/tmp/tuning.xml', iters=100000, burn=100, thin=10, block = 3)
