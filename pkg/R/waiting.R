

isCaliBayesFinished = function(wsdl, sid, no_of_checks, maxtime)
{
    maxtime = 60*maxtime
    increment = maxtime/no_of_checks
    if(increment < 30)
    {
        mess = paste("You are planning on checking the webservice every", round(increment,2), "seconds\n")
        mess = paste(mess, "This is a bit extreme, so I will only check every 30 seconds\n", sep="")
        cat(mess)
        no_of_checks = maxtime/30
        increment = maxtime/no_of_checks
        
    }
    
    counter = 0
    test = isCaliBayesReady(wsdl, sid)
    while(counter<no_of_checks && test=='FALSE'){
        cat(paste("Still Running: ", Sys.time(), "\n"))
        test = isCaliBayesReady(wsdl, sid)
        Sys.sleep(increment)
        counter = counter + 1
    }
    

    if(test)
    {
        cat(paste("Finished: ", Sys.time(), "\n"))
        return(TRUE)
    }
    else
    {
        cat(paste("Still Running: ", Sys.time(), "\n"))
    	return(FALSE)
    }
}


