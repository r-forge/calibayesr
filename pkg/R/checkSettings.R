#$Date$
#$Author$

checkSettings = function(burn, thin, block)
{
    checkBurn(burn)
    checkThin(thin)
    checkBlock(block)
    simulator = 'XXX'
    checkSimulator(simulator)
    
    message("All tests passed")
    return(TRUE)
}

checkBurn = function(burn)
{
    message("Checking burn...", appendLF = FALSE)

    if(burn < 0)
        stop("\n Burn-in cannot be negative\n")

    message("OK")
}

checkThin = function(thin)
{
    message("Checking thin...", appendLF = FALSE)

    if(thin < 1)
        stop("\nThin must be positive")

    message("OK")
}

checkBlock = function(block)
{
    message("Checking block...", appendLF = FALSE)
    if(block < 1)
        stop("\nBlock size must be positive")

    message("OK")
}

checkSimulator = function(simulator)
{
    message("Checking simulator...", appendLF = FALSE)
    valid_simulators = c('XXX')
    
    if(!is.element(simulator, valid_simulators))
        stop("\nUnknown simulator", simulator)

    message("OK")
}

