## hw02-2.R
#############################################
## Simulation of the Birthday Problem
## A sample code
## 04/22/2015 Yuki Yanai
#############################################

## Function to detect birthday matches
detect_bmatch <- function(x, coincide = 2){
    ## Given a vector (of birthdays) x,
    ## return TRUE or FALSE for
    ## "at least 'coincide' people having the same birthday"
    return(max(table(x)) >= coincide)
}

## Function to run simulations
bday_sim <- function(n = 10, coincide = 2, trials = 1000) {
    ## Given group size 'n',
    ## return the relative frequency of the match
    ## and visualize the simulation
    
    results <- rep(NA, trials)
    cum.rate <- rep(NA, trials)
    for (i in 1:trials) {
        ## assuming no leap year
        bdays <- sample(1:365, n, replace = TRUE)
        results[i] <- detect_bmatch(bdays, coincide)
        cum.rate[i] <- sum(results[1:i]) / i
    }
    
    plot(1:trials, cum.rate, type = "l", lwd = 2, col = "tomato",
         main = "Simulation of Birthday Problem",
         xlab = "trilals", ylab = "relative frequncy of match")
    abline(h = pbirthday(n, coincide = coincide), col = "gray", lwd = 2)
    return(cum.rate[trials])
}

## Simulation 1: Pr(at least one pair in group of 10)
bday_sim(n = 10, coincide = 2, trials = 100000)
pbirthday(10)

## Simulation 2: Pr(at least one pair in group of 40)
bday_sim(n = 40, coincide = 2, trials = 100000)
pbirthday(40)

## Simulation 3: Pr(at least one trio in group of 40)
bday_sim(n = 40, coincide = 3, trials = 100000)
pbirthday(40, coincide = 3)
