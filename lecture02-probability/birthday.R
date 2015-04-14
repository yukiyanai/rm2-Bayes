## birthday.R
##################################################
## demonstrate Birthday problem
##
## Created: 03/21/2015 Yuki Yanai
## Last modified: 04/14/2015 YY
##################################################

## Assumption:
## No leap year or no one has the birthday of Feb. 29.

prob_no_match <- function(n){
    ## given the number of people n, 
    ## return the prob. of no pair having the same birthday
    if (n < 1) stop("Invalid input: n must be a postive integer")
    else if (n >= 365 | n == 1) return(0)
    else {
        free <- 365:1 / 365
        return(prod(free[1:n]))
    }
}

prob_one_match <- function(n) {
    ## given the number of people n,
    ## return the prob. of at least one pair hving
    ## the same birthday
    return(1 - prob_no_match(n))
}

## plot the prob of matching birthday vs. n of people
n.vec <- 2:365
probs <- sapply(n.vec, prob_one_match)
plot(n.vec, probs, type = "l", xlim = c(0, 100),
     col = "tomato", lwd = 2,
     xlab = "グループの人数",
     ylab = "確率",
     main = "同じ誕生日のペアが少なくとも1組いる確率")