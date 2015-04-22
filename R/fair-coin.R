## fair-coin.R
##
## Illustrate how Bayesian inference works using
## an example of coin flips.
##
## 03/31/2015 Yuki Yanai

## Let theta denote the probability that we observe Head
## by flipping the coin

## likelihood of theta, up to a constant
bin_lik <- function(theta, n, y) {
    if (y > n) stop("Inlvalid value of y: y can't be greater than n")
    return(theta^y * (1 - theta)^(n - y))
}

## prior PMF of discrete theta
## theta must be rounded to second decimal
prior_pmf <- function(theta = c(.1, .5, .9),
                      prior = rep(1, length(theta))) {
    ## Arguments:
    ##     theta: vector of possible parameter values
    ##     prior: prior weights for the possible values
    ## Return:
    ##     Prior PMF of theta in (0, 1)
    if (length(theta) == 0) stop("Enter at least one possible value")
    if (sum(prior) == 0) stop("Invalid prior values")
    x <- 1:99
    prob <- rep(NA, length(x))
    j = 1 
    for (i in seq_along(x)) {
        if (x[i] %in% round(100 * theta)) {
            prob[i] <- prior[j]
            j = j + 1
        }
        else prob[i] <- 0
    }
    return(prob / sum(prob))
}

## function to make a graph with prior, likelihood, and posterior
## where the prior is given by a PMF
plt_coin_disc <- function(n, y, theta = c(.1, .5, .9), prior = c(1, 1, 1)) {
    x <- seq(0.01, 0.99, by = 0.01)
    post <- bin_lik(x, n, y) * prior_pmf(theta, prior)
    post <- post / sum(post)
    maxlik <- max(bin_lik(x, n, y))
    plot(x, post, type = "h", lwd = 10, ylim = c(0, 1), col = "tomato",
         xlab = expression(theta),
         ylab = "Probability or Likelihood",
         main = paste("Binomial experiment: n = ",n, ", y = ", y, sep = ""))
    par(new = TRUE)
    plot(x, prior_pmf(theta, prior), type = "h", lwd = 4, ylim = c(0, 1),
         xlab = "", ylab = "", main = "")
    curve(bin_lik(x, n, y) / maxlik, 0, 1, lwd = 2, col = "royalblue", add = TRUE)
    if (y >= n / 2) legend.pos <- "topleft"
    else legend.pos <- "topright"
    legend(legend.pos, lty = 1, col = c("black", "royalblue", "tomato"), lwd = 4,
           legend = c("prior", "likelihood", "posterior"))
}

plt_coin_cont <- function(n, y, a = 1, b = 1){
    x <- seq(0.001, 0.999, by = 0.001)
    maxlik <- max(bin_lik(x, n, y))
    posterior <- function(theta) {
        return(bin_lik(theta, n, y) * dbeta(theta, a, b))
    }
    post <- bin_lik(x, n, y) * dbeta(x, a, b)
    post <- post / integrate(posterior, lower = 0, upper = 1)$value
    ymax <- max(dbeta(x, a, b), post)
    plot(x, post, type = "l", lwd = 4, col = "tomato", yaxt = "n", 
         ylim = c(-0.3 * ymax, ymax),
         xlab = expression(theta),
         ylab = "Density or Likelihood",
         main = paste("Binomial experiment: n = ",n, ", y = ", y,
                      " with prior Beta(", a, ", ", b, ")", sep = ""))
    curve(dbeta(x, a, b), from = 0, to = 1, lwd = 2, add = TRUE)
    curve((bin_lik(x, n, y) / maxlik) * ymax, from = 0, to = 1,
          lwd =2, col = "royalblue", add = TRUE)
    abline(h = 0, col = "gray", lwd = 2)
    dgts <- function(ymax){
        if (ymax > 50) -1
        else if (ymax > 10) 0
        else 1
    }
    axis(2, at = round(seq(0, ymax, length = 7), digits = dgts(ymax)))
    legend("bottomright", lty = 1, col = c("black", "royalblue", "tomato"), lwd = 4,
           legend = c("prior", "likelihood", "posterior"))
}

## welcome message
cat("
###########################################################
## Illustration of Bayesian Inference: 'Is a Coin Fair?'
## Prepared for Research Methods in Political Science II,
## Kobe University. 2015. Yuki Yanai
##
## Function to use: 
## (1) discrete theta: plt_coin_disc(n, y, theta, prior),
## (2) continuous theta: plt_coin_cont(n, y, a, b)
##     n: number of trials, a single value
##     y: number of Heads, a single value
##     theta: possible values of theta, a vector
##     prior: probabilities or relative weights for each
##            possbile theta, a vector
##     a, b: shape parameters of beta distribution
###########################################################\n
## Example:
## plt_coin_disc(n = 8, y = 4, theta = c(.5, .8), prior = c(1, 2))
## plt_coin_cont(n = 8, y = 4, a = 6, b = 2)
")
