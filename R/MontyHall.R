## MontyHall.R
##########################################
## Simulate Monty Hall Problem
##
## 03/20/2015 Yuki Yanai
##########################################

## Assumption:
## In addition to the normal Monty Hall assumptions,
## assume that the player always picks "Door 1" as
## her first choice.  Then, Monty Hall chooses another
## door to open randomly "if possible".
set.seed(1921-08-25)  ## Monty Hall's Birthday!!!
trials <- 1000

## Strategies:
## The player has two strategies at each trial.
##   stick: stick to the first choice
##   move: move to the alternative.

## Let's make a deal!!!
prize <- sample(1:3, trials, replace = TRUE)
stick <- prize == 1
move <- prize != 1 

## relative frequency of winniing
prob.stick <- prob.move <- rep(NA, trials)
for (i in 1:trials) {
    prob.stick[i] <- sum(stick[1:i]) / i
    prob.move[i] <- sum(move[1:i]) / i
}

## plot the simulation result
plot(1:trials, prob.move, ylim = c(0, 1),
     type = "l", col = "darkred", 
     xlab = "number of trials",
     ylab = "prob. of winning",
     main = "Simulation of Monty Hall Problem")
lines(1:trials, prob.stick, type = "l", col = "darkblue")
legend("topright", lty = 1, col = c("darkred", "darkblue"),
       legend = c("move", "stick"))
