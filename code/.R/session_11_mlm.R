# preparation -------------------------------------------------------------

pacman::p_load(rethinking, tidyverse)
source("code/.R/dark_theme.R")

# Exercise 1 --------------------------------------------------------------

# generative model

sim_quality_1 <- function(G) {
  N <- length(G)
  quality <- ifelse(G==1, .75, .6) + rnorm(N, 0, .2)
  data.frame(G, quality)
}

# simulate data 
G <- as.factor(sample(c(1, 2), 1e3, replace = T))
d1 <- sim_quality_1(G)

# test model

## complete pooling
mQCP1 <- ulam(
  alist(
    quality ~ dnorm(mu, sigma) , 
    mu ~ dnorm(.5, .2) , 
    sigma ~ dexp(.5)
  ) , data = d1, chains = 4, cores = 4, log_lik = TRUE
)
precis(mQCP1)


## no pooling
mQNP1 <- ulam(
  alist(
    quality ~ dnorm(mu, sigma) , 
    mu <- a[G] , 
    a[G] ~ dnorm(.5, .2) , 
    sigma ~ dexp(.5)
  ) , data = d1, chains = 4, cores = 4, log_lik = TRUE
)
precis(mQNP1, depth = 2)


# Exercise 2 --------------------------------------------------------------

## partial pooling
mQPP1 <- ulam(
  alist(
    quality ~ dnorm(mu, sigma) , 
    mu <- a[G] , 
    a[G] ~ dnorm(a_bar, tau) ,
    a_bar ~ dnorm(.5, .2) , 
    tau ~ dnorm(0, .1) , 
    sigma ~ dexp(.5)
  ) , 
  data = d1, 
  chains = 4, 
  cores = 4, 
  log_lik = TRUE
)

precis(mQPP1, depth = 2)

compare(mQPP1, mQCP1, mQNP1)

# Exercise 3 ---------------------------------------------------------------

# generative model 

sim_quality_3 <- function(G, a, b, oa, ob) {
  N <- length(G)
  numeracy <- rnorm(N, .5, .2)
  quality <- a + oa[G] + b + ob[G]*numeracy + rnorm(N, 0, .1)
  data.frame(G, numeracy, quality)
}

# simulate data

G <- as.factor(sample(1:20, 1e4, replace = T))
oa <- round(sample(seq(-.3, .3, .05), 20, replace = T), 2)
ob <- round(sample(seq(-.2, .2, .05), 20, replace = T), 2)
d3 <- sim_quality_3(G, a = .5, b = 0, oa = oa, ob = ob)

head(tibble(oa=.5+oa, ob=ob), 5)

# test model

## partial pooling 

mQPP3 <- ulam( 
  alist(
    quality ~ dnorm(mu, sigma) , 
    mu <-  a[G] + b[G] * numeracy ,
    a[G] ~ dnorm(a_bar, tau_a) ,
    b[G] ~ dnorm(b_bar, tau_b) , 
    a_bar ~ dnorm(.5, .2) , 
    b_bar ~ dnorm(0, .15) , 
    tau_a ~ dexp(.5) , 
    tau_b ~ dexp(.5) ,
    sigma ~ dexp(1)
  ) , 
  data = d3, 
  chains = 4, 
  cores = 4, 
  iter = 4000,
  log_lik = TRUE
)

precis(mQPP3, depth = 2)
plot(precis(mQPP3, depth = 2))


mQCP3 <- ulam( 
  alist(
    quality ~ dnorm(mu, sigma) , 
    mu <-  a + b * numeracy ,
    a ~ dnorm(.5, .2) , 
    b ~ dnorm(0, .15),
    sigma ~ dexp(1)
  ) , 
  data = d3, 
  chains = 4, 
  cores = 4, 
  iter = 4000,
  log_lik = TRUE
)


precis(mQCP3, depth = 2)

compare(mQPP3, mQCP3)