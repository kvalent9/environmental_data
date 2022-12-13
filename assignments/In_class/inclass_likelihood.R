require(here)
dat_bird <- read.csv(here("data", "bird.sta.csv"))
dat_habitat <- read.csv(here("data", "hab.sta.csv"))

sum(dat_bird$WIWR > 0)

set.seed(1)
vec_rnorm = rnorm(n = 10, mean = 0, sd = 1)
sum(dnorm(vec_rnorm, mean= 0.13, sd=1.0, log= TRUE)) 
vec_rnorm
mean(vec_rnorm)
sd(vec_rnorm)


require(palmerpenguins)

head(penguins)
penguins_flipper <- penguins$flipper_length_mm
penguins_flipper

dpois(x=2, lambda = 4.5)
dpois(x=6, lambda = 4.5)
dpois(x=2, lambda = 4.5) * dpois(x=6, lambda = 4.5)
sum(log(dpois(x = 2, lambda = 4.5) * dpois(x=6, lambda =4.5)))

set.seed(1)
penguins_rnorm = rnorm(n=2, mean= 0, sd=1)
penguins_rnorm
mean(penguins_flipper, na.rm= TRUE)
sd(penguins_flipper, na.rm =TRUE)
sum(dnorm(penguins_rnorm, mean=-0.3, sd= 0.6))
d