ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

par(mfrow = c(1, 3))

exp_fun =function(x, a, b)
{
  return(a * exp(-b * x))
}

curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()


# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")


y_observed_3 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = rexp(n, 1.2))
    
plot(x_sim, y_observed_3, main = "Exponentially Distributed", xlab = "", ylab = "")

fit_1 = lm(y_observed ~ x_sim)
fit_2 = lm(y_observed_2 ~ x_sim)
fit_3 = lm(y_observed_3 ~ x_sim)

par(mfrow = c(1, 4))

plot(y_observed ~ x_sim); abline(fit_1)
plot(y_observed_2 ~ x_sim); abline(fit_2)
plot(y_observed_3 ~ x_sim); abline(fit_3)

require(here)

read.csv(here("data", "dispersal.csv"))

dispersal_dat <- read.csv(here("data", "dispersal.csv"))

plot(x= dispersal_dat$disp.rate.ftb, y=dispersal_dat$dist.class)

a= 1.9
b= 0.3

curve( 
  exp_fun(x, 1.9, 0.1), add = FALSE, from = 0, to = 35,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

curve(
  exp_fun(x, 1.9, 0.3), add = TRUE, from = 0, to = 35,
  ann = FALSE, axes = TRUE, ylab = "f(x)", lty=2, lwd= 1); box()

curve(
  exp_fun(x, 1.2, 0.2), add = TRUE, from = 0, to = 35,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col= 2); box()

curve(
  exp_fun(x, 1.2, 0.4), add = TRUE, from = 0, to = 35,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col= "red", lty=2, lwd=1); box()


#ricker function

curve(
  ricker_fun(x, 25, 0.2), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x",
  ylim= c(0,100))

curve(
  ricker_fun(x, 20, 0.2), 
  from = 0, to = 5, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", lty=2, lwd= 1)

curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 5, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", lty=2, lwd= 1)

curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 5, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", col="red")

curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 5, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", lty=2, lwd= 1, col= "red")

curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 5, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", lty=2, lwd= 1, col= "red")

#salamander 8-13

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

plot(
  dispersal_dat$dist.class,
  dispersal_dat$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders")

curve(line_point_slope(x, 800, 0.20068, -0.0003), add = TRUE)

locator(1)

exp_fun =function(x, a, b)
{
  return(a * exp(-b * x))
}

curve(
  exp_fun(x, 8, 0.002), add = TRUE, from = 0, to = 1500,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 0.015, 0.01), 
  from = 0, to = 1500, add = TRUE, 
  ylab = "f(x)", xlab = "x")

#residuals

n_pts = 20
x_min = 0
x_max = 1500

# X values are uniformly distributed
dat_unif_2 = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = dat_unif_2, y = y_random)

plot(y ~ x, data = dispersal_dat, pch = 8)


guess_x= 
guess_y= 0
guess_slope= 0.3

curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = TRUE)

par(mfrow = c(1, 3))

predicted_line<- line_point_slope(dispersal_dat$dist.class, 800, 0.1, -0.0003)

dispersal_dat$resids= dispersal_dat$disp.rate.ftb - predicted_line
dispersal_dat$resids

hist(dispersal_dat$resids, main= "Histogram of Linear Residuals", xlab= "Residuals")

predicted_line2 <- exp_fun(dispersal_dat$dist.class, 8, 0.002)

dispersal_dat$resids= dispersal_dat$disp.rate.ftb - predicted_line2
dispersal_dat$resids

hist(dispersal_dat$resids, main= "Histogram of Exponential Residuals", xlab= "Residuals")

predicted_line3 <- ricker_fun(dispersal_dat$dist.class, 0.015, 0.01)

dispersal_dat$resids= dispersal_dat$disp.rate.ftb - predicted_line3
dispersal_dat$resids

hist(dispersal_dat$resids, main= "Histogram of Ricker Residuals", xlab= "Residuals")
