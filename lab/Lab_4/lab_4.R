# Generate a vector of x-values
x = seq(0, 20, length.out = 1000)
y = dnorm(x, mean=10.4, sd=2.4)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

mean(penguins$body_mass_g, na.rm = FALSE)
sd(penguins$body_mass_g, na.rm = FALSE)
nrow(penguins)

n_samples = 344
pop_sd = 802
pop_mean = 4202

dat_1 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_2 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_3 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_4 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)

par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

dat_unif = runif(n = 27, min = 0, max = 4)
hist(dat_unif)

dat_unif = runif(n = 270, min = 0, max = 4)
hist(dat_unif)

set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
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

set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

n_pts = 10
x_min = 1
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

guess_x = 6
guess_y = 0
guess_slope = 0.1


plot(y ~ x, data = dat_random, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

dat_random$y_predicted=line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)
dat_random$y_predicted

dat_random$resids=y_random - dat_random$y_predicted
sum(dat_random$resids)

plot(x = dat_random$y_predicted, y=dat_random$resids)
hist(x=dat_random$resids)

pop_sd = 2.4
pop_mean = 10.4

norm_17  = rnorm(n= 17, mean= pop_mean, sd= pop_sd)
norm_30 = rnorm(n= 30, mean= pop_mean, sd= pop_sd)
norm_300 = rnorm(n= 300, mean= pop_mean, sd= pop_sd)
norm_3000 = rnorm(n= 3000, mean= pop_mean, sd= pop_sd)

par(mfrow = c(2, 2))
hist(main= "Histogram n=17", x= norm_17)
hist(main= "Histogram n=30", x=norm_30)
hist(main= "Histogram n=300", x=norm_300)
hist(main= "Histogram n=3000", x=norm_3000)

require(here)
pdf(file=("norm_1.pdf"))


dev.off()

x = seq(0, 20, length.out = 1000)
y = dnorm(x, mean=10.4, sd=2.4)
plot(x, y, main = "Normal PDF: mean: 10.4 sd=2.4", type = "l")
pdf(file= "norm_1.pdf", bg="white")

#Questions 9-10
set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

dat_unif_1
dat_unif_2

hist(x=dat_unif_1, col="red", main= "Histogram of Random Uniformly Generated Data", xlab= "Random Data")

hist(x=dat_unif_2, col="blue", main= "Histogram of Randomly Generated Data 2.0", xlab= "Random Data")

random_1 = rnorm(n=65, mean=800, sd=1)
hist(x=random_1, main= "Histogram of Randomly Generated Data 2.0", xlab= "Random Numbers", col="Steel blue")

dat_unif_2= runif(n=65, min=0, max=10)
plot(x=dat_unif_2, y=random_1, main="Scatterplot of Randomness", col="purple")

boxplot(x=dat_unif_2, main= "Boxplot of Random Data", ylab= "Random numbers", col="lime green")

par(mfrow = c(2, 2))

pdf(file= "random_fig2.pdf", bg="white")

par(mfrow = c(2, 2))
boxplot(x=dat_unif_2, main= "Boxplot of Random Data n= 65", ylab= "Random numbers", col="lime green")
plot(x=dat_unif_2, y=random_1, main="Scatterplot of Randomness n=65", col="purple")
hist(x=random_1, main= "Histogram of Random Data n=65", xlab= "Random numbers", col="Steel blue")
hist(x=dat_unif_1, col="red", main= "Histogram of Random Data n= 270", xlab= "Random numbers")

dev.off()

n_pts = 65
x_min = 0
x_max = 10

# X values are uniformly distributed
dat_unif_2 = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = dat_unif_2, y = y_random)

plot(y ~ x, data = dat_random, pch = 8)

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
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
x= dat_unif_2
guess_x= 4
guess_y= 0
guess_slope= 0.3

curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = TRUE)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add= TRUE)

pdf(file= "random_line.pdf", bg="white")

dev.off()

line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)
dat_random$y_predicted=line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)
dat_random$y_predicted

dat_random$resids=y_random - dat_random$y_predicted
dat_random$resids

pdf(file= "resids.pdf", bg="white")

par(mfrow = c(2, 1))

hist(dat_random$resids, main="Histogram of Model Residuals", xlab= "Residual values")

plot(x=dat_random$y_predicted, y= dat_random$resids, main= "Residual Scatterplot", xlab= "predicted values", ylab= "residual values")

dev.off()
