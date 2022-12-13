install.packages("psych")
no
install.packages("psych")
require("psych")
require(here)
install.packages("here")
pairs.panels(iris)
here(iris)
read.csv(here("data", "bird.sta.csv"))
dat_bird <- data.frame(read.csv(here("data", "bird.sta.csv")))
read.csv(here("data", "hab.sta.csv"))
dat_habitat <- data.frame(read.csv(here("data", "hab.sta.csv")))
head(dat_bird)
dat_all <- data.frame(merge(dat_bird, dat_habitat, by=c("basin", "sub", "sta")))
dat_all
plot(ba.tot ~ elev, data = dat_all)
sample(dat_all$CEWA, 20)
cewa_present_absent <- as.numeric(dat_all$CEWA > 1)
cewa_present_absent
plot(x = dat_all$elev, y = cewa_present_absent)
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)
# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}
# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}
# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}
# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)
cewa_present_absent <- as.numeric(dat_all$CEWA > 1)
cewa_present_absent
plot(x = dat_all$elev, y = cewa_present_absent)
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)
recr_present_absent <- as.numeric(dat_all$CBCH > 1)
recr_present_absent
plot(main = "Basal Area Impact on Chestnut-bk Chickadee Occurrence", x = dat_all$ba.tot, y= recr_present_absent, xlab= "Total Basal Area", ylab= "Chickadee Occurrence") 
curve(logistic_midpoint_slope(x, midpoint = 100, slope = 0.1), add = TRUE)