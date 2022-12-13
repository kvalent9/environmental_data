install.packages("here")
require("here")
read.csv(here("data", "hab.sta.csv"))
dat_habitat <- data.frame(read.csv(here("data", "hab.sta.csv")))
hist(x= dat_habitat$elev, main = "Histogram of sampling site elevation", xlab= "elevation", ylab= "frequency")
par(mfrow = c(3, 1))
hist(x= dat_habitat$aspect, main = "Histogram of sampling site aspect", xlab= "aspect", ylab= "frequency")
hist(x=dat_habitat$slope, main = "Histogram of sampling site slope", xlab= "slope", ylab= "frequency")
breaks(hist(x= dat_habitat$aspect, main = "Histogram of sampling site aspect", xlab= "aspect", ylab= "frequency"))



plot(x=dat_habitat$aspect, y=dat_habitat$ba.tot, main= "Scatterplot of aspect data", xlab= "aspect", ylab= "total basal area", cex= 0.4)
curve(line_point_slope(x, x1 = 50, y1 = 15, slope = 0.3), add = TRUE)

plot(x=dat_habitat$elev, y= dat_habitat$ba.tot, main= "Scatterplot of elevation data", xlab= "elevation", ylab= "total basal area", cex= 0.4)
curve(line_point_slope(x, x1 = 25, y1 = 5, slope = 0.1), add = TRUE)


plot(x=dat_habitat$slope, y=dat_habitat$ba.tot, main= "Scatterplot of slope data", xlab= "slope", ylab= "total basal area", cex= 0.4)
curve(line_point_slope(x, x1 = 50, y1 = 50, slope = 1), add = TRUE)

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


par(mfrow = c(2,2))

