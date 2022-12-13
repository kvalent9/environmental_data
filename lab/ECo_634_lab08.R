require(here)
dat_veg <- read.csv(here("data", "vegdata.csv"))
dat_hab <- read.csv(here("data", "hab.sub.csv"))
dat_bird <- read.csv(here("data", "bird.sub.csv"))

require(palmerpenguins)

dat_pen = subset(penguins, species != "Gentoo")

dat_pen

t.test(flipper_length_mm ~ species, data = dat_pen, alternative = "less")

install.packages("simpleboot")
require(simpleboot)
?two.boot

adelie = droplevels(subset(dat_pen, species != "Chinstrap", na.rm = TRUE))
chinstrap = droplevels(subset(dat_pen, species != "Adelie", na.rm = TRUE))

adelie_flipper <- adelie$flipper_length_mm
chinstrap_flipper <- chinstrap$flipper_length_mm

pen_boot <- two.boot(adelie_flipper, chinstrap_flipper, FUN= mean, R= 10000, na.rm = TRUE)
print= pen_boot
str(pen_boot)

hist(pen_boot$t, main = "Kay's Histogram of Mean Flipper Length Difference", xlab= "Mean Flipper Length Difference")
sd(pen_boot$t)

dat_veg
boxplot(pine ~ treatment, dat = dat_veg)

dat_tree = droplevels(subset(dat_veg, treatment %in% c("control", "clipped")))

boxplot(pine ~ treatment, dat= dat_tree)
table(treatment = "clipped")
table(treatment = "control")

table(dat_tree$pine)

head(dat_tree)

wilcox.test(pine ~ treatment, data = dat_tree, alternative = "two.sided")

clipped = droplevels(subset(dat_tree, treatment == "clipped"))
clipped_sample = clipped$pine
clipped_sample
control = droplevels(subset(dat_tree, treatment == "control"))
control_sample = control$pine

tree_boot <- two.boot(clipped_sample, control_sample, FUN= mean, R=1000, na.rm= TRUE)
str(tree_boot)

class(tree_boot)

install.packages(boot.ci)
require(boot)
boot.ci(tree_boot)
?boot.ci

quantile(tree_boot$t, c(0.025, 0.975))

# I already read my data into dat_bird and dat_habitat:
dat_all = merge(
  dat_bird, 
  dat_hab,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)

b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

mean(dat_all$b.sidi.standardized)
mean(dat_all$s.sidi.standardized)

sd(dat_all$b.sidi.standardized)
sd(dat_all$s.sidi.standardized)

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]
print(slope_observed)
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

set.seed(123)
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices (MC resampled data)",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

m = 10000 
result_mc = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2]

  
  result_mc[i] = coef(fit_resampled_i)[2]
} 

head(result_mc)

hist(result_mc)

hist(
  result_mc,
  main = "Kay's Null Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v=q, lty=2, col= "blue", lwd= 2)


q=quantile(result_mc, c(0.05))
q

set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)

dat_boot = dat_1[index_1, ]
head(dat_boot)

fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)
coef(fit_bs1)

hist(
  result_boot,
  main = "Mike's Alternative Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = 0, lty = 2, col = 1, lwd = 2)

m = 10000 
result_boot = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  
  dat_boot = dat_1[index_1, ]
  head(dat_boot)
    
  fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)
  
  coef(fit_bs1)
  
  
  result_boot[i] = coef(fit_bs1)[2]
} 

head(result_boot)

hist(
  result_boot,
  main = "Mike's Alternative Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = 0, lty = 2, col = 1, lwd = 2)

par(mfrow = c(1,2))
hist(result_mc)
hist(result_boot)

plot(
  density(result_mc),
  main = "Mike's Null Distribution Density Plot",
  xlab = "Slope Coefficient")

plot(
  density(result_mc),
  main = "Mike's Null Distribution Density Plot",
  xlab = "Slope Coefficient")

lines(density(result_boot))

boot.ci(pen_boot)
quantile(pen_boot$t, c(0.025, 0.975))

mean(pen_boot$t)
median(pen_boot$t)

pen_ecdf <- ecdf(pen_boot$t)
1-pen_ecdf(-4.5)
pen_ecdf(-4.5)
pen_ecdf

pen_ecdf(-8)


str(tree_boot)
