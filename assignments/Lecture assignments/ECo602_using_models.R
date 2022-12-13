require(here)
read.csv(here("data", "catrate.csv"))
catrate <- data.frame(read.csv(here("data", "catrate.csv")))
head(catrate)
summary(catrate)

hist(x = catrate$cat.rate, main = "Histogram of Catastrophe Rates", xlab = " Catastrophe Rate")

shapiro.test(catrate$cat.rate)

?t.test
t.test(x = catrate$cat.rate, mu = 0.2857143)
t.test(x= catrate$cat.rate, mu = 0.2857143, alternative = "greater")

wilcox.test(catrate$cat.rate, mu = 2 / 7)

require(palmerpenguins)

penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)

boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")

?aggregate
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_adelie
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")

shapiro.test(x = dat_adelie$flipper_length_mm)
shapiro.test(x = dat_chinstrap$flipper_length_mm)

t.test(flipper_length_mm ~ species, 
         data = penguin_dat)

par(mfrow = c(1,2))

hist(dat_adelie$flipper_length_mm, main = "Adelie Flipper Length", xlab= "Flipper length (mm)" )
hist(dat_chinstrap$flipper_length_mm, main = "Chinstrap Flipper Length", xlab = "Flipper length (mm)")
