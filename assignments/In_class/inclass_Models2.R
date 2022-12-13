data(iris)

fit_species = 
  lm(
    Sepal.Length ~ Species,
    data = iris)

summary(fit_species)

plot(
  Petal.Width ~ Petal.Length,
  data = iris,
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")

fit_petals = 
  lm(Petal.Width ~ Petal.Length,
     data = iris)

summary(fit_petals)

