require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))
dat_ade1 = droplevels(subset(dat_ade, sex == "female"))
dat_ade2 = droplevels(subset(dat_ade, sex = "male"))
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")

head(dat_ade)
females <- dat_ade$sex == "female"
males <- dat_ade$sex == "male"
boxplot(data = dat_ade$body_mass_g ~ sex, xlab= "Female", ylab = "body mass (g)")
boxplot(dat_ade2$body_mass_g, xlab = "Male", ylab = "body mass (g)")

boxplot(dat_ade$sex == "female", ylab = dat_ade$body_mass_g)
par(mfrow = c(1,2))
t.test()

boxplot(body_mass_g ~ sex, data = dat_ade)

t.test(females$body_mass_g, males$body_mass_g)
