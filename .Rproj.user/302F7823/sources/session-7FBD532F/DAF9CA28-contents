catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(
  x = n_success,
  n = n_years,
  p = 0.5)

late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate) 

##one-tailed

binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate,
  alternative ='less')

##t-test

t.test(catrate$cat.rate, mu = 2/7)

##wilcoxon

wilcox.test(catrate$cat.rate, mu = 2/7)

##F-distribution

veg = read.csv(here("data", "vegdata.csv"))
head(veg)

boxplot(pine ~ treatment, data = veg)

veg2 = droplevels(
  subset(
    veg,
    treatment %in% c('control','clipped')
  ))

# verify that treatment is factorized
veg2$treatment = factor(veg2$treatment)

var.test(
  pine ~ treatment,
  data = veg2)

shapiro.test(veg2$pine[veg2$treatment=="control"])
shapiro.test(veg2$pine[veg2$treatment=="clipped"])

fligner.test(
  pine ~ treatment,
  data = veg2)

##tests for multiple variances

#PARAMETRIC
bartlett.test(pine ~ treatment, data = veg)
#NONPARAMETRIC
fligner.test(pine ~ treatment, data = veg)

##t-test

t.test(
  pine ~ treatment,
  data = veg2)

wilcox.test(
  pine ~ treatment,
  data = veg2)

##paired t-test

install.packages("datarium")
require(datarium)
data("mice2")
head(mice2)

t.test(mice2$before, mice2$after, paired = TRUE)

##checking normality assumption
shapiro.test(mice2$before)
shapiro.test(mice2$after)

#paired Wilcoxon test

wilcox.test(mice2$before, mice2$after, paired = TRUE)

#unpaired t-test
t.test(mice2$before, mice2$after, paired = FALSE)

#salamander data

disp = read.csv(here("data", "dispersal.csv"))
disp

plot(
  disp.rate.ftb ~ disp.rate.eb,
  data = disp,
  main = "Marbled Salamander Dispersal Rates",
  xlab = "Dispersal Rate\nFirst Time Breeders",
  ylab = "Dispersal Rate\nExperienced Breeders",
  pch = 21, col = 1, bg = "steelblue"
)

#test correlation

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')

#spearman's rank correlation (nonparametric)

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

#Kolmogorov-Smirnov - comapring two distrib

#first time breeders
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE,
  main = "Mike's Plot of Marbled Salamanders\nFirst-Time Breeders: ECDF")

#first time and experienced
plot(
ecdf(disp$disp.rate.ftb),
verticals=TRUE,
main = "Mike's Plot of Marbled Salamanders\nFirst-Time and Experienced Breeders: ECDF")
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)
legend(
  x = 0.4, y = 0.4,
  lty = c(1, 3),
  legend = c("first-time", "experienced"),
  title = "Breeder Class")

#sex-linked

prop.test(
  x = c(4,16),
  n = c(40,250))

#chi-squared

owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq_owls = chisq.test(owls)
chisq_owls

##expected
round(chisq_owls$expected, 1)
##observed
chisq_owls$observed

##chi resids
round(
  chisq_owls$observed - chisq_owls$expected,
  digits = 1)

#fisher's test: lower expected freq
fisher.test(owls)

birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(
  birds,
  hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(
  birdhab$s.edge,
  birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(
  birdhab$s.edge, 
  birdhab$BRCR > 0)[, 2:1]

br_creeper_table

# QUESTIONS
 ##Q.2
chisq_creepers = chisq.test(br_creeper_table)
chisq_creepers

##Q.3
require(palmerpenguins)
head(penguins)
fit_species = 
  lm(
    formula = body_mass_g ~ species,
    data = penguins)

fit_species

fit_sex = 
  lm(
    formula = body_mass_g ~ sex,
    data = penguins)
fit_sex

fit_both = 
  lm(
    formula = body_mass_g ~ sex:species,
    data = penguins)

boxplot(penguins$body_mass_g ~ penguins$sex:penguins$species,
        names = c("Female\nAdelie", "Male\nAdelie", "Female\nChinstrap", "Male\nChinstrap", "Female\nGentoo", "Male\nGentoo" ), 
        main = "Double Conditioned Boxplot of Species and Sex",
        xlab = "Sex and Species",
        ylab = "Body mass (g)")

boxplot(penguins$body_mass_g ~ penguins$sex,
        main = "Conditional Plot of Penguin Sex and Body Mass Fit",
        xlab= "Sex",
        ylab = "Body mass (g)")

boxplot(penguins$body_mass_g ~ penguins$species,
        main = "Conditional Plot of Penguin Species and Body Mass Fit",
        xlab= "Species",
        ylab = "Body mass (g)")

##Q.10-12

bartlett.test(body_mass_g ~ species, data = penguins)

bartlett.test(body_mass_g ~ sex, data = penguins)

##Q. 13

dat_groups = aggregate(
  body_mass_g ~ sex:species,
  data = penguins,
  FUN = c)
str(dat_groups)

dat_body <- dat_groups$body_mass_g

bartlett.test(dat_body)

##Q. 15
require(here)

read.csv(here("data", "trees_FL.CSV"))

dat_f1 <- data.frame(read.csv(here("data", "trees_FL.CSV")))
dat_f1
barplot(table(dat_f1$ProbabilityofFailure), main = "Probability of Failure")

barplot(table(dat_f1$Failure_Standardized), main = "Failure Standardized")

hist(dat_f1$DBH_in, main = "Histogram of DBH", xlab = "DBH (in)")

plot(x= dat_f1$DBH_in, y= dat_f1$HeighttoTop_ft, cex = 0.1, main = "Scatterplot of DBH and Tree Height", xlab= "DBH (in)", ylab = "Tree height (ft)")

par(mfrow = c(2,2))

whole <- subset(dat_f1, Failure_Standardized == "whole")
whole
none <- subset(dat_f1, Failure_Standardized == "none")
none
ks.test(whole$DBH_in, none$DBH_in)

##Q. 20
cor.test(
  dat_f1$DBH_in,
  dat_f1$HeighttoTop_ft,
  use='complete.obs',
  method='spearman')

##Q.21-25
dat_f1$fail = factor(dat_f1$Failure_Standardized != "none")

failrate <- levels(dat_f1$fail) = c("No Fail", "Fail")

f1_table_2 = table(
  dat_f1$ProbabilityofFailure,
  dat_f1$fail)
f1_table_2

chisq_trees = chisq.test(f1_table_2)
chisq_trees

round(chisq_trees$expected, 1)

chisq_trees$observed

round(chisq_trees$observed - chisq_trees$expected, digits = 1)

