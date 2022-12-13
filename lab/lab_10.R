require(here)
read.csv(here("data", "rope.csv"))

rm(list = ls())

rope <- read.csv(here("data", "rope.csv"))

rope$rope.type = factor(rope$rope.type)

levels(rope$rope.type)

rope_type <- rope$rope.type
rope_type

#Manual ANOVA

n_obs = nrow(rope)
n_obs
n_groups = length(unique(rope$rope.type))
n_groups

grandmean = mean(rope$p.cut)
obs = rope$p.cut
resids = obs - grandmean
ss_tot = sum(resids^2)


df_tot = n_obs - 1


agg_mean = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = mean)

aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) mean (x)
)

agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(y) y-mean(y)
)
str(agg_resids)

agg_sum_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(y) sum(y-mean(y))^2)
str(agg_sum_sq_resids)

ss_within = sum(agg_sum_sq_resids$x)
ss_within

df_within = n_obs - n_groups
df_within

ss_among = ss_tot - ss_within
ss_among

df_among = n_groups - 1
df_among

ms_within = ss_within / (n_obs - n_groups)
ms_within

ms_among  = ss_among / (n_groups - 1)
ms_among

f_ratio = ms_among / ms_within
f_ratio

f_pval = 1 - pf(f_ratio, df_among, df_within)
f_pval

#ANOVA in R

fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)

anova_fit_1$`Sum Sq`

##Question 4
bartlett.test(p.cut ~ rope.type, data = rope)

##Question 5
fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

mean_p_XTC = 0.36714 - 0.10164
mean_p_XTC

#Post-Hoc Testing
rope2 = droplevels(
  subset(rope,
         rope.type %in% c("XTC"))
)

mean(rope2$p.cut)

boxplot(
  p.cut ~ rope.type,
  data = rope2,
  las = 2,
  xlab = "",
  ylab = "Proportion Rope Cut",
  main = "Subset of Rope Data")
mtext("Rope Type", side = 1, line = 3)

##Question 8

shapiro.test(residuals(fit_rope_1))

##Question 10
lapply(agg_resids$x,
       function(x) shapiro.test(x)$p)

#Question 12

require(palmerpenguins)
pen_fem = subset(penguins, sex == "female")

boxplot(pen_fem$body_mass_g ~ pen_fem$species, xlab = "Species", ylab = "Body mass (g)", main = "Female Penguin Body Mass Conditional Boxplot")

#Question 14
bartlett.test(body_mass_g ~ species, data = pen_fem)

##Question 15

pen = lm(body_mass_g ~ species, data = pen_fem)
pen

shapiro.test(residuals(pen))

##Question 16

TukeyHSD(aov(pen))
