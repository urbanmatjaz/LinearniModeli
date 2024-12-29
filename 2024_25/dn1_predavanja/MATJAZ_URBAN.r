library(ggplot2)
library(ggpubr)
library(GLMsData)
library(effects)
library(emmeans)
library(car)

data(lungcap)
str(lungcap)
summary(lungcap)

# Ht, telesna višina v cm

lungcap$Ht <- lungcap$Ht*2.54

#  Smoke naj bo faktor z vrednostma "Ne" in "Da"

lungcap$Smoke <- factor(lungcap$Smoke, labels=c("Ne", "Da"))
levels(lungcap$Gender)

# zamenjamo oznaki za spol za grafične prikaze

levels(lungcap$Gender) <- c("Ženske", "Moški")  

mod2.int <- lm(log(FEV) ~ Age*Ht*Gender*Smoke, data=lungcap) 

plot(Effect(c("Age","Ht","Gender","Smoke"), mod2.int, partial.residuals=TRUE), main="")

summary(mod2.int)

# ŽN : log(FEV) = -2.55 + 0.12 * Age + 0.02 * Ht - 0.0006 * Age * Ht
# ŽK : log(FEV) = (-2.55 + 12.05) + (0.12 - 0.84) * Age + (0.02 - 0.07) * Ht + (0.005 - 0.0006) * Age * Ht
# MN : log(FEV) = (-2.55 + 0.98) + (0.12 - 0.14) * Age + (0.02 - 0.006) * Ht + (0.0008 - 0.0006) * Age * Ht
# MK : log(FEV) = (-2.55 + 0.98 + 12.05 - 13.88) + (-0.14 - 0.84 + 0.87) * Age + (-0.006 - 0.07 + 0.08) * Ht + (0.0008 + 0.005 - 0.005- 0.0006) * Age * Ht



