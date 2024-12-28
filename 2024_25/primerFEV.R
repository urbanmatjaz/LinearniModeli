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

summary(lungcap)

ggplot(lungcap, aes(x=Age, y=FEV))+ geom_point() + 
  xlim(c(0,20)) + 
  geom_smooth(se=FALSE) +
  xlab("Starost (dopolnjena leta)") + 
  ylab("Pljučna kapaciteta (L)") + 
  theme_bw()
ggplot(lungcap, aes(x=Ht, y=FEV))+ geom_point() + xlim(c(110, 190)) + geom_smooth(se=FALSE) +
  xlab("Telesna višina (cm)") + ylab("Pljučna kapaciteta (L)") + theme_bw()
ggplot(lungcap, aes(x=Gender, y=FEV))+ geom_boxplot() +  xlab("Spol") +
  ylab("Pljučna kapaciteta (L)") + theme_bw()
ggplot(lungcap, aes(x=Smoke, y=FEV))+ geom_boxplot() +  xlab("Status kajenja") + 
  ylab("Pljučna kapaciteta (L)") + theme_bw()

# Opisna spremenljivka v lm() modelu

t.test(FEV~Smoke, data=lungcap, alternative="two.sided", var.equal=FALSE)

# Kaj pomenijo parametri modela, če vključimo kot napovedno spremenljivko samo eno opisno spremenljivko?
# Kako je FEV odvisna od Smoke?

mod.opisna <- lm(FEV~Smoke, data=lungcap)
par(mfrow=c(2,2))
plot(mod.opisna)

# Ostanki niso idealni (tega zdaj ne bomo reševali)

summary(mod.opisna)
mod.opisna$coeff
summary(mod.opisna)$r.squared

plot(Effect(c("Smoke"), mod.opisna), multiline=TRUE, 
     ci.style="bar", main="", lty=0)

# Na podlagi spremenljivk Gender in Smoke naredimo novo spremenljivko 
# Gender.Smoke  

lungcap$Gender.Smoke <- lungcap$Gender:lungcap$Smoke
levels(lungcap$Gender.Smoke) # spremenljivka ima 4 vrednosti/kategorije

# FEV v odvisnosti od Gender in Smoke

ggplot(lungcap, aes(x=Smoke, y=FEV))+ geom_boxplot() +  facet_wrap(.~ Gender) + 
  xlab("Status kajenja") + ylab("Pljučna kapaciteta (L)") + theme_bw()

# model za odvisnost FEV od Gender.Smoke

mod.opisna.4 <- lm(FEV ~ Gender.Smoke, data=lungcap)
par(mfrow=c(2,2))
plot(mod.opisna.4)

# ostanki še vedno kažejo, da predpostavke linearnega modela niso izpolnjene
# za vajo interpretiramo samo ocene parametrov (brez standardnih napak)  
# in r^2

summary(mod.opisna.4)

plot(Effect(c("Gender.Smoke"), mod.opisna.4), multiline=TRUE, 
     ci.style="bar", main="", lty=0)

# Dve opisni spremenljivki v modelu

mod.opisni2 <- lm(FEV ~ Smoke + Gender, data=lungcap)
par(mfrow=c(2,2))
plot(mod.opisni2)

summary(mod.opisni2)
plot(Effect(c("Gender","Smoke"), mod.opisni2), multiline=TRUE, 
     ci.style="bar", main="", lty=0)

# Dve opisni spremenljivki in njuna interakcija v modelu

mod.opisni2.int <- lm(FEV~Smoke*Gender, data=lungcap)
par(mfrow=c(2,2))
plot(mod.opisni2.int)

summary(mod.opisni2.int)

# ŽN y = 2,38
# ŽK y = 2,38 + 0,59
# MN y = 2,38 + 0,36
# MK y = 2,38 + 0,59 + 0,36 + 0,42


plot(Effect(c("Gender","Smoke"), mod.opisni2.int), multiline=TRUE, 
     ci.style="bar", main="", lty=0)

# Številska in dve opisni spremenljivki v modelu

ggplot(lungcap, aes(x=Ht, y=FEV))+ geom_point() + geom_smooth(se=FALSE) + 
  facet_grid(Smoke~ Gender) + xlab("Telesna višina (cm)") + 
  ylab("Pljučna kapaciteta (L)") + theme_bw()

mod3 <- lm(FEV ~ Gender + Smoke + Ht, data=lungcap) # brez interakcij
par(mfrow=c(2,2))
plot(mod3)

summary(mod3)

# ŽN y = -5,36 + 0,05
# ŽK y = -5,36 + 0,03 + 0,05
# MN y = -5,36 + 0,13 + 0,05
# MK y = -5,36 + 0,13 + 0,03 + 0,05

plot(Effect(c("Ht", "Smoke","Gender"), mod3), multiline=TRUE, ci.style="band", main="")

# grafikon parcialnih ostankov

plot(Effect(c("Ht","Gender","Smoke"), mod3, partial.residuals=TRUE), main="")

# Številska, dve opisni spremenljivki ter njihove interakcije v modelu

mod3.int <- lm(FEV ~ Gender * Smoke * Ht, data=lungcap) 
# enak model lahko na dolgo zapišemo:
# mod3.int <- lm(FEV ~ Gender + Smoke + Ht + 
#                  Gender : Smoke + Gender : Ht + Smoke : Ht +
#                  Gender : Smoke : Ht, data=lungcap) 
par(mfrow=c(2,2))
plot(mod3.int)

summary(mod3.int)
# ŽN y = -4,4 + 0,04Ht
# ŽK y = (-4,40 + 4,37) + (0,04 - 0,03)Ht
# MN y = (-4,40 - 1,31) + (0,04 + 0,01)Ht
# MK y = (-4,40 - 1,31 + 4,38 - 8,96) + (0,04 + 0,01 - 0,03 + 0,05)Ht

# za intercepr uzamemo vse k nimajo Ht(številska spremenljivka za naklon)

# yi = b0 + b1*wgi + b2*wsi + b3*Ht + b4*wg:ws + b5*wg:Ht + b6*ws:Ht + b7*wg:ws:Ht + epsi
# Pogledamo ali so interakcije sploh pomembne
# H0: b4 = b5 = b6 = b7 = 0
anova(mod3, mod3.int)

# grafikon parcialnih ostankov za model z interakcijami

plot(Effect(c("Ht","Gender","Smoke"), mod3.int, partial.residuals=TRUE), main="")

plot(Effect(c("Ht", "Smoke","Gender"), mod3.int), multiline=TRUE, 
     ci.style="band", main="")

#  FEV ~ Age + Ht + Gender + Smoke, še nekaj grafičnih prikazov

ggplot(lungcap, aes(x=Age, y=FEV))+ geom_point() + geom_smooth(se=FALSE) + 
  facet_grid(Smoke~ Gender) + xlab("Starost (dopolnjena leta)") + 
  ylab("Pljučna kapaciteta (L)") + theme_bw()

mod1 <- lm(FEV ~ Age + Ht + Gender + Smoke, data=lungcap)

head(model.matrix(mod1))  # prvih šet vrstic modelske matrike X
tail(model.matrix(mod1))  # zadnjih šest vrstic modelske matrike X

par(mfrow=c(2,2), oma = c(0, 0, 3, 0))
plot(mod1)

# ali lahko s transformacijo log(FEV) izpolnimo predpostavke linearnega modela?

ggplot(lungcap, aes(x=Age, y=log(FEV)))+ geom_point() + geom_smooth(se=FALSE) +
  facet_grid(Smoke~ Gender) + xlab("Starost (dopolnjena leta)") + 
  ylab("log(Pljučna kapaciteta (L))") + theme_bw()

ggplot(lungcap, aes(x=Ht, y=log(FEV)))+ geom_point() + geom_smooth(se=FALSE) + 
  facet_grid(Smoke~ Gender) + xlab("Telesna višina (cm)") + 
  ylab("log(Pljučna kapaciteta (L))") + theme_bw()

# Po logaritmiranju nimamo več heteroskedastičnosti

mod2 <- lm(log(FEV) ~ Age + Ht + Gender + Smoke, data=lungcap)

par(mfrow=c(2,2), oma = c(0, 0, 3, 0))
plot(mod2)

avPlots(mod2)
crPlots(mod2)

#  izračun z matrikami

# log(lungcap$FEV)[c(1:3, 654)]
# lungcap$Age[c(1:3, 654)]
# lungcap$Ht[c(1:3, 654)]
# lungcap$Gender[c(1:3, 654)]
# lungcap$Smoke[c(1:3, 654)]
# 
# Xmat <- model.matrix(~ Age + Ht + Gender + Smoke, data=lungcap)
# head(Xmat)
# tail(Xmat)
# 
# XtX <- t(Xmat) %*% Xmat # t() transponiranje matrike; %*% množenje matrik
# y <- log(lungcap$FEV)
# inv.XtX <- solve(XtX) # solve() vrne inverzno matriko
# XtY <- t(Xmat) %*% y
# beta <- inv.XtX %*% XtY
# round(drop(beta), 5)
# 
# beta <- solve(XtX, XtY); round(beta, 5)
# 
# QR <-qr(Xmat)
# beta <- qr.coef(QR, y); round(beta, 5)
# 
# y.hat <- Xmat %*% beta
# SSost <- sum((y-y.hat)^2); SSost
# s2 <- SSost / (length(lungcap$FEV) - length(beta))
# round(c(s=sqrt(s2), s2=s2), 4)
# 
# x0.vek <- matrix(c(1, 18, 168, 0, 1), nrow=1) # prva komponenta vektorja je konstanta
# y0.x0 <- x0.vek %*% beta
# var.y0.x0 <- sqrt(x0.vek %*% (solve(t(Xmat) %*% Xmat)) %*% t(x0.vek)*s2)
# round(c(y0.x0, var.y0.x0, sqrt(var.y0.x0)),3)
# 
# names(mod2)
# names(summary(mod2))
# round(vcov(mod2), 7)
# round(sqrt(diag(vcov(mod2))),5)

# Dodatni diagnostični grafikoni dodane spremenljivke in parcialnih ostankov:

# sekvenčni F-testi

anova(mod2)

summary(mod2)

confint(mod2)

plot(Effect(c("Smoke", "Gender", "Age"), mod2), 
     multiline=TRUE, ci.style="bands", main="", ylim=c(0.1,1.6))

plot(Effect(c("Smoke", "Gender", "Age"), mod2, transformation = list(link = log,inverse = exp)), 
     axes = list(y = list(lab = "FEV (L)", type = "response")),
     multiline=TRUE, ci.style="bands", main="", ylim=c(1, 5))

plot(Effect(c("Smoke", "Gender", "Ht"), mod2), multiline=TRUE, 
     ci.style="bands", main="", ylim=c(0.1,1.6))

plot(Effect(c("Smoke", "Gender", "Ht"), mod2, transformation = list(link = log,inverse = exp)), 
     axes = list(y = list(lab = "FEV (L)", type = "response")), multiline=TRUE, 
     ci.style="bands", main="", ylim=c(1, 5))

# funkcija predict() ne dela s šumniki zato ravni Gender spremenimo in ponovimo modeliranje

levels(lungcap$Gender) <- c("Z", "M") # funkcija predict() ne dela s šumniki
mod2a <- lm(log(FEV) ~ Age + Ht + Gender + Smoke, data=lungcap)

# vrednosti napovednih spremenljivk pri katerih napovedujemo povprečno vrednost odzivne spremenljivke
# zapišemo v podatkovni okvir z enakimi imeni spremenljivk

novi.df <- data.frame (Age=c(17, 18, 19), Ht=c(168, 168, 168), Gender=c("Z", "Z", "Z"),
                       Smoke=c("Kadilec", "Kadilec","Kadilec"))

# povprečne napovedi za log(FEV) s pripadajočimi 95 % IZ

povp.napoved <- predict(mod2a, newdata=novi.df, interval="confidence")
cbind(novi.df, povp.napoved)

#  inverzna transformacija napovedi za FEV in pripadajoči 95 % IZ

cbind(novi.df, round(exp(povp.napoved), 2))

# posamične napovedi za log(FEV) s pripadajočimi 95 % IZ

pos.napoved <- predict(mod2a, newdata=novi.df, interval="prediction")
cbind(novi.df, pos.napoved)

#  inverzna transformacija napovedi za FEV in pripadajoči 95 % IZ

cbind(novi.df, round(exp(pos.napoved), 2))


# kaj pa interakcije med napovednimi spremenljivkami, ali se da model še izboljšati?

plot(Effect(c("Age","Ht","Gender","Smoke"), mod2, partial.residuals=TRUE), main="")


mod2.int <- lm(log(FEV) ~ Age*Ht*Gender*Smoke, data=lungcap) 
anova(mod2, mod2.int)

plot(Effect(c("Age","Ht","Gender","Smoke"), mod2.int, partial.residuals=TRUE), main="")

anova(mod2.int1)