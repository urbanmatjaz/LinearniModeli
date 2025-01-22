# Primer POSTAJE (Box-Cox transformacija)

postaje<-read.table("POSTAJE.txt", header=TRUE, sep="\t")
str(postaje)
head(postaje)
summary(postaje)
rownames(postaje)<-postaje$Postaja
rownames(postaje)[is.na(postaje$x.gdol)]
rownames(postaje)[is.na(postaje$y.gsir)]
postaje <- postaje[postaje$Postaja != "Kredarica",]

# koordinate geografske dolžine in širine izrazimo v km
postaje$x <- postaje$x.gdol/1000 
postaje$y <- postaje$y.gsir/1000

# izločimo dve postaji, ki nimata podatka za geografsko dolžino
postaje <- na.omit(postaje)
dim(postaje)

# za grafični prikaz 
library(car)
scatterplot(padavine ~ x, regLine = FALSE, smooth = FALSE, boxplots = 'xy',
            xlab = c("Geografska dolžina (km)"), ylab = c("Padavine (mm)"),
            data = postaje, pch = 16,
            id = list(n = 2, location = "lr")) # id = TRUE

# vidimo, da količina padavin v sloveniji od zahoda proti vzhodu pada. 
# Variablinost padavin prav tako pada v zahoda proti vzhodu.
# zveza ni linearna. (mogoče vseeno treba sprobat)
# Če imamo box plot lahko povemo tudi kaj o porazdelitvi spremenljivk
# omenimo osamelce


model.1 <- lm(padavine~x, data=postaje)
par(mfrow = c(2, 2))
plot(model.1)


par(mfrow = c(1, 1))
symbox(~padavine, xlab= "Lambda", ylab="Transformirane vrednosti za padavine",
       data=postaje)
summary(powerTransform(model.1))
boxCox(model.1)

# zberemo okroglo vrednost znotraj 95% IZ -> -1

# inverzna transformacija ne reši situacije

model.2 <- lm(1/padavine~x, data=postaje)
par(mfrow = c(2, 2), oma=c(0,0,3,0))
plot(model.2)

# Na podlagi diagnostike vidimo, da mamo še vedno heteroskedastičnost
# nelinearnost se je še povečala

# KOVINE

kovine0<-read.table("KOVINE.txt", header=TRUE, sep="\t")
kovine0$razdalja<-kovine0$razdalja.m/1000

# izločimo vzorčne točke z oddaljenostjo več kot 10 km
kovine<-kovine0[kovine0$razdalja<10,] 
dim(kovine)
summary(kovine[,c("Pb","razdalja")])

scatterplot(Pb~razdalja, regLine=F, xlab="Razdalja (km)",
            ylab="Pb (mg/kg)", smooth=list(span=0.5, spread=FALSE),
            boxplots='xy', data=kovine, pch=16)

# Večja oddaljenost, manj svinca
# Zveza ni linearna (eksponentna)
# Težava s heteroskedastičnostjo 
# ker gre za eksponentno zvezo odvisno spremenljivko logaritmiramo 


scatterplot(log(Pb)~razdalja, regLine=F, smooth=list(span=0.5, spread=FALSE),
            xlab="Razdalja (km)", ylab="log(Pb)",
            boxplots='xy', data=kovine, pch=16)

model.Pb <- lm(log(Pb)~razdalja, data=kovine)
par(mfrow = c(2, 2),  oma = c(0, 0, 2, 0))
plot(model.Pb)

# znebili smo se heteroskedastičnti, residuali so razen na repih
# normalno porazdeljeni

par(mfrow = c(1, 1))
influencePlot(model.Pb, id.n=2)


# Mamo 3 kandidate za regresijske osamelce (<2 ali >-2)
# navpične črte predstavljajo 2kratnik in 3 kratnik povprečnega vzvoda
# če je oboje od tega je vplivna točka

outlierTest(model.Pb)

# Točka 105 je bila kandidat ampak po testu ne moremo zavrniti H0, da ni outlier.
# Gledamo Bonferonijev popravek (unadjusted p * število testov (n = 103))

summary(model.Pb)

# Če smo čisto pri cinkarni je logaritem svinca enak 5,36
# Če se odmaknemo za 1km se logaritem svinca zmanjša za 0,2

confint(model.Pb)
(b <- coefficients(model.Pb))

# antilogaritmiranje ocen parametrov za povprečno napoved pri cinkarni
# in za relativno spremembo Pb z razdaljo ter pripadajoči 95 % IZ
exp(b[1])
exp(confint(model.Pb)[1,])

# Pri 95% gotovosti lahko trdimo, da je povprečna vsebnost svinca pri 
# cinkarni med 163.7072 in 278.7921

exp(b[2])-1
# za vsak km stran od cinkarne v povprečju vsebnost svinca pade za 20%
exp(confint(model.Pb)[2,])-1

#  grafični prikaz napovedi s paketom effects
library(lattice)
library(effects)
plot(Effect(c("razdalja"), model.Pb), ci.style = "bands", main = "", 
     xlab = "Razdalja (km)", ylab = "log(Pb)")

plot(Effect(c("razdalja"), model.Pb,
            transformation = list(link = log, inverse = exp)), 
     axes = list(y = list(lab = "Pb (mg/kg)", type = "response")),
     ci.style = "bands", main = "", xlab = "Razdalja (km)")

# ali pa  z ggplot2
library(ggplot2)
ggplot(data = kovine, aes(x = razdalja, y = log(Pb)))+ 
  geom_point() + 
  stat_smooth(method = "lm") + 
  xlab("Razdalja (km)") +
  ylab("log(Pb)")

ggplot(data = kovine, aes(x = razdalja, y = Pb)) + 
  geom_point() + 
  xlab("Razdalja (km)") + ylab("Pb (mg/kg)") + 
  stat_function(fun=function(x) exp(5.36427-0.21302*x))


# Interakcija dveh številskih spremenljivk na primeru padavin v Sloveniji

par(mfrow = c(1, 2))
with(postaje, plot(x, padavine, pch = 16)) 
with(postaje, plot(z.nv, padavine, pch = 16)) 

model.m1 <- lm(padavine ~ x + z.nv, data = postaje)
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
plot(model.m1)

# Tudi ta model še vedno ni kul, heteroskedastičnost

# grafikoni dodanih spremenljivk

avPlots(model.m1, ylim = c(-1000, 1500), id = list(location = "avoid")) 

# Pokaže zvezo med odzivno spremenljivko in napovedno 
# spremenljivko ob upoštevanu ostalih spremenljivk.
# Naklona teh premic sta točno naklona v modelu

summary(model.m1)

# če gremo za 1km proti vzhodu se količina padavin 
# zmanjša za 4,3 ob enaki nadmorski višini

# interakcija pomeni, da je vpliv ene spremenljivke na odzivno 
# odvisna od druge/ih spremenljivk

# grafikoni parcialnih ostankov
library(effects)
plot(Effect(c("z.nv", "x"), model.m1, partial.residuals = TRUE),
     ci.style = "none", lattice = list(layout = c(4, 1)))
# Če interakcije ni bi se gladilnik moral pokrivat z modelsko napovedjo
# V tem primeru sigurno moramo uporabit tudi interakcije

plot(Effect(c("x","z.nv"), model.m1, partial.residuals = TRUE),
     ci.style = "none", lattice = list(layout = c(4, 1)))

# vidimo, da model relativno dobro deluje za nižje nadmorske višine
# Večja nadmorska višina bolj vidimo potrebo po interakciji

model.m2 <- lm(padavine ~ z.nv + x + z.nv:x , data = postaje)
model.matrix(model.m2)

plot(Effect(c("z.nv", "x"), model.m2, partial.residuals = TRUE),
     ci.style = "none", lattice = list(layout = c(4, 1)))

# Vidimo, da modelske napovedi nimajo več istih naklonov
# vidimo, da se gladilniki veliko bolj ujemajo 
# (mamo mal podatkov, tko da mormo bit mal bolj prizanesljivi)

plot(Effect(c("x","z.nv"), model.m2, partial.residuals = TRUE),
     ci.style = "none", lattice = list(layout = c(4, 1)))

# Tukaj se še bolje vidi da se gladilniki ujemajo
# V 3 četrtini nadmorske višine vidimo da 5 točk vleče malo stran

par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
plot(model.m2)

# Mala izboljšava je, imamo pa še vedno heteroskedastičnost
# 

avPlots(model.m2, ylim = c(-1000, 600), id = list(location = "avoid")) 

# če je parameter interakcije negativen to pomeni, v tem primeru to pomeni, 
# da je pri manjših vrednosti (zahod) vpliv nadmorske višine večji 
# Kandidata za osamelce sta Idrija in Planica

outlierTest(model.m2)

# test na podlagi studentiziranega ostanka testira H0 da enota ni osamelec
# Vrne samo tiste, ki so regresijski osamelci (Povedat je treba tud kaj to pomen)

model.m2$coeff
summary(model.m2)$r.squared

# Model ki vsebuje geo. sirino, nadmorsko visino in interakcijo pojasni 
# 80% variabilnost

# napovedi modela
plot(predictorEffects(model.m2, ~.,xlevels = list(x = 4, z.nv = c(250, 500, 750, 1500))),
     rows = 2, cols = 1, main = "", layout = c(4,1))
# IZ so najozji pri povprecni vrednosti

# 1. Vidimo, da je vpliv nadmorske visine na zahodu pozitiven bolj kot gremo proti vzhou pa se naklon premice 
# spreminja in je na koncu ta vpliv že negativen

# 2. Vidi se da je vpliv geo sirine na padavine negativen in da naklon raste z nadmorsko visino
# ce mamo dva kraja na visini 1500 metrov in se premaknemo proti vzhodu bo razlika v padavinah večja 
# kot če smo na 300 metrov nadmorske visine

# MAMMALS (vplivnost točk, neenakomerna porazdelitev vrednosti napovedne spremenljivke)

# podatke najdemo v paketu MASS v podatkovnem okviru mammals
library(MASS)
data(mammals)
summary(mammals)

#  grafični prikaz odvisnosti mase možganov od mase telesa

scatterplot(brain ~ body, regLine = FALSE, smooth = FALSE, 
            id = list(method = 'mahal', n = 3), boxplots = 'xy', 
            xlab = "Masa telesa (kg)", 
            ylab = "Masa možganov (g)", pch = 16, data = mammals)

# Problem bodo predstavljali osamelci

# obe spremenljivki logaritmiramo, desetiški logaritem
# ker nas zanima proporcionalno -> 1Kg pri slonu =/= kot 1kg pri človeku
# Uporabimo desetiški log ker si lahko brez problema predstavljamo mase 
# številka na X osi predstavlja 10^x

scatterplot(log10(brain) ~ log10(body), regLine = FALSE, 
            smooth = list(span = 0.5, spread = FALSE),
            id = list(method = 'mahal', n = 3), boxplots = 'xy', 
            xlab = "log10(body)", ylab = "log10(brain)", pch = 16, 
            data = mammals)

model.m <- lm(log10(brain) ~ log10(body), data = mammals)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.m)
# Pri diagnostiki ni težav po pričakovanjih

outlierTest(model.m)

par(mfrow = c(1, 1))
influencePlot(model.m, xlab = "Vzvodi", ylab = "Studentizirani ostanki",
              id = list(n = 3, location = "avoid")) # n = 3 -> 3 enote z največjimi vrednostmi

# Slona sta vzvodni točki
# 3 kandidati za regresijske osamelce (outlier test tega ne pokaže tko da smo good)


summary(model.m)

# Intercept nima smisla (telesna masa nemore bit 0)
# Če se telesna masa poveča za 1% se bo masa možganov povečala za 0,75% 

confint(model.m)

# Primer: ANDY (WLS)

andy <- read.table("ANDY.txt", header = TRUE)
str(andy)

# višino dreves izrazimo v metrih
andy$height <- andy$height/3.2808 

# buckets naj bo opisna spremenljivka, ker je za obravnavanje kot številsko
# spremenljivko premalo vrednosti

andy$buckets <- factor(andy$buckets) 
summary(andy)
library(ggplot2)
ggplot(data = andy, aes(x = age, y = height, col = buckets)) +
  geom_point() + xlab("Starost (leta)") + ylab("Visina dreves (m)")

mod.OLS <- lm(height ~ age * buckets, data = andy)
par(mfrow = c(2, 2))
plot(mod.OLS)

# lahko izračunamo variance višine dreves pri posamezni starosti

library(dplyr)
utez <- andy %>% group_by(age) %>%
  summarise(w = 1/var(height))
utez
andy <- merge(andy, utez, by = "age")

# uteži glede na starost

par(mfrow = c(1, 1))
plot(andy$age, andy$w, pch = 16, type = c("b"))

#  WLS

mod.WLS <- lm(height ~ age * buckets, weights = w, data = andy)
par(mfrow = c(2, 2))
plot(mod.WLS)

# Pri prvem grafu se še vedno kaže heteroskedastičnost
# ampak je okej ker tm so še navadni ostaniki in še ni informacije o
# varianci

par(mfrow = c(1, 1))
plot(fitted(mod.WLS), rstandard(mod.WLS), xlab = "Prilagojene vrednosti", 
     ylab = "Standardizirani ostanki")
abline(h = 0)

anova(mod.WLS)

# samo za primerjavo

anova(mod.OLS)

# Sekvenčni F testi
# najprej vidimo a Age kej pojasn
# pol vidmo če dodamo temu še buckets če kej več vemo (vemo, ker 
# je statistično značilna)
# nakonc pa še a je interakcija kej več pojasn (ni statistično značilno, 
# ampak predpostavke modela niso izpolnjene tko da iz tega nemormo nč sklepat) 

# povzetek WLS modela

summary(mod.WLS)


plot(Effect(c("buckets"), mod.WLS), main = "", ylim = c(0, 16))

# Povprečja višine dreves za različno količino vode pri povprečni starosti

plot(Effect(c("buckets", "age"), mod.WLS), main = "", multiline = TRUE, 
     ci.style = "band", ylim = c(0, 16))
