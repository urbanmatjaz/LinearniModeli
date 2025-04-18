# linearni mešani modeli

library(nlme)
head(Orthodont)
class(Orthodont)
str(as.data.frame(Orthodont))

library(ggplot2)
library(dplyr)
gd <- Orthodont %>% group_by(age,Sex) %>% summarise(distance = mean(distance))
gd
ggplot(data=as.data.frame(Orthodont), aes(x=age, y=distance, col=Sex)) + 
  geom_point()+geom_point(data = gd, size = 3, shape = 17) +
  geom_line(data=gd)     

# model brez slučajnega vpliva in necentrirana starost
mod.lm0<-lm(distance~age*Sex, data=Orthodont)
round(vcov(mod.lm0), 2)

mod.lm<-lm(distance~I(age-11)*Sex, data=Orthodont)
round(vcov(mod.lm), 2)
summary(mod.lm)

par(oma=c(0,0,2,0), mfrow=c(2,2))
plot(mod.lm)
par(mfrow=c(1,1))

# pogledamo, kako so ostanki porazdeljeni glede na Subject
df <- data.frame(Orthodont$Subject, residuals(mod.lm))

ggplot(df, aes(x=residuals(mod.lm), y=Orthodont$Subject)) + geom_point() + theme_bw()

# še drugače prikazano (boxplot na štirih podatkih???)

ggplot(df, aes(x=residuals(mod.lm), y=Orthodont$Subject))+ geom_boxplot() + theme_bw()


# razsevni grafikoni po Subject
plot(Orthodont, ylab="Distance (mm)", xlab="Age (leta)", pch=16)

# linearni model za vsako osebo posebej
mod.list<-lmList(distance~I(age-11), data=Orthodont)
summary(mod.list)

# enak model dobimo z lm funkcijo:
# mod <- lm(distance~I(age-11)*Subject, data=Orthodont)
# summary(mod)

# grafični prikaz intervalov zaupanja za parametre modela
plot(intervals(mod.list))

# linearni mešani model s slučajnim vplivom osebe na presečišče in na naklon
mod.lme1<-lme(distance~I(age-11)*Sex, random=~I(age-11)|Subject,
              data=Orthodont, method="REML")
summary(mod.lme1)

# izpustimo slučajni vpliv osebe na naklon
mod.lme2<-lme(distance~I(age-11)*Sex, random=~1|Subject,data=Orthodont)

anova(mod.lme1, mod.lme2)
# anova deluje na podlagi logaritma razmerja verjetij ne na podlagi f testa

summary(mod.lme2)
intervals(mod.lme2)
confint(mod.lm)
plot(mod.lme2)
plot(mod.lme2, Subject~resid(.), abline=0,pch=16)
plot(mod.lme2, resid(.,type="pearson")~fitted(.)|Sex,id=0.05,adj=-0.3,pch=16)

# modeliranje nekonstantne variance
mod.lme3<-lme(distance~I(age-11)*Sex, random=~1|Subject, 
              weights=varIdent(form=~1|Sex), data=Orthodont)
summary(mod.lme3)
anova(mod.lme2,mod.lme3)
plot(mod.lme3, resid(.,type="pearson")~fitted(.)|Sex, 
     id=0.05, adj=-0.3, pch=16)
qqnorm(mod.lme3, ~resid(.,type="pearson")|Sex, abline=c(0, 1), lty=2,
       grid=T, id=0.05, adj=0.5, pch=16)
library(car)
compareCoefs(mod.lme2,mod.lme3)

# napovedi slučajnih vplivov
u0<-random.effects(mod.lme3)
u0
mean(u0[,1])
sd(u0[,1])

# testiranje domnev o parametrih LMM

library(emmeans)

# primerjava povprečne distance pri povprečni starosti 11 let, 
# pri ostalih starostih pa to ne velja.:

(test.povp <- emmeans(mod.lme3, pairwise ~ Sex))
confint(test.povp)

# primerjava naklonov premic

test <- emtrends(mod.lme3, pairwise ~ Sex, var="I(age-11)")
summary(test)
confint(test)

# napovedovanje

cbind(Orthodont, round(fitted(mod.lme3, level=0:1),1))[1:8,]
cbind(Orthodont, round(fitted(mod.lme3, level=0:1),1))[101:108,]

plot(augPred(mod.lme3, level=0), grid=T, pch=16) 
plot(augPred(mod.lme3, level=1), pch=16, grid=T) 

library(effects)
# napovedi za povprečno razdaljo glede na starost za moške in ženske s pripadajočimi 95% iz za povp. razdaljo v odvisnosti od starosti 
plot(effect(c("I(age-11)","Sex"), mod.lme3), ci.style="bands", multiline=T, main="") 

novaOrth<-data.frame(Subject=rep(c("M10","M11","F03"),c(2,2,2)),
                     Sex=rep(c("Male","Male","Female"),c(2,2,2)),
                     age=rep(c(13,15),3))
novaOrth
predict(mod.lme3, newdata=novaOrth,level=0:1)


# alternativni pristop z modeliranjem variančno-kovariančne matrike napak

# modeliramo različne varince v različnih časih meritve in tudi koreliranost napak v času
# ocenjujemo tri parametre, ki ocenjujejo razmerje standardnih odklonov distance v različnih časih
# ocenjujemo 4*(4-1)/2 korelacijskih koeficientov korelacijske matrike napak

mod.gls1<-gls(distance~I(age-11)*Sex, 
              weights=varIdent(form=~1|age),
              correlation=corSymm(form=~1|Subject), data=Orthodont)
summary(mod.gls1)
intervals(mod.gls1)
# ocene parametrov variančne matrike napak so zelo podobne, 
# vrednost 1 je znotraj 95% IZ, predpostavimo, da ne gre za različno varianco glede na čas meritve
# korelacijsko matriko napak poenostavimo - vsi koeficienti korelacije,
# ne glede na odlog, so enaki

mod.gls2<-gls(distance~I(age-11)*Sex,weights=NULL,
              correlation=corCompSymm(form=~1|Subject), data=Orthodont)
summary(mod.gls2)

# preverimo, ali sta modela enakovredna

anova(mod.gls1,mod.gls2)

# analiza ostankov

plot.lme(mod.gls2, resid(.,type="n")~age|Sex,id=0.05,adj=0.5, pch=16)

# kaže, da je potrebno modelirati različno varianco za moške in za ženske

mod.gls3<-gls(distance~I(age-11)*Sex,
              weights=varIdent(form=~1|Sex),
              correlation=corCompSymm(form=~1|Subject), data=Orthodont)
anova(mod.gls2,mod.gls3)

plot(mod.gls3, resid(.,type="n")~age|Sex,id=0.05,adj=0.5,pch=16)
qqnorm(mod.gls3, ~resid(.,type="n")|Sex, abline=c(0, 1), id=0.05, adj=0.5, pch=16)
summary(mod.gls3)

# primerjamo lme in gls modela

anova(mod.lme3,mod.gls3) # če mamo isto št parametrov (df) ne moremo računat z logaritmom razmerja verjetij
compareCoefs(mod.lme3,mod.gls3)

library(effects)
plot(effect(c("I(age-11)","Sex"), mod.gls3), ci.style="bands", multiline=T, main="") 


# VAJA jajčni folikli

# library(nlme)
head(Ovary)
summary(Ovary)
str(Ovary)

# ker je podatkovni okvir vrste groupedData:

plot(Ovary, type=c("p","l","smooth"), pch=16) 

mod.o0.lme <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), 
                  random=~sin(2*pi*Time) + cos(2*pi*Time)|Mare, data=Ovary)
intervals(mod.o0.lme)
mod.o1.lme <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), 
                  random=pdDiag(~sin(2*pi*Time) + cos(2*pi*Time)),
                  data=Ovary)
intervals(mod.o1.lme)

# diagonalna variančno-kovariančna matrika slučajnih vplivov (kovariance postavimo na 0)

mod.o2.lme <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), 
                  random=pdDiag(~sin(2*pi*Time)),
                  data=Ovary)
anova(mod.o1.lme, mod.o2.lme )

plot(mod.o1.lme, resid(.,type="p")~Time|Mare, pch=16, type="b")

# ocene avtokorelacijskih koeficientov za različne odloge

ACF(mod.o1.lme, maxLag=10)

# grafični prikaz avtokorelogram 

plot(ACF(mod.o1.lme, maxLag=10, resType="n"), alpha=0.05)

# za napake predpostavimo avtoregresiski model prvega reda

mod.o1.lme.cor <- update(mod.o1.lme, correlation=corAR1())
anova(mod.o1.lme, mod.o1.lme.cor)
intervals(mod.o1.lme.cor)

mod.o2.lme.cor <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), 
                      random=pdDiag(~sin(2*pi*Time)), correlation=corAR1(), data=Ovary)  
anova(mod.o1.lme.cor, mod.o2.lme.cor)
summary(mod.o2.lme.cor)

mod.o3.lme.cor <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), 
                      random=pdDiag(~1), correlation=corAR1(), data=Ovary)  

# random člen lahko zapišemo preprosteje
# mod.o3.lme.cor <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), 
#                       random=~1, correlation=corAR1(), data=Ovary)  

anova(mod.o2.lme.cor, mod.o3.lme.cor)
intervals(mod.o3.lme.cor)

plot(ACF(mod.o3.lme.cor, maxLag=10, resType="n"), alpha=0.05)
par(mfrow=c(1,2))
plot(fitted(mod.o3.lme.cor), resid(mod.o3.lme.cor, type="n"), pch=16)
abline(h=0)
qqnorm(resid(mod.o3.lme.cor, type="n"))
abline(a=0,b=1)

# modelske napovedi za populacijo

plot(augPred(mod.o3.lme.cor, level=0), grid=T, pch=16) 

# modelske napovedi za posamezno kobilo

plot(augPred(mod.o3.lme.cor, level=1), grid=T, pch=16) 
