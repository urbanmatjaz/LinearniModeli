tlak <- read.table("SKT.txt", header=TRUE, sep="\t", stringsAsFactors = TRUE)
head(tlak)
str(tlak)
summary(tlak)

library(ggplot2)
ggplot(data=tlak) +
  geom_point(mapping=aes(x=starost, y=SKT)) +
  xlab("Starost (leta)") +
  ylab("SKT (mm Hg)")

model.SKT <- lm(SKT~starost, data=tlak)
names(model.SKT)
model.SKT$coeff
model.SKT$fitted.values

ggplot(data=tlak, mapping=aes(x=starost, y=SKT)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  xlab("Starost (leta)") +
  ylab("SKT (mm Hg)")


par(mfrow=c(2,2))
plot(model.SKT)
par(mfrow=c(1,1))

# peš izračun standardiziranih ostankov in kvantilov standardizirane normalne porazdelitve

e <- model.SKT$residuals

# vektor vzvodov
h <- hatvalues(model.SKT)

# kako so vzvodi odvisni od x
plot(tlak$starost,h)
abline(v=mean(tlak$starost))

# standardna napaka regresije
s <- sqrt(sum(e^2)/(length(e)-2)) 

# standardizirani ostanki
e.stand <- e/(s*sqrt(1-h))
summary(e.stand)

# kvantili standardizirane normalne porazdelitve od 1/69 do 1 po 1/69
q <- qnorm(seq(from=1/69, to=1, by=1/69), mean=0, sd=1)
par(mfrow=c(1,1))
# Q-Q graf, kvantilni grafikon
plot(q,sort(e.stand))
abline(a=0,b=1, lty=2)
abline(h=0, lty=2, col="grey")

#############################################################

# povzetek modela
summary(model.SKT)
names(summary(model.SKT))

# variančno kovariančna matrika ocen parametrov
vcov(model.SKT)
# standardne napake ocen parametrov
sqrt(diag(vcov(model.SKT)))

# centriranje spremenljivke starost
tlak$starost.cent<- tlak$starost-mean(tlak$starost)
model.SKT.cent<- lm(SKT~starost.cent, data=tlak)
par(mfrow=c(2,2))
plot(model.SKT.cent)
summary(model.SKT.cent)
vcov(model.SKT.cent)


# t-test peš
t.b0<-model.SKT$coef[1]/sqrt(vcov(model.SKT)[1,1]); t.b0
t.b1<-model.SKT$coef[2]/sqrt(vcov(model.SKT)[2,2]); t.b1


# intervali zaupanja za parametre
confint(model.SKT) 

# peš izračun koeficienta determinacije
SS_model<-sum((model.SKT$fitted-mean(tlak$SKT))^2);SS_model
SS_res<-sum(model.SKT$residual^2);SS_res
R2<-SS_model/(SS_model+SS_res); R2

#tabela anova za model
anova(model.SKT)

# napovedovanje na osnovi modela

# izbrane vrednosti napovedne spremenljivke
starost.napovedi<-data.frame(starost=c(30,60))

# povprečne napovedi
povp.napovedi.SKT<-predict(model.SKT, starost.napovedi, interval="confidence")
data.frame(cbind(starost.napovedi,povp.napovedi.SKT ))

# posamične napovedi
pos.napovedi.SKT<-predict(model.SKT, starost.napovedi, interval="prediction")
data.frame(cbind(starost.napovedi,pos.napovedi.SKT ))

ggplot(data=tlak, mapping=aes(x=starost, y=SKT)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE) +
  xlab("Starost (leta)") +
  ylab("SKT (mm)") 

