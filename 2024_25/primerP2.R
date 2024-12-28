tlak <- read.table("SKT.txt", header=TRUE, sep="\t", stringsAsFactors = TRUE)

library(ggplot2)
ggplot(data=tlak, mapping=aes(x=starost, y=SKT)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  xlab("Starost (leta)") +
  ylab("SKT (mm Hg)")

model.SKT <- lm(SKT~starost, data=tlak)
model.SKT.0 = lm(SKT~starost - mean(tlak$starost), data=tlak)
summary(model.SKT.0)
par(mfrow=c(2,2))
plot(model.SKT)
par(mfrow=c(1,1))

model.SKT <- lm(SKT ~ starost, data = tlak)

# modelska matrika X

X <- model.matrix(model.SKT)  

# prvih 5 vrstic

X[1:5,]  

# Kaj je v matriki t(X) %*% X:

t(X) %*% X  #  n, vsota x, vsota x*x

length(tlak$starost)
sum(tlak$starost)
sum(tlak$starost^2)

# kaj je v matriki t(X) %*% tlak$SKT

t(X) %*% tlak$SKT  # vsota y, vsota x*y

sum(tlak$SKT)
sum(tlak$starost*tlak$SKT)

# ocene parametrov

# inverzna matrika t(X) %*% X

solve(t(X) %*% X)

(b <- solve(t(X) %*% X) %*% t(X) %*% tlak$SKT)


# variančno kovariančna matrika cenilk parametrov
(s2 <- sum(model.SKT$residuals^2)/(length(tlak$starost)-2))
(var_b <- s2*solve(t(X) %*% X))

vcov(model.SKT)

# standardne napake ocen parametrov
sqrt(diag(vcov(model.SKT)))

summary(model.SKT)$coeff

# matrika H
H <- X %*% solve(t(X) %*% X) %*% t(X)
dim(H)

# izpis prvih 10 stolpcev in 10 vrstic matrike H

round(H[1:10,1:10], 3)  
round(hatvalues(model.SKT),3)
diag(H)
plot(hatvalues(model.SKT),diag(H))
