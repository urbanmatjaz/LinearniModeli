# knjižnice
library(cvTools)
library(olsrr)
library(MASS)
library(vtable)
library(knitr)
library(kableExtra)
library(reshape2)
library(ggplot2)
library(corrplot)
library(dplyr)
library(car)

# Primer: bodyfat

bodyfat <- read.table("bodyfat.txt", header=T)
str(bodyfat)
summary(bodyfat)

# naredimo podatkovni okvir za grafični prikaz porazdelitve vseh spremenljivk

bodyfat_long <- melt(bodyfat)

qplot(value, data = bodyfat_long) + 
  facet_wrap(~variable, scales = "free") + 
  theme_bw() + 
  xlab("")

# matrika razsevnih grefikonov

pairs(bodyfat)

# grafični prikaz Pearsonovih koeficientov korelacije med napovednimi spremenljivkami

colors10 <- colorRampPalette(c("#0000aa","white","#aa0000"))(10)
corrplot.mixed(cor(bodyfat[,-1]),
               lower.col = colors10, upper.col = colors10,
               tl.col="black",   tl.cex = 0.7)



mod.polni <- lm(siri ~ ., data = bodyfat)
par(mfrow = c(2, 2))
plot(mod.polni)

# predpostavka linearnosti

crPlots(mod.polni)

summary(mod.polni)

# kako je s kolinearnostjo v modelu?

vif(mod.polni)

# Nad 5 je že opozorilo, nad 10 rabmo že ukrepat. 
# To kaže na kolinearnost aka spremenljivke 
# prinašajo zelo podobne informacije v model

#  PRESS ostanki in navadni ostanki (peš izračun)

e <- residuals(mod.polni)
e.press <- numeric()

for (i in 1:length(e)){
  mod <- lm(siri ~ ., data = bodyfat[-i,])
  novi <- bodyfat[i,]
  e.press[i] <- bodyfat[i,"siri"] - predict(mod, newdata = novi)
}
par(mfrow = c(1, 1))
plot(e,e.press, pch = 1)
abline(a = 0, b = 1, h = 0, col = (c("red", "blue")), lty = 2, lwd=2)
sum(e.press^2)
sum(e.press^2)-sum(e^2)

# PRESS-ostanke izračunamo hitreje na podlagi vzvodov

h <- hatvalues(mod.polni)
press.ost <- residuals(mod.polni)/(1 - h)
(PRESS <- sum(press.ost^2))

# PRVI KORAK izbire najboljše podmnožice modelov: za vsako število napovednih spremenljivk izberemo
# najboljšega glede na vsoto kvadratov ostankov RSS

# najprej pripravimo vektor formul za različne modele

nap.sprem <- names(bodyfat)
(nap.sprem <- nap.sprem[! nap.sprem %in% "siri"])
(k <- length(nap.sprem)) # k = 13

(formule <- paste("siri~", 1))

# id predstavlja vse možne kombinacije 13 spremenljivk

id <- unlist(lapply(1:k,function(i) combn(1:k, i, simplify = FALSE)),
             recursive = FALSE)
length(id)
2^k

formule[2:2^k] <- sapply(id, function(i) 
  paste("siri~", paste(nap.sprem[i], collapse="+")))

length(formule)

#  število napovednih spremenljivk v posameznem modelu

p <- c(0, sapply(id, length))

# podatkovni okvir za izpis rezultatov RSS
tabelaRSS <- data.frame(formula = formule, p = p)


# za vsak p izberemo najboljši model na podlagi vsote kvadriranih ostankov oz. R^2

RSS <- numeric()
for (i in (1:length(formule))){
  mod <- lm(formule[i], data = bodyfat)
  RSS[i] <- sum(resid(mod)^2)
}

tabelaRSS$RSS <- RSS 

best_p_model <- tabelaRSS %>% 
  group_by(p) %>%
  slice_min(order_by = RSS)

best_p_model
par(mfrow = c(1, 1))
plot(tabelaRSS$p, tabelaRSS$RSS, xlab="p", ylab=expression(SS[residuals]), ylim=c(0, 18000))
lines(best_p_model$p, best_p_model$RSS, pch=16, col="red", type="b")

# DRUGI KORAK IZBIRE modela z najboljšo napovedno kakovostjo
# PRESS statistika

PRESS <- numeric()

for (i in (1:(length(nap.sprem)+1))){
  mod <- lm(best_p_model$formula[i], data = bodyfat)
  h <- lm.influence(mod)$hat
  press.ost <- residuals(mod)/(1 - h)
  PRESS[i] <- sum(press.ost^2)
}

best_p_model$PRESS <- PRESS

#  narišimo RSS in PRESS-statistiko za primerjavo

plot(best_p_model$p[-1], best_p_model$RSS[-1], pch=16, ylim=c(4200,5700))
points(best_p_model$p[-1], best_p_model$PRESS[-1], col="red", pch=16)
izbrani <- best_p_model$p[best_p_model$PRESS==min(best_p_model$PRESS)]
abline(v = izbrani, col = "blue", lty = 2, lwd=2)

best_p_model$formula[izbrani+1]

# Navzkrižno preverjanje - osnovno, dva enako velika vzorca: učni in testni

(n <- dim(bodyfat)[1])
(n.u <- round(n/2))
(n.t <- floor(n/2))

# naredimo vektor z vrednostmi TRUE in FALSE, vsaka vrednost po n/2-krat

ind <- rep(c(TRUE, FALSE), c(n.u, n.t))

# slučajno razporedimo vrednosti

# set.seed(12345) # zaradi ponovljivosti

ind <- sample(ind)

bodyfat.ucni <- bodyfat[ind,]
bodyfat.test <- bodyfat[!ind,]

# izračun napovedi za testni vzorec na podlagi modela narejenega na učnem vzorcu

mod.ucni <- lm(siri ~ ., data = bodyfat.ucni)
y.nap <- predict(mod.ucni, bodyfat.test)

# kriterij navzkrižnega preverjanja CVC

(CVC <- sum((bodyfat.test$siri - y.nap)^2))

# za primerjavo izpis RSS

(RSS <- sum((mod.ucni$residuals)^2))

# povprečna kvadratna napaka napovedi MSE

(MSE <- CVC/n.t)

# povprečna napaka napovedi RMSE

(RMSE <- sqrt(CVC/n.t))

# Za preglednejše računanje bomo pri izbiri najboljše podmnožice spremenljivk na
# učenm vzorcu uporabili funkcijo naj.podmn, ki za 2^k možnih modelov za vsak p,
# izpiše model z največjo vrednostjo koeficienta determinacije 

# funkcija za izpis R^2
get_r2 <- function(formula, data) {
  model <- lm(formula, data = data)
  return(summary(model)$r.squared)
}

# najboljši model s p-spremenljivkami
naj.podmn <- function(formule, p, data){
  formula_i <- NULL
  for (i in 1:k){
    formule_p <- as.list(formule[which(p==i)])
    r2 <- unlist(lapply(formule_p, get_r2, data = data))
    formula_i[i] <- formule_p[which.max(r2)]
  }
  return(formula_i)
}

# funkcija za izračun napovedi na testnem vzorcu na podlagi 
# parametrov ocenjenih na učnem vzorcu
nap.test <- function(formula, pod_ucni, pod_test) {
  model <- lm(formula, data = pod_ucni)
  return(predict(model, pod_test))
}

# petkrat razdelimo podatke na dva enaka dela in primerjajmo RMSE

mse <- matrix(NA, 5, length(nap.sprem))

for (j in 1:5) {
  izbor <- rep(c(TRUE, FALSE), c(n.u, n.t))
  set.seed(j * 10)
  izbor <- sample(izbor)
  bodyfat.ucni <- bodyfat[izbor,]
  bodyfat.test <- bodyfat[!izbor,]
  
  naj_fit_ucni <- naj.podmn(formule, p, data=bodyfat.ucni)
  val_nap <- lapply(naj_fit_ucni, nap.test, pod_ucni = bodyfat.ucni,
                    pod_test = bodyfat.test)
  
  for(i in 1:length(nap.sprem)){
    mse[j, i] = mean((bodyfat.test$siri - val_nap[[i]])^2)
  }
}

plot(1:length(nap.sprem), mse[1, ], ylim=c(17, 25), xlab="p", ylab="MSE", type="b", pch=16)
points(which.min(mse[1, ]), min(mse[1, ]), pch=17, cex=2)
for(i in c(2:5)){
  lines(1:length(nap.sprem), mse[i, ], type="b", pch=16, col=i)
  points(which.min(mse[i, ]), min(mse[i, ]), pch=17, cex=2, col=i)
}

# zgornjo simulacijo naredimo nekajkrat še brez nastavljenega brsta generatorja slučajnih števil


# K-kratno navzkrižno preverjanje

K <- 10

set.seed(10)

folds <- sample(1:K, nrow(bodyfat), replace =TRUE)
table(folds)

mse <- matrix(NA, K, length(nap.sprem), 
              dimnames=list(NULL, paste(1:length(nap.sprem))))

for(j in 1:K){
  # izbiro najboljše podmnožice moramo ponoviti K-krat
  naj_fit <- naj.podmn(formule, p, data = bodyfat[folds!=j,])
  # napovedi za testni vzorec na podlagi ocen na učnem vzorcu
  pred <- lapply(naj_fit, nap.test, pod_ucni = bodyfat[folds!=j,],
                 pod_test = bodyfat[folds==j,])
  for(i in 1:length(nap.sprem)){
    mse[j, i] = mean((bodyfat$siri[folds==j] - pred[[i]])^2)
  }
}

mean.mse=apply(mse, 2, mean)
round(mean.mse, 2)

# optimalno število spremenljivk v modelu na podlagi 
# 5-kratnega navzkrižnega preverjanja 
best_p_model$formula[best_p_model$p==which.min(mean.mse)]

plot(c(1:13), best_p_model$RSS[-1]/nrow(bodyfat), type='b', ylim=c(17, 24), col="red", 
     xlab="p", ylab="MSE", pch=16)
lines(mean.mse, xlab = "p", ylab = "MSE", type="b", pch=16)
points(which.min(mean.mse), 
       mean.mse[which.min(mean.mse)], 
       pch = 17, cex=2)
lines(best_p_model$PRESS[-1]/nrow(bodyfat), type='b', col="blue", pch=16)

points(which.min(best_p_model$PRESS[-1]), 
       (best_p_model$PRESS[-1]/nrow(bodyfat))[which.min(best_p_model$PRESS[-1])], 
       pch = 17, cex=2, col="blue")

legend("topright", c("Ostanki", "PRESS-ostanki", "10-kratno navzkrižno preverjanje"), 
       col=c("red", "blue", "black"), lty=1, pch=16, bty="n")

# K-kratno navzkrižno preverjanje je veliko hitrejše, če uporabimo funkcijo regsubsets() iz paketa leaps:

library(leaps)

# funkcija, ki nam da napovedi za najboljše modele velikosti p

predict.regsubsets <- function(object, newdata, id){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

K <- 5
set.seed(12)
folds <- sample(1:K, nrow(bodyfat), replace =TRUE)

mse <- matrix(NA, K, length(nap.sprem), 
              dimnames=list(NULL, paste(1:length(nap.sprem))))

for(j in 1:K){
  naj.fit=regsubsets(siri ~., data=bodyfat[folds!=j,], nvmax=13) 
  # argument nvmax določa največji model
  for(i in 1:length(nap.sprem)){
    pred = predict(naj.fit, bodyfat[folds==j,], id=i)
    mse[j, i] = mean((bodyfat$siri[folds==j] - pred)^2)
  }
}

mean.mse <- apply(mse, 2, mean)
mean.mse

best_p_model$formula[best_p_model$p==which.min(mean.mse)]


#  Malowa Cp-statistika

best.fit = regsubsets(siri ~. , data = bodyfat, nvmax = 13)
summary(best.fit)$cp
best_p_model$formula[summary(best.fit)$cp==min(summary(best.fit)$cp)]

# AIC

AIC <- numeric()

for (i in (1:(length(nap.sprem)+1))){
  mod <- lm(best_p_model$formula[i], data = bodyfat)
  AIC[i] <- AIC(mod)
}

best_p_model$AIC <- AIC
round(best_p_model$AIC,2)
best_p_model$formula[best_p_model$AIC==min(best_p_model$AIC)]

# SEKVENČNE METODE
# Izbira naprej

# library(MASS)

mod.null <- lm(siri ~ 1, data = bodyfat)
stepFS <- stepAIC(mod.null, 
                  scope = ~age+weight+height+neck+chest+abdomen+hip+
                    thigh+knee+ankle+biceps+forearm+wrist, 
                  direction = "forward",
                  trace=FALSE)
stepFS$anova

# Izbira nazaj

stepBE <- stepAIC(mod.polni, direction = "backward", trace = FALSE)
stepBE$anova

# Izbira po korakih

mod.prvi <- lm(siri ~ abdomen + height, data = bodyfat)
stepB <- stepAIC(mod.prvi, 
                 scope = ~age+weight+height+neck+chest+abdomen+hip+
                   thigh+knee+ankle+biceps+forearm+wrist, 
                 direction = "both",
                 trace = FALSE)
stepB$anova

# stroka pravi, da morajo biti določene napovedne spremenljivke v vsakem primeru v modelu
# abdomen in height

stepFS2 <- stepAIC(lm(siri~abdomen+height, data=bodyfat),  
                   direction = "forward",
                   scope = ~age+weight+height+neck+chest+abdomen+hip+
                     thigh+knee+ankle+biceps+forearm+wrist,
                   trace = FALSE)

stepFS2$anova

stepBE2 <- stepAIC(mod.polni,  
                   direction = "backward",
                   scope = list(upper = formula(siri~age+weight+height+neck+
                                                  chest+abdomen+hip+thigh+knee+
                                                  ankle+biceps+forearm+wrist), 
                                lower = formula(siri~abdomen+height)),
                   trace = FALSE)

stepBE2$anova

# Problemi pri sekvenčnih metodah, primer

mod1 <- lm(siri ~ weight, data=bodyfat)
summary(mod1)$coefficients
summary(mod1)$r.squared


mod2 <- lm(siri ~ weight + height, data=bodyfat)
summary(mod2)$coefficients
summary(mod2)$r.squared

mod3 <- lm(siri ~ weight + abdomen, data=bodyfat)
summary(mod3)$coefficients
summary(mod3)$r.squared

mod4 <- lm(siri ~ weight + height + abdomen, data=bodyfat)
summary(mod4)$coefficients
summary(mod4)$r.squared

# Stabilnost modela izbranega z sekvenčno metodo izbira nazaj na podlagi bootstrapa

summary(stepBE)
par(mfrow = c(2, 2))
plot(stepBE)

# kreiramo 500 bootstrap vzorcev

B=500

# prazna matrika za vključenost posamezne napovedne spremenljivke (TRUE, FALSE)

rfs <- matrix(NA, nrow=B, ncol=length(nap.sprem))

# matrika z začetnimi vrednostmi 0 za vse ocene parametrov izbranih modelov

coefs <- matrix(0, nrow=B, ncol=length(nap.sprem))
colnames(rfs) <- colnames(coefs) <- names(bodyfat)[-1]

# prazen vektor za zapise formul izbranih modelov

msf <- vector(mode='character',length=B)

set.seed(333)

# v vsakem od B vzorcev ponovimo postopek izbire modela

for(i in 1:B){
  id <- sample(1:nrow(bodyfat), size = nrow(bodyfat), replace = T)
  bodyfat_sub <- bodyfat[id, ]
  stepBE_boot <- stepAIC(lm(siri ~ ., data=bodyfat_sub), 
                         direction = "backward", 
                         trace = FALSE)
  izbor <- names(coef(stepBE_boot))[-1]
  rfs[i, ] <- is.element(nap.sprem, izbor)
  msf[i] <- paste(names(coef(stepBE_boot)), collapse="+")
  coefs[i, which(is.element(nap.sprem, izbor)==T)] <- coef(stepBE_boot)[-1] 
}

stabilnost <- data.frame("koef_polni" = mod.polni$coefficients[-1], 
                         "SE_polni" = sqrt(diag(vcov(mod.polni)))[-1],
                         "koef_izbrani" = NA,
                         "se_izbrani" = NA,
                         "RFS" = apply(rfs, 2, sum)/500) # relativna frekvenca vključitve

stabilnost$koef_izbrani[which(is.element(nap.sprem, names(stepBE$coef)[-1])==T)] <- 
  stepBE$coefficients[-1]

stabilnost$se_izbrani[which(is.element(nap.sprem, names(stepBE$coef)[-1])==T)] <- 
  sqrt(diag(vcov(stepBE)))[-1] 

nap.sprem_order <- order(apply(rfs, 2, mean), decreasing=T)
stabilnost[with(stabilnost, order(-RFS)), ]

# Primerjava 95 \\% intervalov zaupanja za parametre modela, dobljenih z ocenami polnega modela, 
# modela izbranega z izbiro nazaj in na bootstrap vzorcih, kjer rdeča pika predstavlja mediano 
# bootstrap ocen parametrov.

ci_izbrani <- matrix(NA, length(nap.sprem), 2)
ci_izbrani[which(is.element(nap.sprem, names(stepBE$coef)[-1])==T), ] <- confint(stepBE)[-1,]

coefs_plot <- data.frame("Spremenljivka" = rep(nap.sprem, 3),
                         "Model" = rep(c("1-polni", "2-izbrani", "3-bootstrap"),
                                       each=length(nap.sprem)),
                         rbind(cbind(coef(mod.polni), confint(mod.polni))[-1,],
                               cbind(stabilnost$koef_izbrani, ci_izbrani),
                               cbind(apply(coefs, 2, median),
                                     apply(coefs, 2, function(x) quantile(x, probs=0.025)),
                                     apply(coefs, 2, function(x) quantile(x, probs=0.975)))
                         )   
)

coefs_plot[is.na(coefs_plot)] <- 0

colnames(coefs_plot) <- c("Spremenljivka", "Model", "ocena", "spodnja", "zgornja")

coefs_plot$Spremenljivka <- factor(coefs_plot$Spremenljivka, 
                                   levels = rownames(stabilnost)[nap.sprem_order])
coefs_plot$Model <- factor(coefs_plot$Model, levels =  unique(coefs_plot$Model)[3:1])

ggplot(aes(x=ocena, y= Spremenljivka, xmin =spodnja, xmax = zgornja, color = Model), 
       data = coefs_plot) +
  geom_point(position = position_dodge(0.9)) +
  geom_errorbarh(height = 0.2, position = position_dodge(0.9)) +
  theme_bw() +
  scale_y_discrete(limits=rev) +
  theme(legend.title = element_blank(), legend.position = "bottom")  + 
  guides(colour = guide_legend(reverse=T)) +
  xlab("Ocena parametra")

# Porazdelitve ocen parametrov za posamezne napovedne spremenljivke, dobljene na bootstrap vzorcih.

coefs_ordered <- coefs[, nap.sprem[nap.sprem_order]]

coefs_ordered_long <- melt(coefs_ordered)
qplot(value, data = coefs_ordered_long) +
  geom_vline(xintercept = 0, color = "blue") + 
  facet_wrap(~ Var2, scales = "free", ncol = 3) + 
  theme_bw() + 
  xlab("Ocene parametrov") +
  ylab("Število bootstrap vzorcev")

# Poglejmo si, kakšne so relativne frekvence izbire modela. 


#relativna frekvenca izbire modela

tabela_msf <- prop.table(table(msf)) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

# Relativne frekvence izbire modela za deset najpogosteje izbranih modelov na bootstrap vzorcih.

tabela_msf[1:10, ]

# relativna frekvenca vključitve parov spremenljivk

# matrika TRUE, FALSE za urejene ocene parametrov B izbranih bootstrap vzorcev

coefs_ordered_01 <- coefs_ordered != 0

# urejene vrednosti relativnih frekvenc vključenosti v model v odstotkih

rfs2 <- sort(apply(rfs, 2, mean), decreasing=T) * 100

# p-vrednost za hi-kvadrat test za ničelno domnevo, ki pravi,
# da je vključenost ene spremenljivke v paru neodvisna od vključenosti druge spremenljvke

pval <- 0.01

# matrika reda k x k za relativne frekvence vključenosti parov spremenljivk

pairfreq <- matrix(100, ncol = length(nap.sprem), nrow = length(nap.sprem),
                   dimnames = list(nap.sprem[nap.sprem_order], nap.sprem[nap.sprem_order]))

# pričakovane relativne frekvence

expect_pairfreq <- NULL

# vsi možni pari napovednih spremenljivk v matriki reda 2 x 78

combis <- combn(nap.sprem[nap.sprem_order], 2)

dim(combis)

# izračun relativnih frekvenc za posamezne pare in hi-kvadrat test

for (i in 1:dim(combis)[2]) {
  # relativna frekvenca prisotnosti para spremenljivk v B bootstrap izbranih modelih
  pairfreq[combis[1, i], combis[2, i]] <-
    sum(apply(coefs_ordered_01[, combis[, i]], 1, sum) == 2) / B * 100
  # pričakovana relativna frekvenca če je vključenost ene neodvisna od vključenosti druge
  expect_pairfreq[i] <-
    rfs2[grepl(combis[1, i], nap.sprem[nap.sprem_order])][1] *
    rfs2[grepl(combis[2, i], nap.sprem[nap.sprem_order])][1] / 100
  pairfreq[combis[2, i], combis[1, i]] <-
    ifelse(is(suppressWarnings(try(chisq.test(coefs_ordered_01[, combis[1, i]],
                                              coefs_ordered_01[, combis[2, i]]),
                                   silent = T)), "try-error"), NA,
           ifelse(suppressWarnings(chisq.test(coefs_ordered_01[,combis[1,i]],
                                              coefs_ordered_01[,combis[2,i]])$p.value)>pval,
                  "", 
                  ifelse(as.numeric(pairfreq[combis[1,i],combis[2,i]])<expect_pairfreq[i],
                         "-", "+")))
}

diag(pairfreq) <- rfs2

pairfreq <- pairfreq[!diag(pairfreq) == 100, !diag(pairfreq) == 100]

print(pairfreq, quote = F)
