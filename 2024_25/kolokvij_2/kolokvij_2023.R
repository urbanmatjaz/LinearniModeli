library(vtable) # summary table
library(kableExtra) # creates nice latex tables
library(corrplot) # correlation plot
library(car) # regression
library(reshape2) # reshape data sets for ggplot (melt)
library(ggplot2) # nice plots (ggplot)
library(knitr)  # for markdown 
library(leaps) # best subset
library(glmnet) # lasso
library(mgcv) # gam
library(summarytools) # summary table
library(effects)
library(gridExtra)
library(cvTools)
library(olsrr)
library(MASS)
library(lattice)
library(nlme)
library(emmeans)
library(tidyr)
library(patchwork)
library(splines)

train_set <- read.table("betaplazma.txt", header=TRUE)

#############################################################################
# 1. NALOGA
# Naredite smiseln statistični povzetek za vse spremenljivke v podatkovnem okviru in na
#kratko opišite podatke. Pri obrazložitvi si pomagajte z grafičnimi prikazi. Ali lahko za
# predpostavimo linearno odvisnost od posamezne številske spremenljivke.
# Utemeljite odgovor. 


num_var = train_set[, sapply(train_set, is.numeric)]

data_long_num = melt(num_var)

qplot(value, data = data_long_num) +
  facet_wrap(~variable, scales = "free") +
  theme_bw() +
  xlab("")
numeric_vars = names(num_var)
cor(train_set[, numeric_vars], use = "complete.obs")


colors10 <- colorRampPalette(c("#0000aa","white","#aa0000"))(10)
corrplot.mixed(cor(train_set[, numeric_vars],
                   method = "spearman"),
               lower.col = colors10, upper.col = colors10,
               tl.col="black",   tl.cex = 0.5, number.cex = 0.8)

pairs(train_set[, numeric_vars])



# scatter ploti po številskih spremenljivkah z log10 y skalo in fitted lm in loess gladilniki
plot_list <- list()

# Loop to create a ggplot for each numeric predictor
for (var in numeric_vars) {
  p <- ggplot(train_set, aes_string(x = var, y = "betaplazma")) +
    geom_point(alpha = 0.5, color = "#69b3a2") +
    geom_smooth(method = "loess", se = TRUE, color = "darkred") +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    theme_minimal() +
    scale_y_log10() +
    labs(title = paste("betaplazma vs", var), x = var, y = "log(betaplazma)")
  
  plot_list[[var]] <- p
}

# Combine all plots into a grid using patchwork
combined_plot <- wrap_plots(plot_list, ncol = 2)

# Print the final grid of plots
combined_plot

#############################################################################
# 2. NALOGA
# Naredite linearni model za odvisnost betaplazma od ostalih spremenljivk v podatkovnem okviru, 
# pri čemer uporabite ustrezno transformacijo odzivne spremenljivke.
# Pojasnite svojo izbiro transformacije. Naredite diagnostiko ostankov
# (osnovni diagnostični grafikoni, grafikoni dodane spremenljivke in grafikoni parcialnih ostankov).
# Ali so vse predpostavke linearnega modela izpolnjene? Obrazložite svojo trditev.

symbox(betaplazma ~ 1, data = train_set, powers = c(-2, -1, -0.5, 0, 0.5, 1, 2),
       main = "Symbox za betaplazma", ylab = "Transformirana betaplazma")

# tisti boc plot k izgleda najbolj simetricno je taprav

#If power = 0 (i.e., log) looks most symmetric → use log(betaplazma)
#If power = 0.5 → use sqrt(betaplazma)
#If power = -1 → use 1/betaplazma

model <- lm(log(betaplazma) ~ starost + spol + kajenje + ITM + vitamini + kalorije + mascoba + balasti + kolesterol + betadieta, data=train_set)

train_set$log_betaplazma = log(train_set$betaplazma)

par(mfrow = c(2, 2))  # Postavi 4 grafe v mrežo
plot(model)

# 1. Residuals vs Fitted (top left)
# Purpose: Checks linearity and homoskedasticity (equal variance of residuals).
# 
#  Interpretation:
#   
#   The residuals seem fairly randomly scattered around 0, which is good.
# 
# Slight curvature in the red line suggests a small departure from linearity, but it's not severe.
# 
# No major funneling → no obvious heteroskedasticity.
# 
# Conclusion: No strong violations here.

# 2. Normal Q-Q Plot (top right)
# Purpose: Checks whether the residuals are normally distributed.
# 
# Interpretation:
#   
#   The points closely follow the diagonal line, especially in the middle range.
# 
# There are a few outliers at both tails (e.g., obs 233, 282, 268), but that's not unusual for real-world data.
# 
# Conclusion: The normality assumption looks reasonably satisfied.

# 3. Scale-Location (bottom left)
# (aka Spread-Location plot)
# 
# Purpose: Again checks homoskedasticity, but now on standardized residuals.
# 
# Interpretation:
#   
#   The red line is fairly flat with a random spread of points around it.
# 
# Slight curvature, but no clear pattern or funneling.
# 
# Conclusion: No strong evidence of heteroskedasticity — this looks good.



par(mfrow = c(1, 1))  # Reset

# grafikoni dodane spremenljivke 
avPlots(model)

# Grafikoni parcialnih ostankov
crPlots(model)

#############################################################################
# 3. NALOGA
# Ali je v modelu prisotna kolinearnost? Obrazložite svojo trditev.

vif(model)

# Preverimo adjusted GVIF in glede an to da nobena vrednost ni 
# večja od 5 ni potrebno posegati v model kar se tiče kolinearnosti.

# 1	Brez kolinearnosti
# 1–5	Sprejemljivo, zmerna korelacija
# > 5	Povečana kolinearnost
# > 10	Močna kolinearnost – potencialna težava

#############################################################################
# 4. NALOGA
# Model dopolnite tako, da se znebite nelinearnosti zveze betaplazma in 
# starost ob upoštevanju ostalih spremenljivk v modelu. Uporabite naravne zlepke. Svojo izbiro števila
# vozlišč obrazložite na podlagi F-testa za primerjavo modelov. Kakšno transformacijo
# napovedne spremenljivke predstavljajo naravni zlepki?

model.ns3 <- lm(log_betaplazma ~ ns(starost, df = 2) + spol + kajenje + ITM + vitamini + kalorije + mascoba + balasti + kolesterol + betadieta, data = train_set)
model.ns4 <- lm(log_betaplazma ~ ns(starost, df = 3) + spol + kajenje + ITM + vitamini + kalorije + mascoba + balasti + kolesterol + betadieta, data = train_set)
model.ns5 <- lm(log_betaplazma ~ ns(starost, df = 4) + spol + kajenje + ITM + vitamini + kalorije + mascoba + balasti + kolesterol + betadieta, data = train_set)
model.ns6 <- lm(log_betaplazma ~ ns(starost, df = 5) + spol + kajenje + ITM + vitamini + kalorije + mascoba + balasti + kolesterol + betadieta, data = train_set)
model.ns7 <- lm(log_betaplazma ~ ns(starost, df = 6) + spol + kajenje + ITM + vitamini + kalorije + mascoba + balasti + kolesterol + betadieta, data = train_set)
model.ns8 <- lm(log_betaplazma ~ ns(starost, df = 7) + spol + kajenje + ITM + vitamini + kalorije + mascoba + balasti + kolesterol + betadieta, data = train_set)
model.ns9 <- lm(log_betaplazma ~ ns(starost, df = 8) + spol + kajenje + ITM + vitamini + kalorije + mascoba + balasti + kolesterol + betadieta, data = train_set)

AIC(model, model.ns4, model.ns5, model.ns6, model.ns7, model.ns8, model.ns9)[,2]

plot(1:8, AIC(model, model.ns3, model.ns4, model.ns5, model.ns6, model.ns7, model.ns8, model.ns9)[,2], xlab="Stopinje prostosti naravnega zlepka",
     ylab="AIC")

# Vidimo, da je najboljši model s 4 stopinjami prostosti, torej 5 vozlišči. To določimo s funkcijo AIC, 
# ne moremo uporabiti F testa če kompleksnost zlepka variiramo tako da povečujemo število stopinj prostosti,
# saj bodo vsakič tudi položaji vozlišč drugačni in s tem bazne funkcije. Ne gre torej za gnezdene modele!
  

# NE DELA: anova(model, model.ns3, model.ns4, model.ns5, model.ns6, model.ns7, model.ns8, model.ns9)

#############################################################################
# 5. NALOGA
# Število vozlišč izberite še ne podlagi PRESS-statistike modelov z različnim številom vozlišč za spremenljivko starost. 
# Ali rezultati sovpadajo z rezultati F-testa za primerjavo modelov?

calc_press <- function(model) {
  res <- resid(model)
  hii <- hatvalues(model)
  sum((res / (1 - hii))^2)
}

press_values = c(calc_press(model.ns3),calc_press(model.ns4),calc_press(model.ns5),calc_press(model.ns6),calc_press(model.ns7),calc_press(model.ns8),calc_press(model.ns9))

press_df = data.frame(
  df = 2:8,
  PRESS = round(press_values, 2)
)

plot(press_df$df, press_df$PRESS)
# Vidimo, da je najboljši model s 4 stopinjami prostosti, torej 5 vozlišči.


#############################################################################
# 6. NALOGA
# Uporabite ustrezne grafične prikaze, da ugotovite, ali je v model potrebno dodati
# interakcĳske člene za opisne spremenljivke. Grafikone obrazložite.

# Spol in kajenje
ggplot(train_set, aes(x = kajenje, y = log_betaplazma, color = spol, group = spol)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  theme_minimal() +
  labs(title = "Interakcija med spolom in kajenjem", y = "log(betaplazma)")



# Spol in vitamini

ggplot(train_set, aes(x = vitamini, y = log_betaplazma, color = spol, group = spol)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  theme_minimal() +
  labs(title = "Interakcija med spolom in uporabo vitaminov", y = "log(betaplazma)")


# Kajenje in vitamini

ggplot(train_set, aes(x = kajenje, y = log_betaplazma, color = vitamini, group = vitamini)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  theme_minimal() +
  labs(title = "Interakcija med kajenjem in uporabo vitaminov", y = "log(betaplazma)")

# če so nakloni različni oziroma se črte sekajo je smiselno uporabt interakcije, 
# torej bi na podlagi grafikonov use 3 interakcije dodau

#############################################################################
# 7. NALOGA
#
#V naslednjem koraku v model vključite interakcĳo med kajenje in vitamini. Kaj
#vsebinsko predstavlja ta interakcĳa? Izvedite sekvenčni F-test in obrazložite rezultat.
#Ali je interakcijski člen v modelu potreben?

model.ns5.int <- lm(log_betaplazma ~ ns(starost, df = 4) + spol + kajenje + ITM + vitamini + kalorije + mascoba + balasti + kolesterol + betadieta + kajenje:vitamini, data = train_set)

anova(model.ns5,model.ns5.int)

# V primerjavi modelov z in brez interakcijskega člena kajenje:vitamini smo dobili p = 0.03767, 
# kar pomeni, da interakcija statistično značilno izboljša razlago modela. 
# Učinek jemanja vitaminov na log_betaplazma se torej razlikuje glede na status kajenja. 
# Ker je test značilen na 5 % ravni, obdržimo interakcijski člen v končnem modelu.


#############################################################################
# 8. NALOGA
# Izvedite izbiro modela z izbiro nazaj, pri čemer z naravnimi zlepki transformirane spremenljivke starost in spremenljivke spol ne podvržete postopku izbire. Začetni model
# naj bo model, ki ste ga dobili z modeliranjem nelinearnosti in vključitvijo interakcije,
# če je bilo to potrebno. Za kriterijsko statistiko vzemite Akaikejev informacijski kriterij.
# Kako bi pojasnili izločitev spremenljivk iz modela?

scope_list <- list(
  lower = ~ ns(starost, df=4) + spol,
  upper = formula(model.ns5.int)
)

model_backward <- stepAIC(
  model.ns5.int,
  scope = scope_list,
  direction = "backward",
  trace = FALSE   # or FALSE to hide iteration messages
)

summary(model_backward)

# Glede na to da mamo interakcijo. bi blo v lower fajn dt se use ucinke znotrej interakcije.
# Začnemo s polnim modelom in iterativno poskušamo jemat iz modela prediktorje, če se AIC za dovolj zmanjša
# ta prediktor odstranimo iz modela.


#############################################################################
# 9. NALOGA
# Pojasnite, zakaj izbire modela ne moremo narediti na podlagi vsote kvadratov ostankov modela

# zato ker RSS pada z dodajanjem parametrov v model, torej nam RSS, 
# kot merilo prileganja ne pomaga najti optimalnega števila spremenljivk; 
# pokaže le, kako dobro model opiše trenutne podatke, ne pa tudi, kako se bo obnesel na drugih podatkih

# za ta namen uporabimo raje AIC, BIC ali adj. R^2


#############################################################################
# 10. NALOGA

# Interpretirajte izbrani model. Pri interpretaciji si pomagajte z grafičnimi prikazi.

model_final = model_backward
summary(model_final)

plot(allEffects(model_final))

# vidimo, da model pojasni približno 28% variabilnosti spremenljivke log_betaplazma.