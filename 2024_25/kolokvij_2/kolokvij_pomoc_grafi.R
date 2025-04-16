library(effects)
library(car)
library(gridExtra)
library(cvTools)
library(olsrr)
library(MASS)
library(lattice)
library(nlme)
library(emmeans)
library(reshape2)
library(leaps)
library(ggplot2)
library(ggpubr)

set.seed(123)  # For reproducibility

# Number of observations
n <- 500

# Create the data
df <- data.frame(
  StudyHours = round(runif(n, min = 0, max = 10), 1),     # 0 to 10 hours
  SleepHours = round(runif(n, min = 4, max = 9), 1),       # 4 to 9 hours
  Group = sample(c("Control", "TreatmentA", "TreatmentB"), n, replace = TRUE)
)

# Simulate a dependent variable with some relationship
# We'll say Score increases with StudyHours and SleepHours slightly,
# and add an effect based on group
df$Score <- with(df,
                 50 + 
                   5 * StudyHours + 
                   2 * SleepHours +
                   ifelse(Group == "TreatmentA", 10, ifelse(Group == "TreatmentB", 5, 0)) +
                   rnorm(n, mean = 0, sd = 5)  # Add some random noise
)

# Round Score for cleaner display
df$Score <- round(df$Score, 1)

# View the first few rows
head(df)



data_long = melt(df)


qplot(value, data = data_long) +
  facet_wrap(~variable, scales = "free") +
  theme_bw() +
  xlab("")



p1 <- ggplot(data = df, aes(x = StudyHours, y = Score)) +
  facet_grid(.~Group) +
  geom_point() +
  geom_smooth(se = FALSE) +
  xlab("številska_napovedna_spr1") +
  ylab("odzivna_spr")

p2 <- ggplot(data = df, aes(x = SleepHours, y = Score)) +
  facet_grid(.~Group) +
  geom_point() +
  geom_smooth(se = FALSE) +
  xlab("številska_napovedna_spr2") +
  ylab("odzivna_spr")

ggarrange(p1, p2, nrow = 2, ncol = 1)




# enostaven scatter plot z gladilnikom
ggplot(df, aes(x = StudyHours, y = Score)) + geom_point() +
  geom_smooth(method = "loess", se = FALSE)


# 1 številska napovedna spr in ena opisna napovedna spr (factor) in odzivna spr Z GLADILNIKOM
ggplot(data = df, aes(x = StudyHours, y = Score, col = Group)) +
  geom_point() + geom_smooth(se = FALSE) +
  ylab("odzivna spr (enota)") +
  xlab("Številska napovedna spr (enota)")





train <- sample(1:nrow(df), round(2/3*nrow(df)), replace=F)
train_set <- df[train, ]
test_set <- df[-train, ]


m3_1 = lm(Score~., train_set)


preds_m3_1 <- predict(m3_1, newdata = test_set)

plot(preds_m3_1, test_set$Score, main="", 
     ylim=c(4, 8), xlim=c(4, 8),
     xlab="Napovedi za odzivno_spr", ylab="Dejanske vrednosti")
abline(coef(lm(test_set$Score ~ preds_m3_1)), lty=3)
abline(0,1, lty=2, col="red")
legend("bottomright", lty=c(2,3), col=c("red", "black"), 
       legend=c("Idealni naklon", "Dejanski naklon"), bty="n")





odzivna_spr <- test_set$Score
Predicted_odzivna_spr = as.numeric(preds_m3_1)
data_plot <- data.frame(odzivna_spr, Predicted_odzivna_spr)

ggplot(data_plot, aes(x=odzivna_spr, y=odzivna_spr - Predicted_odzivna_spr)) + 
  geom_point() + 
  geom_smooth(method="loess") + 
  ylab("Ostanki")+
  xlab("Dejanske vrednosti odzivne_spr v testnem vzorcu") +
  ggtitle("Lokalna pristranskost napovedi")



ggplot(data_plot, aes(x=odzivna_spr, y=abs(odzivna_spr-Predicted_odzivna_spr))) +
  geom_point() +
  geom_smooth(method="loess") +
  ylab("Absolutni ostanki")+
  xlab("Dejanske vrednosti odzivne_spr v testnem vzorcu") +
  ggtitle("Lokalna napaka napovedi")
