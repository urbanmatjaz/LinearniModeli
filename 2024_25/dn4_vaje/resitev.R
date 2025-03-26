library(ggplot2)
library(ggpubr)
library(car)
library(effects)
library(dplyr)
library(ISLR2)
library(splines)

ggplot(data=Wage, aes(x=age, y=wage, col=education)) +
  geom_point() + 
  geom_smooth(se=FALSE) +
  #geom_smooth(method="lm", se=FALSE) +
  xlab("Starost (leta)") +
  ylab("Plača (1000 $)") 

ggplot(data=Wage, aes(x=age, y=log(wage), col=education)) +
  geom_point() + 
  geom_smooth(se=FALSE) +
  #geom_smooth(method="lm", se=FALSE) +
  xlab("Starost (leta)") +
  ylab("log(Plača (1000 $))") 

ggplot(data=Wage, aes(x=age, y=log(wage))) +
  geom_point() + 
  geom_smooth(se=FALSE) +
  #geom_smooth(method="lm", se=FALSE) +
  xlab("Starost (leta)") +
  ylab("log(Plača (1000 $))") +
  facet_grid(.~education)


m_0 <- lm(log(wage)~age*education, data=Wage)

m_p2 <- lm(log(wage)~poly(age,2)*education, data=Wage)
m_p3 <- lm(log(wage)~poly(age,3)*education, data=Wage)
m_p4 <- lm(log(wage)~poly(age,4)*education, data=Wage)
anova(m_p2, m_p3, m_p4)
anova(m_0, m_p4)

m_s3 <- lm(log(wage)~ns(age,df=3)*education, data=Wage)
m_s4 <- lm(log(wage)~ns(age,df=4)*education, data=Wage)
m_s5 <- lm(log(wage)~ns(age,df=5)*education, data=Wage)
m_s6 <- lm(log(wage)~ns(age,df=6)*education, data=Wage)
AIC(m_0, m_s3,m_s4,m_s5,m_s6)

m_s4_2 <- lm(log(wage)~ns(age,knots=c(25, 40, 60))*education, data=Wage)
AIC(m_s4, m_s4_2)

par(mfrow=c(2,2))
plot(m_s4_2)

plot(Effect(c("age", "education"), m_s4_2, partial.residuals=TRUE))

plot(Effect(c("age", "education"), m_s4_2), multiline=TRUE, ci.style = "bands")