library(haven)

#*****************
#   EXERCICE-1   #
#*****************

#*****************
#   EXERCICE-2   #
#*****************
# Le jeu de données automobile contient des informations sur 392 voitures. 
# On considère un modèle linéaire liant la consommation d’essence (en miles au gallon) 
# des voitures en fonction de leur puissance (en watts).

df <- read_sas("./MATH60619.H2020_R/datasets/automobile.sas7bdat")
head(df, n=4)

#   consommation cylindre deplacement puissance masse acceleration annee origine     nom                      
#1        18        8         307       130     3504       12       70       1   chevrolet chevelle malibu
#2        15        8         350       165     3693       11.5     70       1   buick skylark 320        
#3        18        8         318       150     3436       11       70       1   plymouth satellite       
#4        16        8         304       150     3433       12       70       1   amc rebel sst 

dim(df)
# [1] 392   9

# a) consommation = β 0+ β1.puissance + E
par(mfrow = c(1,1)) 
plot(consommation ~ puissance, data=df)
# Il semble y avoir une forte relation entre la variable explicative (puissance) 
# et la variable expliqué (consommation), quand la puissance augmente la consommation
# diminue.

# b) 
par(mfrow = c(2,2))
mod1 <- lm(consommation ~ puissance, data=df)
abline(mod1, col="red")
plot(mod1)

summary(mod1)
# Residuals:
#  Min       1Q   Median       3Q      Max 
# -13.5710  -3.2592  -0.3435   2.7630  16.9240 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 39.935861   0.717499   55.66   <2e-16 ***
#   puissance   -0.157845   0.006446  -24.49   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 4.906 on 390 degrees of freedom
# Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6049 
# F-statistic: 599.7 on 1 and 390 DF,  p-value: < 2.2e-16

# R^2 = 0.6059 cela veut dire que la variable explicative (puissance) 
# explique 60% de la variablité de consommation.

# c) consommation = β0 + β1.puissance + β2.puissance2 + E
df$puissance2 <- df$puissance^2

par(mfrow = c(2,2))
mod2 <- lm(consommation ~ puissance + puissance2, data=df)
plot(mod2)

summary(mod2)
# Residuals:
#    Min       1Q   Median       3Q      Max 
# -14.7135  -2.5943  -0.0859   2.2868  15.8961 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 56.9000997  1.8004268   31.60   <2e-16 ***
#   puissance   -0.4661896  0.0311246  -14.98   <2e-16 ***
#   puissance2   0.0012305  0.0001221   10.08   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 4.374 on 389 degrees of freedom
# Multiple R-squared:  0.6876,	Adjusted R-squared:  0.686 
# F-statistic:   428 on 2 and 389 DF,  p-value: < 2.2e-16

# d) consommation = β0 + β1.puissance + β2.puissance2 + β3.puissance3 + E
df$puissance3 <- df$puissance^3
par(mfrow = c(2,2))
mod3 <- lm(consommation ~ puissance + puissance2 + puissance3, data=df)
plot(mod3)
summary(mod3)
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -14.7039  -2.4491  -0.1519   2.2035  15.8159 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  6.068e+01  4.563e+00  13.298  < 2e-16 ***
#   puissance   -5.689e-01  1.179e-01  -4.824 2.03e-06 ***
#   puissance2   2.079e-03  9.479e-04   2.193   0.0289 *  
#   puissance3  -2.147e-06  2.378e-06  -0.903   0.3673    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 4.375 on 388 degrees of freedom
# Multiple R-squared:  0.6882,	Adjusted R-squared:  0.6858 
# F-statistic: 285.5 on 3 and 388 DF,  p-value: < 2.2e-16



































