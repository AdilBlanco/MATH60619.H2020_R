library(haven)

#*****************
#   EXERCICE-1   #
#*****************
df1 <- haven::read_sas("./MATH60619.H2020_R/datasets/intention.sas7bdat")
head(df1, n=4)
#   fixation emotion  sexe   age revenu  educ statut intention
# 1    0.081   1.42      1    27      1     2      0        11
# 2    2.24    1.15      0    27      1     1      0        12
# 3    1.68    0.296     1    26      1     2      1         6
# 4    0.63    0.731     1    34      3     3      0         4

unique(df1$revenu)
# [1] 1 3 2
# (1) [0, 20 000]
# (2) [20 000, 60 000] 
# (3) 60 000 et plus.

levels(as.factor(df1$revenu))
df1$revenu <- factor(df1$revenu)
df1$revenu <- relevel(df1$revenu, ref="3")

levels(as.factor(df1$educ))
df1$educ <- factor(df1$educ)
df1$educ <- relevel(df1$educ, ref="3")

mod1 <- lm(intention ~ revenu, data=df1)
summary(mod1)
#Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.3095 -1.6000 -0.3095  2.4000  5.6905 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     7.1163     0.4243  16.771  < 2e-16 ***
#   revenu1       2.4837     0.6334   3.921 0.000149 ***
#   revenu2       1.1932     0.6036   1.977 0.050420 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 2.782 on 117 degrees of freedom
# Multiple R-squared:  0.1163,	Adjusted R-squared:  0.1012 
# F-statistic: 7.698 on 2 and 117 DF,  p-value: 0.0007229

# revenu1:
# En fixant toutes les autres co-variables, toutes choses étant égales,
# les gens qui ont un revenu de [0, 20 000] leur intention d'achat augmente 
# de 2.4837 en moyen par rapport aux gens qui ont un revenu de 60 000 et plus.

# revenu2:
# En fixant toutes les autres co-variables, toutes choses étant égale,
# les gens qui ont un revenu de [20 000, 60 000] leur intention d'achat augmente 
# de 1.1932 en moyen par rapport aux gens qui ont un revenu de 60 000 et plus.

par(mfrow = c(2,2))
plot(mod1)

# (b)
newdata <- data.frame(revenu=factor(c(3)))
pred <- predict(object=mod1,  newdata=newdata)
pred

# (c)
mod2 <- lm(intention ~ revenu, data=df1)
# (Intercept)  as.factor(revenu)1  as.factor(revenu)2  
#  7.116               2.484               1.193 

# (d)
mod3 <- lm(intention ~ as.numeric(revenu), data=df1)
# (Intercept)  as.numeric(revenu)  
#  7.0572              0.6031 

# (e)
mod4 <- lm(intention ~ fixation+emotion+sexe+age+revenu+educ+statut, data=df1)
summary(mod4)
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   7.75740    1.67647   4.627 1.02e-05 ***
#  fixation     1.16837    0.19464   6.003 2.54e-08 ***
#  emotion      1.09720    0.40894   2.683  0.00842 ** 
#  sexe         1.11979    0.44156   2.536  0.01262 *  
#  age         -0.12846    0.04770  -2.693  0.00818 ** 
#  revenu1      1.70178    0.61526   2.766  0.00666 ** 
#  revenu2      0.24126    0.55209   0.437  0.66298    
#  educ1        0.09265    0.61136   0.152  0.87983    
#  educ2        0.76960    0.51334   1.499  0.13669    
#  statut      -0.28669    0.43424  -0.660  0.51050    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 2.264 on 110 degrees of freedom
# Multiple R-squared:  0.4497,	Adjusted R-squared:  0.4047 
# F-statistic: 9.989 on 9 and 110 DF,  p-value: 4.337e-11

#*****************
#   EXERCICE-2   #
#*****************
# Le jeu de données automobile contient des informations sur 392 voitures. 
# On considère un modèle linéaire liant la consommation d’essence (en miles au gallon) 
# des voitures en fonction de leur puissance (en watts).

df4 <- haven::read_sas("./MATH60619.H2020_R/datasets/automobile.sas7bdat")
head(df4, n=4)

#   consommation cylindre deplacement puissance masse acceleration annee origine     nom                      
#1        18        8         307       130     3504       12       70       1   chevrolet chevelle malibu
#2        15        8         350       165     3693       11.5     70       1   buick skylark 320        
#3        18        8         318       150     3436       11       70       1   plymouth satellite       
#4        16        8         304       150     3433       12       70       1   amc rebel sst 

dim(df4)
# [1] 392   9

# a) consommation = β 0+ β1.puissance + E
par(mfrow = c(1,1)) 
plot(consommation ~ puissance, data=df4)
# Il semble y avoir une forte relation entre la variable explicative (puissance) 
# et la variable expliqué (consommation), quand la puissance augmente la consommation
# diminue.

# b) 
par(mfrow = c(2,2))
mod1 <- lm(consommation ~ puissance, data=df4)
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
df4$puissance2 <- df4$puissance^2

par(mfrow = c(2,2))
mod2 <- lm(consommation ~ puissance + puissance2, data=df4)
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
df4$puissance3 <- df4$puissance^3
par(mfrow = c(2,2))
mod3 <- lm(consommation ~ puissance + puissance2 + puissance3, data=df4)
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

#*****************
#   EXERCICE-3   #
#*****************
df3 <- cbind(df1)

levels(as.factor(df3$educ))
df3$educ <- factor(df3$educ)
df3$educ <- relevel(df3$educ, ref="3")

# (a)
# intention = β0 + β1.fixation + β2.educ1 + β3.educ2
mod3_1 <- lm(intention ~ fixation + educ, data=df3)
summary(mod3_1)$coef
#         Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) 5.533543  0.5429172 10.192242 8.301711e-18
# fixation    1.096846  0.2195556  4.995752 2.091496e-06
# educ1       1.410459  0.6512014  2.165933 3.236284e-02
# educ2       1.399775  0.5657071  2.474381 1.479345e-02

# (a-i)
# intention = 5.533543 + 1.096846.fixation + 1.410459.educ1 + 1.399775.educ2

# (a-ii)
# Si educ=1:
#************
# intention = (5.533543 + 1.410459) + 1.096846.fixation
#           = 6.944002 + 1.096846.fixation
# Si educ=2:
#************
# intention =  (5.533543 + 1.399775) + 1.096846.fixation
#           = 6.933318 + 1.096846.fixation
# Si educ=3:
#************
# intention = 5.533543 + 1.096846.fixation

# (a-iii)
plot(intention ~ fixation, 
     data=df3, 
     col=c("red","blue", "green")[educ], 
     pch=c(19,4,1)[educ])
legend(x="topleft", 
       legend=levels(df3$educ),
       col=c("red","blue", "green"),
       pch=c(19,4,1))
abline(a=6.944002, b=1.096846, col=2)
abline(a=6.933318, b=1.096846, col=3, lty=2)
abline(a=5.533543, b=1.096846, col=4, lty=3)

# (b-i)
# intention = β0 + β1.fixation + β2.educ1 + β3.educ2 + β4.fixation.educ1 + β5.fixation.educ2
mod3_2 <- lm(intention ~ fixation + educ + fixation*educ, data=df3)
summary(mod3_2)
# Coefficients:
# (Intercept)   fixation      educ1       educ2   fixation:educ1  fixation:educ2  
# 4.5860        1.7543        1.4484      3.3196         -0.1101         -1.2581

# intention = 4.5860 + 1.7543.fixation + 1.4484.educ1 + 3.3196.educ2 - 0.1101.fixation:educ1 - 1.2581.fixation:educ2
anova(mod3_2)
# Analysis of Variance Table
# Response: intention
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#  fixation        1 186.23 186.234 28.7553 4.35e-07 ***
#  educ            2  48.52  24.261  3.7461  0.02657 *  
#  fixation:educ   2  51.91  25.956  4.0078  0.02079 *  
#  Residuals     114 738.32   6.477                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# L’interaction est significative à niveau 5% (valeur-p de 0.02) ;
# le modèle avec interaction est plus adéquat que le modèle sans interaction.

# (a-ii)
# Si educ=1:
#************
# intention = (4.5860 + 1.4484) + (1.7543 - 0.1101).fixation
#           = 6.0344 + 1.6442.fixation
# Si educ=2:
#************
# intention = 4.5860 + 3.3196 + (1.7543 - 1.2581).fixation
#           = 7.9056 + 0.4962.fixation
# Si educ=3:
#************
# intention = 4.5860 + 1.7543.fixation

# (a-iii)
plot(intention ~ fixation, 
     data=df3, 
     col=c("red","blue", "green")[educ], 
     pch=c(19,4,1)[educ])
legend(x="topleft", 
       legend=levels(df3$educ),
       col=c("red","blue", "green"),
       pch=c(19,4,1))
abline(a=6.0344, b=1.6442, col=2)
abline(a=7.9056, b=0.4962, col=3, lty=2)
abline(a=4.5860, b=1.7543, col=4, lty=3)





























