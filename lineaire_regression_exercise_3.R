library(haven)

#*****************
#   EXERCICE-1   #
#*****************

df <- read_sas("./MATH60619.H2020_R/datasets/croissance.sas7bdat")
head(df, n=4)
# # A tibble: 4 x 5
#     nom     taille masse   age  sexe 
# 1 Alfred    69     112.    14   homme
# 2 Alice     56.5    84     13   femme
# 3 Barbara   65.3    98     13   femme
# 4 Carol     62.8   102.    14   femme

dim(df)
# [1] 19  5

# 0 (homme)/ 1 (femme)
df$sexe <- factor(df$sexe, labels = c("homme", "femme"))

# (a) Calculer les statistiques descriptives de l’échantillon (moyenne, écart-type, quartiles, etc.) 
# des deux variableset inspecter visuellement l’effet de la taille sur la masse à l’aide d’un nuage 
# de points.
dfa <- df[, c("taille", "masse")]
head(dfa, n=4)
summary(dfa)
#         taille          masse       
# Min.   :51.30   Min.   : 50.50  
# 1st Qu.:58.25   1st Qu.: 84.25  
# Median :62.80   Median : 99.50  
# Mean   :62.34   Mean   :100.03  
# 3rd Qu.:65.90   3rd Qu.:112.25  
# Max.   :72.00   Max.   :150.00

par(mfrow=c(1,1), pch=20, bty='l')
plot(masse ~ taille, data=dfa, col="red")
abline(lm(masse ~ taille, data=dfa), col="green")
# l  y  a  apparence  d’une  relation  linéaire  forte  et  positive  entre  les  variables  
# masse  et  taille  (à  mesure  quel’adolescent grandit, sa masse augmente).

# (b) Calculer le coefficient de corrélation entre la taille et la masse et interprétez ce dernier.
cor(dfa$taille, dfa$masse)
# Le coefficient de corrélation linéaire estimé est r=0.88, ce qui confirme la présence 
# d’une relation linéaire forte entre les variables.

# (c) Ajustez un modèle de régression linéaire pour prédire la masse en fonction de la taille.  
# Écrivez l’équation du modèle de régression ainsi que celle du modèle ajusté.
lm(masse ~ taille, data=dfa) 
# Coefficients:
# (Intercept)       taille  
#   -143.027        3.899  

# masse = β0 + β1 taille + ε
# masse = -143.027 + 3.899 taille  

# (d) Interprétez les paramètres du modèle, β0 et β1.
# β0: N’a pas d’interprétation, puisque qu’un humain ne peut pas avoir une taille nulle.
# β1: Lorsque une augmentation de la taille d'un pouce, la masse augmente en moyen de 3.899 livres.

# (e) Quelles sont les hypothèses nulles et alternatives des tests-t de la sortie? 
# Écrivez les conclusions de ces tests.
# H0: β1=0
# H1: β1#0
# On rejete H0, donc il y a un effet significatif de la taille sur la masse.
limod <- lm(masse ~ taille, data=dfa) 
summary(limod)
# Residuals:
#    Min       1Q     Median    3Q      Max 
# -17.6807  -6.0642   0.5115   9.2846  18.3698 
# Coefficients:
#            Estimate Std.  Error t   value    Pr(>|t|)    
#(Intercept)   -143.0269    32.2746  -4.432   0.000366 ***
#  taille         3.8990    0.5161    7.555   7.89e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 11.23 on 17 degrees of freedom
# Multiple R-squared:  0.7705,	Adjusted R-squared:  0.757 
# F-statistic: 57.08 on 1 and 17 DF,  p-value: 7.887e-07

# (f) Donne un intervalle à 95% pour la pente et interprétez ce dernier.
confint(limod, level=0.95)
#                   2.5 %     97.5 %
# (Intercept) -211.120354  -74.933483
# taille         2.810167    4.987893

# Puisque zéro n’est pas inclus dans l’intervalle, on conclut que l’effet linéaire de 
# la taille sur la masse est significativement différent de zéro. 95% des intervalles 
# ainsi construits devraient contenir le vrai effet moyen de la taille sur lamasse.

# (g) Prédisez la masse de deux adolescents hypothétiques de 70,5 et 71,5 pouces.   
# Que représente la différenceentre ces valeurs?
newdata <- data.frame(taille=seq(70.5, 71.5))
pred <- predict(object=limod,  newdata=newdata)
pred
#   1          2 
# 131.8547   135.7537  

# La différence entre les deux prédictions est 135.7537 − 131.8547=3.899 livres, soit la valeur de β1.
# C’est l'effet de la taille sur la masse: en moyenne, la masse augmente de 3,899 livres pour chaque 
# pouce supplémentaire.

# h)  Fournissez des intervalles de confiance pour la masse moyenne et la masse prédite pour 
# deux individus detaille 70,5 et 71,5 pouces. Commentez sur les différences entre ces deux intervalles.
pred <- predict(object=limod,  newdata=newdata, interval="conf")
pred
#     fit       lwr       upr
# 1 131.8547  121.4368  142.2726
# 2 135.7537  124.3926  147.1149

# L’intervalle de confiance à 95% pour la masse moyenne d’un individu de 70,5 pouces est 
# [121.4368; 142.2726] etde [124.3926; 147.1149] pour 71,5 pouces. 

# (i) Supposez qu’on ajuste un modèle de régression pour la masse (en kg) et la taille (en cm).  
# Est-ce que le mod-èle linéaire serait toujours adéquat?  Expliquez en quoi le changement d’unité 
# affecterait les estimés de vosparamètres.

#*****************
#   EXERCICE-2   #
#*****************

df <- read_sas("./MATH60619.H2020_R/datasets/eolienne.sas7bdat")
head(df, n=4)
#      vitesse  production
# 1    2.45      0.123
# 2    2.7       0.5  
# 3    2.9       0.653
# 4    3.05      0.558

dim(df)
# [1] 25  2

# (a) Ajustez un modèle linéaire avec la vitesse du vent comme covariable et produisez 
# un graphique des résidus contre les valeurs ajustées. Est-ce que vous remarquez une structure 
# résiduelle qui n’est pas prise en compte dans votre modèle? Essayez aussi un modèle avec la 
# réciproque de la vitesse du vent comme variable explicative. Commentez sur l’adéquation 
# des deux modèles.
coef(lm(production ~ vitesse, data=df))
# (Intercept)     vitesse 
#  0.1308751    0.2411489 

eomod <- lm(production ~ vitesse, data=df)
summary(eomod)
# Residuals:
#  Min       1Q      Median    3Q      Max 
# -0.59869 -0.14099  0.06059  0.17262  0.32184 

# Coefficients:
#            Estimate Std. Error     t value   Pr(>|t|)    
# (Intercept)  0.13088     0.12599   1.039     0.31    
# vitesse      0.24115     0.01905   12.659    7.55e-12 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.2361 on 23 degrees of freedom
# Multiple R-squared:  0.8745,	Adjusted R-squared:  0.869 
# F-statistic: 160.3 on 1 and 23 DF,  p-value: 7.546e-12

par(mfrow = c(2,2)) 
plot(eomod)
# la présence de structure résiduelle dans les données (quadratique) 
# indiquent que le modèle pour la moyenne est mal spécifié.









