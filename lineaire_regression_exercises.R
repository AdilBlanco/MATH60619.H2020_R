library(haven)

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

# (b) Calculer le coefficient de corrélation entre la taille et la masse et interprétez ce dernier.
cor(dfa$taille, dfa$masse)

# (c) Ajustez un modèle de régression linéaire pour prédire la masse en fonction de la taille.  
# Écrivez l’équation du modèle de régression ainsi que celle du modèle ajusté.
lm(masse ~ taille, data=dfa) 
# Coefficients:
# (Intercept)       taille  
#   -143.027        3.899  

# masse = β0 + β1 taille
# masse = -143.027 + 3.899 taille  
# (d) Interprétez les paramètres du modèle, β0 et β1.
# β0: La masse diminue en moyen de 143.027 lorsque la taille est égale à zero
# β1: Lorsque une augmentation de la taille d'un pouce, la masse augmente en moyen de 3.899 livres.

# (e) Quelles sont les hypothèses nulles et alternatives des tests-tde la sortie? 
# Écrivez les conclusions de ces tests.


