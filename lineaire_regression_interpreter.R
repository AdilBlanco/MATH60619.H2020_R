library(haven)
library(ggpubr)

df <- read_sas("./MATH60619.H2020_R/datasets/intention.sas7bdat")
head(df, n=4)
# fixation emotion  sexe   age revenu  educ statut intention
#  0.081   1.42      1    27      1     2      0        11
#  2.24    1.15      0    27      1     1      0        12
#  1.68    0.296     1    26      1     2      1         6
#  0.63    0.731     1    34      3     3      0         4

dim(df)
# [1] 120   8

summary(df)
#     fixation        emotion            sexe             age            revenu           educ      
# Min.   :0.028   Min.   :0.0530   Min.   :0.0000   Min.   :19.00   Min.   :1.000   Min.   :1.000  
# 1st Qu.:0.836   1st Qu.:0.7175   1st Qu.:0.0000   1st Qu.:27.00   1st Qu.:1.000   1st Qu.:1.750  
# Median :1.307   Median :0.9260   Median :1.0000   Median :30.00   Median :2.000   Median :2.000  
# Mean   :1.578   Mean   :1.0380   Mean   :0.5167   Mean   :30.06   Mean   :2.067   Mean   :2.042  
# 3rd Qu.:2.066   3rd Qu.:1.3790   3rd Qu.:1.0000   3rd Qu.:33.25   3rd Qu.:3.000   3rd Qu.:3.000  
# Max.   :5.835   Max.   :2.7970   Max.   :1.0000   Max.   :45.00   Max.   :3.000   Max.   :3.000  
# statut         intention     
# Min.   :0.0000   Min.   : 2.000  
# 1st Qu.:0.0000   1st Qu.: 6.000  
# Median :1.0000   Median : 8.000  
# Mean   :0.5417   Mean   : 8.258  
# 3rd Qu.:1.0000   3rd Qu.:11.000  
# Max.   :1.0000   Max.   :14.000  

# *************************************************
# Interprétation du résultats (variable continue):#
# *************************************************
# intention = β0 + β1 fixation
# β1=1.14: Pour chaque seconde additionnelle de fixation, le score 
#          intention augmente en moyenne de 1.14 points.
# β0=6.453: Le score moyen d'intention d'achat lorsque les autres 
#           sujets sont égaux à zero (personne n'a fixé la pub).
lm(intention ~ fixation, data=df)
# Coefficients:
# (Intercept)     fixation  
#     6.453        1.144  

# *************************************************
# Interprétation du résultats (variable binaire): #
# *************************************************
# 0 (homme)/ 1 (femme)
# 0 (célibataire) / 1 (en couple)
# intention = β0 + β1 sexe
# β1=1.368: Soit une augmentation de l'intention moyenne d'achat
#           de 1.368 points pour les femmes par rapport à la 
#           moyenne des hommes.
# 7.55: La moyenne d'intention d'achat des hommes est de 7.552 points.
# 1.36: La moyenne d'intention d'achat des femmes est de 8.92=(7.55+1.36) points.
lm(intention ~ sexe, data=df)
# Coefficients:
#  (Intercept)        sexe  
#       7.552        1.368 

# ******************************************************
# Interprétation du résultats (variable catégorielle): #
# ******************************************************
# intention = β0 + β1 educ
# E (intention | educ=1) = β0 + β1, 
# E (intention | educ=2) = β0 + β2, 
# E (intention | educ=3) = β0.

# L'intention moyenne d'achat de chacun des niveaux d'éducation est 
# 8.77 (1.652+7.114), 8.71 (1.595+7.114) et 7.114 pour les niveaux d'educ
# 1, 2 et 3 respectivement.
# 1) L'intention moyenne d'achat est 1.652 points plus élevés si educ=1 
# que si educ=3.
# 2) L'intention moyenne d'achat est 1.595 points plus élevés si educ=2
# que si educ=3
# Remarque: Pour comparer educ=1 et educ=2, on pourrait réajuster le mdele
#           en changeant la catégorie de référence.
levels(as.factor(df$educ))
df$educ <- factor(df$educ)
df$educ <- relevel(df$educ, ref="3" )

lm(intention ~ educ, data=df)
# Coefficients:
# (Intercept)        educ1        educ2  
#       7.114        1.652        1.595

df <- read_sas("./MATH60619.H2020_R/datasets/intention.sas7bdat")
head(df, n=4)

lm(intention ~ 
     fixation + 
     emotion +
     age +
     sexe +
     statut, data=df)
# Coefficients:
#   (Intercept)     fixation      emotion         age         sexe       statut  
#     10.2011       1.1152         1.3228      -0.1843       1.2112      -0.2996  

# Fixaxtion: En fixant toutes les autres co-variables, toutes choses étant égales, 
#            l'augmentation de fixation d'une secode vas augmenter en moyen l'intention d'achat 
#            de 1.1152. 

# Age: En fixant toutes les autres co-variables, toutes choses étant égales, l'augmentation
#      d'une année vas diminuer en moyen l'intention d'achat de 0.1843.

# Sexe (binaire): En fixant toutes les autres co-variables, toutes choses étant égales, 
#                 l'intention d'achat moyen chez le femmes augmente de 1.2112 par rapport 
#                 à la moeyenne des hommes.
#                 (la différence de l'effet moyen entre la 2ieme catégorie et la 
#                   catégorie de réfèrence)

# Statut (binaire): En fixant toutes les autres co-variables, toutes choses étant égales, 
#                   l'intention d'achat moyen chez les couples diminue de 0.2996 par 
#                   rapport a la moyenne d'achat chez les célibataire.

#*************************************************************************
# Corrélation r: qunatifie la force de la relation linéaire entre X et Y #
#*************************************************************************
cor(df$intention, df$fixation)
# [1] 0.4262547
# La corrélation linéaire de Pearson entre intention et fixation est 0.4262547.
cor(df$intention, df$emotion)
# [1] 0.2347929
# La corrélation linéaire de Pearson entre intention et emotion est 0.2347929.
par(mfrow=c(1,2), pch=20, bty='l')
plot(intention ~ fixation, data=df, col="red")
plot(intention ~ emotion, data=df, col="green")

#**************************************************************************
# Coefficient de détermnation R^2:                                        #
# + mesure la force de la relation linéaire entre fit(Y) et Y             #
# + représente la proportion de la variabilité de Y par les regresseurs X #
#**************************************************************************
lmod <- lm(intention ~ fixation + emotion, data=df)
summary(lmod)
# Residuals:
#        Min      1Q      Median      3Q     Max 
#        -5.7033 -1.7601 -0.4144  2.1520  6.8140 
# Coefficients:
#            Estimate Std. Error   t value  Pr(>|t|)    
# (Intercept)   5.5185     0.5990   9.212  1.56e-15 ***
# fixation      1.0797     0.2219   4.865  3.60e-06 ***
# emotion       0.9984     0.4544   2.197     0.03 *  
#        ---
#        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.624 on 117 degrees of freedom
#Multiple R-squared:  0.2141,	Adjusted R-squared:  0.2007 
#F-statistic: 15.94 on 2 and 117 DF,  p-value: 7.559e-07

# Les variables fixation et emotion expliquent plus que 20% de la varibilité d'intention. 

# ****************************************************
# Interprétation du test d'hypothèse (un paramètre): #
# ****************************************************
lmod <- lm(intention ~ fixation, data=df)
summary(lmod)
# Residuals:
#        Min     1Q  Median     3Q    Max 
#     -5.813 -1.828  -0.207  2.176  6.130 

# Coefficients:
#               Estimate Std.  Error   t value    Pr(>|t|)    
#    (Intercept)   6.4532      0.4285   15.060   < 2e-16 ***
#      fixation    1.1441      0.2235    5.119   1.21e-06 ***
#        ---
#        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 2.666 on 118 degrees of freedom
# Multiple R-squared:  0.1817,	Adjusted R-squared:  0.1748 
# F-statistic:  26.2 on 1 and 118 DF,  p-value: 1.209e-06

confint(lmod, level=0.95)
#                 2.5 %    97.5 %
# (Intercept) 5.6046573  7.301720
# fixation    0.7014641  1.586703

# + La valeur de la statistique-t pour H0: β1=0 versus H1: β1<>0 est t=1.1441/0.2235=5.119.
# + La valeur-p du test bilatéral est inférieur à 0.05 (1.21e-06 < 0.05).
# + On rejette H0, car il y a un effet significatif de temps de fixation sur l'intention d'achat.
# + L'intervalle de confiance à 95% pour β1, correspondant à l'effet linéaire de fixation est: [0.7014641, 1.586703].  
# Conclusion: puisque l'intervalle ne contient pas 0, on déduit que le paramètre est significativement
#             différent de zéro à niveau α=5%.

# ************************************************************
# Interprétation du test d'hypothèse (plusieurs paramètres): #
# ************************************************************


