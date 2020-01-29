library(haven)
library(ggpubr)

df <- read_sas("./MATH60619.H2020_R/datasets/intention.sas7bdat")
head(df)

summary(df)

# *************************************************
# interpretation du resultats (Variable continue):#
# *************************************************
# intention = β0 + β1 fixation
# β1=1.14: Pour chaque seconde additionnelle de fixation, le score 
#          intention augmente en moyenne de 1.14 points.
# β0=6.453: Le score moyen d'intention d'achat lorsque les autres 
#           sujets sont egales a zero (personne n'a fixer la pub).
lm(intention ~ fixation, data=df)
# Coefficients:
# (Intercept)     fixation  
#     6.453        1.144  

# *************************************************
# interpretation du resultats (Variable binaire): #
# *************************************************
# 0 (homme)/ 1 (femme)
# 0 (celibataire) / 1 (en couple)
# intention = β0 + β1 sexe
# β1=1.368: Soit une augmentation de l'intention moyenne d'achat
#           de 1.368 points pour les femmes par rapport a la 
#           moyenne des hommes.
# 7.55: La moyenne d'intention d'achat des hommes est de 7.552 points.
# 1.36: La moyenne d'intention d'achat des femmes est de 8.92=(7.55+1.36) points.
lm(intention ~ sexe, data=df)
# Coefficients:
#  (Intercept)        sexe  
#       7.552        1.368 

# ******************************************************
# interpretation du resultats (Variable categorielle): #
# ******************************************************
# intention = β0 + β1 educ
# E (intention | educ=1) = β0 + β1, 
# E (intention | educ=2) = β0 + β2, 
# E (intention | educ=3) = β0.

# L'intention moyen d'achat de chacun des niveaux d'education est 
# 8.77 (1.652+7.114), 8.71 (1.595+7.114) et 7.114 pour les niveaux d'educ
# 1, 2 et 3 respectivement.
# 1) L'intention moyenne d'achat est 1.652 points plus elevee si educ=1 
# que si educ=3.
# 2) L'intention moyenne d'achat est 1.595 points plus elevee si educ=2
# que si educ=3
# Remarque: Pour comparer educ=1 et educ=2, on pourrait reajuster le mdele
#           en changeant la categorie de reference.
levels(as.factor(df$educ))
df$educ <- factor(df$educ)
df$educ <- relevel(df$educ, ref="3" )

lm(intention ~ educ, data=df)
# Coefficients:
# (Intercept)        educ1        educ2  
#       7.114        1.652        1.595

df <- read_sas("./MATH60619.H2020_R/datasets/intention.sas7bdat")
head(df)

lm(intention ~ 
     fixation + 
     emotion +
     age +
     sexe +
     statut, data=df)
# Coefficients:
#   (Intercept)     fixation      emotion         age         sexe       statut  
#     10.2011       1.1152         1.3228      -0.1843       1.2112      -0.2996  

# Fixaxtion: En fixant toutes les autres covariables, toutes choses etant egales, 
#            l'augmentation de fixation d'une secode vas augmenter l'intention d'achat 
#            en moyen de 1.1152. 

# Age: En fixant toutes les autres covariables, toutes choses etant egales, l'augmentation
#      d'une annee vas diminuer en moyenne l'intention d'achat en moyen de 0.1843.

# Sexe (binaire): En fixant toutes les autres covariables, toutes choses etant egales, 
#                 l'intention d'achat moyen chez le femmes augmente de 1.2112 par rapport 
#                 a la moeyenne des hommes.
#                 (la difference de l'effet moyen entre la 2ieme categorie et la 
#                   categorie de reference)

# Statut (binaire): En fixant toutes les autres covariables, toutes choses etant egales, 
#                   l'intention d'achat moyen chez les couples diminue de 0.2996 par 
#                   rapport a la moyenne d'achat chez les  celibataire.






