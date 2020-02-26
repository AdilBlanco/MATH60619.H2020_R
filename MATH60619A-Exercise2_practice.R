#***************
# Exercice_2-1 #
#***************

tickets <- haven::read_sas("./MATH60619.H2020_R/datasets/billets.sas7bdat")
head(tickets)
dim(tickets)
summary(tickets)

# H0: μ_cash = μ_credit
# Ha: μ_cash < μ_credit
# ou μ_cash est la moyenne des individus payant comptant dans la population
# et μ_credit est la moyenne des individus payant avec carte de crédit dans la population

t.test(tickets$offre ~ tickets$groupe, alternative="less")
# 	Welch Two Sample t-test
# data:  tickets$offre by tickets$groupe
# t = -3.3887, df = 35.72, p-value = 0.0008623
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf -7.528621
# sample estimates:
#   mean in group 0 mean in group 1 
#          56.60606        71.61290 

# La valeur-p provenant du test-t est de 0.0008623 < 0.05, donc on rejette H0 et on conclut
# que les individus ayant carte de crédit payent plus chère que les individus ayant comptant.

#***************
# Exercice_2-2 #
#***************
tickets_extrem <- cbind(tickets)
tickets_extrem[1, ]$offre <- 210

head(tickets_extrem, n=2)
#   offre groupe
# 1   210      0
# 2    44      0

# a)
t.test(tickets$offre ~ tickets$groupe, alternative="two.sided")
# 	Welch Two Sample t-test
# data:  tickets$offre by tickets$groupe
# t = -3.3887, df = 35.72, p-value = 0.001725
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -23.990758  -6.022927
# sample estimates:
#   mean in group 0 mean in group 1 
#          56.60606        71.61290 
t.test(tickets_extrem$offre ~ tickets_extrem$groupe, alternative="two.sided")
# 	Welch Two Sample t-test
# data:  tickets_extrem$offre by tickets_extrem$groupe
# t = -1.6385, df = 61.388, p-value = 0.1064
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -23.361250   2.317261
# sample estimates:
#   mean in group 0 mean in group 1 
#          61.09091        71.61290 

# la valeur-p provenant du premier test-t avec les données qui ne contienent pas de valeurs 
# abérantes est de 0.001725 < 0.05, donc on rejette H0, alors que la valeur-p provenant du deuxième 
# test-t qui contienent des valeurs abérantes est de 0.1064 > 0.05, donc on ne rejette pas H0.

# b)
par(mfrow=c(2,1))
boxplot(tickets$offre ~ tickets$groupe, 
        horizontal=TRUE, 
        col="lightblue", 
        xlab="offre", 
        ylab="groupe",
        main="Sans valeurs abérantes",
        frame=FALSE)
boxplot(tickets_extrem$offre ~ tickets_extrem$groupe, 
        horizontal=TRUE, 
        col="lightblue", 
        xlab="offre", 
        ylab="groupe",
        main="Présence des valeurs abérantes",
        frame=FALSE)

# c)
wilcox.test(tickets$offre ~ tickets$groupe, alternative="two.sided")
# 	Wilcoxon rank sum test with continuity correction
# data:  tickets$offre by tickets$groupe
# W = 288.5, p-value = 0.002743
# alternative hypothesis: true location shift is not equal to 0
wilcox.test(tickets_extrem$offre ~ tickets_extrem$groupe, alternative="two.sided")
# 	Wilcoxon rank sum test with continuity correction
# data:  tickets_extrem$offre by tickets_extrem$groupe
# W = 308, p-value = 0.0063
# alternative hypothesis: true location shift is not equal to 0

# la valeur-p provenant du premier test Wilcoxon avec les données qui ne contienent pas de valeurs 
# abérantes est de 0.002743 < 0.05, donc on rejette H0, alors que la valeur-p provenant du deuxième 
# test Wilcoxon qui contienent des valeurs abérantes est de 0.0063 < 0.05, donc on rejette H0.

#*************************
# Diagnostique des tests #
#*************************
# Normalité (sans valeurs abérantes)

par(mfrow=c(3,2), pch=20, bty='l')
# histogram plot offers for group 0.
hist(tickets$offre[tickets$groupe==0], freq=FALSE, col="lightblue",
     xlab="offer", main="Individuals who pay cash", ylab="density")
lines(density(tickets$offre[tickets$groupe==0]), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(tickets$offre[tickets$groupe==0]), 
                  sd=sd(tickets$offre[tickets$groupe==0])), from=20, to=200, add=TRUE), col="red", lwd=2)
# histogram plot offers for group 1.
hist(tickets$offre[tickets$groupe==1], freq=FALSE, col="lightblue",
     xlab="offer", main="Individuals who pay with credit card", ylab="density")
lines(density(tickets$offre[tickets$groupe==1]), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(tickets$offre[tickets$groupe==1]), 
                  sd=sd(tickets$offre[tickets$groupe==1])), from=20, to=200, add=TRUE), col="red", lwd=2)
# Normal Qiantile-Q plot offers for group 0.
qqnorm(tickets$offre[tickets$groupe==0], pch=1, main="Individuals pay cash", frame=FALSE)
qqline(tickets$offre[tickets$groupe==0], col="steelblue", lwd=2)
# Normal Qiantile-Q plot offers for group 1.
qqnorm(tickets$offre[tickets$groupe==1], pch=1, main="Individuals pay with credit card", frame=FALSE)
qqline(tickets$offre[tickets$groupe==1], col="steelblue", lwd=2)
# Box plot offer
boxplot(tickets$offre ~ tickets$groupe, 
        horizontal=TRUE, 
        col="lightblue", 
        xlab="offer", 
        ylab="group",
        main="Offers per group",
        frame=FALSE)

# Normalité (avec valeurs abérantes)

par(mfrow=c(3,2), pch=20, bty='l')
# histogram plot offers for group 0.
hist(tickets_extrem$offre[tickets_extrem$groupe==0], freq=FALSE, col="lightblue",
     xlab="offer", main="Individuals who pay cash", ylab="density")
lines(density(tickets_extrem$offre[tickets_extrem$groupe==0]), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(tickets_extrem$offre[tickets_extrem$groupe==0]), 
                  sd=sd(tickets_extrem$offre[tickets_extrem$groupe==0])), from=20, to=200, add=TRUE), col="red", lwd=2)
# histogram plot offers for group 1.
hist(tickets_extrem$offre[tickets_extrem$groupe==1], freq=FALSE, col="lightblue",
     xlab="offer", main="Individuals who pay with credit card", ylab="density")
lines(density(tickets_extrem$offre[tickets_extrem$groupe==1]), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(tickets_extrem$offre[tickets_extrem$groupe==1]), 
                  sd=sd(tickets_extrem$offre[tickets_extrem$groupe==1])), from=20, to=200, add=TRUE), col="red", lwd=2)
# Normal Quantile-Q plot offers for group 0.
qqnorm(tickets_extrem$offre[tickets_extrem$groupe==0], pch=1, main="Individuals pay cash", frame=FALSE)
qqline(tickets_extrem$offre[tickets_extrem$groupe==0], col="steelblue", lwd=2)
# Normal Quantile-Q plot offers for group 1.
qqnorm(tickets_extrem$offre[tickets_extrem$groupe==1], pch=1, main="Individuals pay with credit card", frame=FALSE)
qqline(tickets_extrem$offre[tickets_extrem$groupe==1], col="steelblue", lwd=2)
# Box plot offer
boxplot(tickets_extrem$offre ~ tickets_extrem$groupe, 
        horizontal=TRUE, 
        col="lightblue", 
        xlab="offer", 
        ylab="group",
        main="Offers per group",
        frame=FALSE)

# Homoscédasticité (valeurs abérantes)
var.test(tickets_extrem$offre ~ tickets_extrem$groupe)
# 	F test to compare two variances
# data:  tickets_extrem$offre by tickets_extrem$groupe
# F = 1.3884, num df = 32, denom df = 30, p-value = 0.369
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.6746542 2.8335643
# sample estimates:
#   ratio of variances 
#        1.388433 

# On ne rejette pas H0, donc les variances des deux groupes sont significativement différentes.
# Donc le test le test de Fligner-Policello sera approprié.

fp.test(tickets_extrem$offre, tickets_extrem$groupe, alternative="two.sided")
# 	Fligner-Policello test
# data:  tickets_extrem$offre and tickets_extrem$groupe
# U* = -Inf, p-value < 2.2e-16
# alternative hypothesis: true difference in location is not equal to 0

#***************
# Exercice_2-3 #
#***************

food <- haven::read_sas("./MATH60619.H2020_R/datasets/nourriture.sas7bdat")
head(food, n=2)
#   balance couleur attraction desir plaisir
# 1       0       0        -15    92      -6
# 2       0       0        -23    96      42

dim(food)
# [1] 68  5

summary(food)
#     balance       couleur      attraction         desir          plaisir       
# Min.   :0.0   Min.   :0.0   Min.   :-67.00   Min.   : 0.00   Min.   :-98.000  
# 1st Qu.:0.0   1st Qu.:0.0   1st Qu.: -5.00   1st Qu.:35.00   1st Qu.:-38.250  
# Median :0.5   Median :0.5   Median : 29.50   Median :58.50   Median :  7.500  
# Mean   :0.5   Mean   :0.5   Mean   : 24.29   Mean   :55.66   Mean   :  6.853  
# 3rd Qu.:1.0   3rd Qu.:1.0   3rd Qu.: 50.25   3rd Qu.:76.25   3rd Qu.: 44.000  
# Max.   :1.0   Max.   :1.0   Max.   :100.00   Max.   :99.00   Max.   : 99.000

# l'étude est porter sur la symétrie et couleur du plat sur l’intention de goûter
# + *balance*: symétrie du plat, soit symétrique (1) ou non symétrique (2).
# + *couleur*: couleur du plat, soit monochrome (0), soit coloré (1).
# + *attraction*: score d’attraction entre -100 et 100 (les valeurs négatives 
#   indiquant une répulsion, les valeurs positives indiquant une attraction).
# + *desir*: score relié au désir de goûter le plat entre -100 et 100 (les valeurs négatives
#   indiquant une aversion,les valeurs positives indiquant un désir)
# + *plaisir*: score relié au plaisir que le sujet a eu à goûter le plat entre -100 et 100 
#   (les valeurs négatives indiquant un désagrément, les valeurs positives indiquant un plaisir)

# a)
mean(food$desir[food$couleur==0])
# [1] 61.73529
mean(food$desir[food$couleur==1])
# [1] 49.58824
# Les moyennes sont différente dans les deux groupes.

# b)
# H0: μ_m = μ_c
# Ha: μ_m <> μ_c
# ou μ_m est la moyenne des plats monochrome dans la population
# et μ_c est la moyenne des plats coloré dans la population

# c, d, e et f
par(mfrow=c(3,2), pch=20, bty='l')
# histogram plot desirs for group 0.
hist(food$desir[food$couleur==0], freq=FALSE, col="lightblue",
     xlab="desir", main="Monochrome plates", ylab="density")
lines(density(food$desir[food$couleur==0]), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(food$desir[food$couleur==0]), 
                  sd=sd(food$desir[food$couleur==0])), from=20, to=200, add=TRUE), col="red", lwd=2)
# histogram plot desirs for group 1.
hist(food$desir[food$couleur==1], freq=FALSE, col="lightblue",
     xlab="desir", main="Colored plates", ylab="density")
lines(density(food$desir[food$couleur==1]), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(food$desir[food$couleur==1]), 
                  sd=sd(food$desir[food$couleur==1])), from=20, to=200, add=TRUE), col="red", lwd=2)
# Normal Quantile-Q plot desirs for group 0.
qqnorm(food$desir[food$couleur==0], pch=1, main="Monochrone plates", frame=FALSE)
qqline(food$desir[food$couleur==0], col="steelblue", lwd=2)
# Normal Quantile-Q plot desirs for group 1.
qqnorm(food$desir[food$couleur==1], pch=1, main="Colored plates", frame=FALSE)
qqline(food$desir[food$couleur==1], col="steelblue", lwd=2)
# Box plot offer
boxplot(food$desir ~ food$couleur, 
        horizontal=TRUE, 
        col="lightblue", 
        xlab="desir", 
        ylab="group",
        main="desirs per group",
        frame=FALSE)

# Selon les graphiques (Histogram et Q-Q plot) les deux groupes suivent une loi Normal
# et les variances ne sont pas égales, vérifions les variances avec un test de variances.
var.test(food$desir[food$couleur==0], food$desir[food$couleur==1])
# 	F test to compare two variances
# data:  food$desir[food$couleur == 0] and food$desir[food$couleur == 1]
# F = 1.2155, num df = 33, denom df = 33, p-value = 0.5782
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#     0.6070705 2.4338766
# sample estimates:
#   ratio of variances 
# 1.215539 
t.test(desir ~ couleur, data=food, var.equal=FALSE)
# 	Welch Two Sample t-test
# data:  desir by couleur
# t = 2.0095, df = 65.381, p-value = 0.04861
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.07618013 24.21793752
# sample estimates:
#   mean in group 0 mean in group 1 
# 61.73529        49.58824 

# La valeur-p provenant du test de Welch est de 0.04861 < 0.05, donc on rejette H0.
# Alors la moyenne des deux groupes sont significativement différentes.

#***************
# Exercice_2-4 #
#***************

assurance <- haven::read_sas("./MATH60619.H2020_R/datasets/assurance.sas7bdat")
head(assurance, n=2)
#     age sexe    imc enfant fumeur region   frais
# 1    19 femme  27.9      0 oui    sudouest 16885
# 2    18 homme  33.8      1 non    sudest    1726

dim(assurance)
# [1] 1338    7

summary(assurance)
#       age            sexe                imc            enfant         fumeur         
# Min.   :18.00   Length:1338        Min.   :16.00   Min.   :0.000   Length:1338       
# 1st Qu.:27.00   Class :character   1st Qu.:26.30   1st Qu.:0.000   Class :character  
# Median :39.00   Mode  :character   Median :30.40   Median :1.000   Mode  :character  
# Mean   :39.21                      Mean   :30.67   Mean   :1.095                     
# 3rd Qu.:51.00                      3rd Qu.:34.70   3rd Qu.:2.000                     
# Max.   :64.00                      Max.   :53.10   Max.   :5.000                     

# region              frais      
# Length:1338        Min.   : 1122  
# Class :character   1st Qu.: 4740  
# Mode  :character   Median : 9382  
# Mean   :13270  
# 3rd Qu.:16640  
# Max.   :63770  

# + age: âge (en années)
# + sexe: sexe, homme ou femme,
# + imc: indice de masse corporelle (en kg/m2),
# + enfant: nombre d’enfants à charge,
# + fumeur: oui pour les fumeurs, non autrement,
# + region: lieu de résidence, une région parmi sudouest, sudest, nordouest ou nordest,
# + frais: les frais médicaux annuels en 2013 (en dollars USD).

# a)
colnames(assurance)
# [1] "age"    "sexe"   "imc"    "enfant" "fumeur" "region" "frais"

par(mfrow=c(2, 2))
boxplot(frais ~ sexe, 
        data=assurance,
        boxwex=.3, 
        staplewex=.6,
        col="lightblue", 
        xlab="sex", 
        ylab="fee ($)",
        main="Fee per sex",
        frame=FALSE)
boxplot(frais ~ enfant, 
        data=assurance,
        boxwex=.3, 
        staplewex=.6,
        col="lightblue", 
        xlab="Nbr children", 
        ylab="fee ($)",
        main="Fee per children",
        frame=FALSE)
boxplot(frais ~ fumeur, 
        data=assurance,
        boxwex=.3, 
        staplewex=.6,
        col="lightblue", 
        xlab="smoke", 
        ylab="fee ($)",
        main="Fee per smokers",
        frame=FALSE)
boxplot(frais ~ region, 
        data=assurance,
        boxwex=.3, 
        staplewex=.6,
        col="lightblue", 
        xlab="region", 
        ylab="fee ($)",
        main="Fee per region",
        frame=FALSE)

assurance$sexe <- factor(assurance$sexe)
assurance$fumeur <- factor(assurance$fumeur)
par(mfrow=c(1, 1))
plot(frais ~ imc, 
     data=assurance, 
     col=c("red","blue", "green")[sexe], 
     pch=c(19,4,1)[sexe])
legend(x="topleft", 
       legend=levels(assurance$sexe),
       col=c("red","blue", "green"),
       pch=c(19,4,1),
       cex = 0.5)

par(mfrow=c(1, 1))
plot(frais ~ imc, 
     data=assurance, 
     col=c("red","blue", "green")[fumeur], 
     pch=c(19,4,1)[fumeur])
legend(x="topleft", 
       legend=levels(assurance$fumeur),
       col=c("red","blue", "green"),
       pch=c(19,4,1),
       cex = 0.5)

# Classification        IMC (kg/m2)
#-------------------------------------------------
# < 18.5              Insuffisance pondérale
# 18.5 – 24.9         Corpulence normale
# 25.0 – 29.9         Surpoids
# 30.0 – 34.9         Obésité
# 35.0 – 39.9         Obésité de classe II et III
#-------------------------------------------------
par(mfrow=c(1, 1))
assurance$classe <- cut(assurance$imc, breaks = c(0, 18.5, 25, 30, 35, max(assurance$imc)),
                        labels=c("<18.5", "18.5-24.9", "25.0-29.9", "30.0-34.9", "35.0-39.9"), include.lowest=TRUE)
assurance$classe <- factor(assurance$classe)
plot(frais ~ imc, 
     data=assurance, 
     col=c("#333333","#FFCC00", "#FF3300", "#99CC00", "#3399FF")[classe], 
     pch=c(19, 4, 1, 22, 23, 24)[classe])
legend(x="topleft", 
       legend=levels(assurance$classe),
       col=c("#333333", "#FFCC00", "#FF3300", "#99CC00", "#3399FF"),
       pch=c(19, 4, 1, 22, 23, 24),
       cex = 0.5)















