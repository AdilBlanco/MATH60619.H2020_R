#***************
# Exercice_2-1 #
#***************

tickets <-
  haven::read_sas("./MATH60619.H2020_R/datasets/billets.sas7bdat")

head(tickets)
dim(tickets)
summary(tickets)

# H0: μ_cash = μ_credit
# Ha: μ_cash <> μ_credit
# ou μ_cash est la moyenne des individus payant comptant dans la population
# et μ_credit est la moyenne des individus payant avec carte de crédit dans la population

t.test(tickets$offre ~ tickets$groupe, alternative = "two.sided")
# 	Welch Two Sample t-test
# data:  tickets$offre by tickets$groupe
# t = -3.3887, df = 35.72, p-value = 0.001725
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -23.990758  -6.022927
# sample estimates:
#   mean in group 0 mean in group 1 
#          56.60606        71.61290 

# La valeur-p provenant du test de Welch est de 0.001725 < 0.05, donc on rejette H0, on conclut
# que le montant moyen est différent à un niveau de 5% pour les individus qui paient avec 
# carte de crédit et ceux qui paient comptant.

#***************
# Exercice_2-2 #
#***************
tickets_extrem <- cbind(tickets)
# Replace observation by 180
# tickets_extrem[1,1] <- 180 
tickets_extrem[1,]$offre <- 210

head(tickets_extrem, n = 2)
#   offre groupe
# 1   210      0
# 2    44      0

# a)
var.test(offre ~ groupe, data = tickets)
# 	F test to compare two variances
# data:  offre by groupe
# F = 0.10206, num df = 32, denom df = 30, p-value = 5.595e-09
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.04959248 0.20828966
# sample estimates:
#   ratio of variances 
# 0.1020609 
t.test(tickets$offre ~ tickets$groupe, var.eq = FALSE, alternative = "two.sided")
# 	Welch Two Sample t-test
# data:  tickets$offre by tickets$groupe
# t = -3.3887, df = 35.72, p-value = 0.001725
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -23.990758  -6.022927
# sample estimates:
#   mean in group 0 mean in group 1
#          56.60606        71.61290
var.test(offre ~ groupe, data = tickets_extrem)
# 	F test to compare two variances
# data:  offre by groupe
# F = 1.3884, num df = 32, denom df = 30, p-value = 0.369
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.6746542 2.8335643
# sample estimates:
#   ratio of variances 
# 1.388433 
t.test(tickets_extrem$offre ~ tickets_extrem$groupe, var.eq = TRUE, alternative = "two.sided")
# 	Welch Two Sample t-test
# data:  tickets_extrem$offre by tickets_extrem$groupe
# t = -1.6385, df = 61.388, p-value = 0.1064
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -23.361250   2.317261
# sample estimates:
#   mean in group 0 mean in group 1
#          61.09091        71.61290

# la valeur-p provenant du premier test de Welch avec les données qui ne contienent pas de valeurs
# abérantes est de 0.001725 < 0.05, donc on rejette H0, alors que la valeur-p provenant du deuxième
# test de Welch qui contienent des valeurs abérantes est de 0.1064 > 0.05, donc on ne rejette pas H0.

# b)
par(mfrow = c(2, 1))
boxplot(
  tickets$offre ~ tickets$groupe,
  horizontal = TRUE,
  col = "lightblue",
  xlab = "offre",
  ylab = "groupe",
  main = "Sans valeurs abérantes",
  frame = FALSE
)
boxplot(
  tickets_extrem$offre ~ tickets_extrem$groupe,
  horizontal = TRUE,
  col = "lightblue",
  xlab = "offre",
  ylab = "groupe",
  main = "Présence des valeurs abérantes",
  frame = FALSE
)

# c)
wilcox.test(tickets$offre ~ tickets$groupe, alternative = "two.sided")
# 	Wilcoxon rank sum test with continuity correction
# data:  tickets$offre by tickets$groupe
# W = 288.5, p-value = 0.002743
# alternative hypothesis: true location shift is not equal to 0
wilcox.test(tickets_extrem$offre ~ tickets_extrem$groupe, alternative =
              "two.sided")
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

par(mfrow = c(3, 2),
    pch = 20,
    bty = 'l')
# histogram plot offers for group 0.
hist(
  tickets$offre[tickets$groupe == 0],
  freq = FALSE,
  col = "lightblue",
  xlab = "offer",
  main = "Individuals who pay cash",
  ylab = "density"
)
lines(density(tickets$offre[tickets$groupe == 0]),
      lty = 2,
      col = "black",
      lwd = 1)
lines(curve(
  dnorm(
    x,
    mean = mean(tickets$offre[tickets$groupe == 0]),
    sd = sd(tickets$offre[tickets$groupe == 0])
  ),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# histogram plot offers for group 1.
hist(
  tickets$offre[tickets$groupe == 1],
  freq = FALSE,
  col = "lightblue",
  xlab = "offer",
  main = "Individuals who pay with credit card",
  ylab = "density"
)
lines(density(tickets$offre[tickets$groupe == 1]),
      lty = 2,
      col = "black",
      lwd = 1)
lines(curve(
  dnorm(
    x,
    mean = mean(tickets$offre[tickets$groupe == 1]),
    sd = sd(tickets$offre[tickets$groupe == 1])
  ),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# Normal Qiantile-Q plot offers for group 0.
qqnorm(tickets$offre[tickets$groupe == 0],
       pch = 1,
       main = "Individuals pay cash",
       frame = FALSE)
qqline(tickets$offre[tickets$groupe == 0], col = "steelblue", lwd = 2)
# Normal Qiantile-Q plot offers for group 1.
qqnorm(tickets$offre[tickets$groupe == 1],
       pch = 1,
       main = "Individuals pay with credit card",
       frame = FALSE)
qqline(tickets$offre[tickets$groupe == 1], col = "steelblue", lwd = 2)
# Box plot offer
boxplot(
  tickets$offre ~ tickets$groupe,
  horizontal = TRUE,
  col = "lightblue",
  xlab = "offer",
  ylab = "group",
  main = "Offers per group",
  frame = FALSE
)

# Normalité (avec valeurs abérantes)
par(mfrow = c(3, 2),
    pch = 20,
    bty = 'l')
# histogram plot offers for group 0.
hist(
  tickets_extrem$offre[tickets_extrem$groupe == 0],
  freq = FALSE,
  col = "lightblue",
  xlab = "offer",
  main = "Individuals who pay cash",
  ylab = "density"
)
lines(
  density(tickets_extrem$offre[tickets_extrem$groupe == 0]),
  lty = 2,
  col = "black",
  lwd = 1
)
lines(curve(
  dnorm(
    x,
    mean = mean(tickets_extrem$offre[tickets_extrem$groupe == 0]),
    sd = sd(tickets_extrem$offre[tickets_extrem$groupe == 0])
  ),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# histogram plot offers for group 1.
hist(
  tickets_extrem$offre[tickets_extrem$groupe == 1],
  freq = FALSE,
  col = "lightblue",
  xlab = "offer",
  main = "Individuals who pay with credit card",
  ylab = "density"
)
lines(
  density(tickets_extrem$offre[tickets_extrem$groupe == 1]),
  lty = 2,
  col = "black",
  lwd = 1
)
lines(curve(
  dnorm(
    x,
    mean = mean(tickets_extrem$offre[tickets_extrem$groupe == 1]),
    sd = sd(tickets_extrem$offre[tickets_extrem$groupe == 1])
  ),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# Normal Quantile-Q plot offers for group 0.
qqnorm(
  tickets_extrem$offre[tickets_extrem$groupe == 0],
  pch = 1,
  main = "Individuals pay cash",
  frame = FALSE
)
qqline(tickets_extrem$offre[tickets_extrem$groupe == 0], col = "steelblue", lwd =
         2)
# Normal Quantile-Q plot offers for group 1.
qqnorm(
  tickets_extrem$offre[tickets_extrem$groupe == 1],
  pch = 1,
  main = "Individuals pay with credit card",
  frame = FALSE
)
qqline(tickets_extrem$offre[tickets_extrem$groupe == 1], col = "steelblue", lwd =
         2)
# Box plot offer
boxplot(
  tickets_extrem$offre ~ tickets_extrem$groupe,
  horizontal = TRUE,
  col = "lightblue",
  xlab = "offer",
  ylab = "group",
  main = "Offers per group",
  frame = FALSE
)

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

fp.test(tickets_extrem$offre, tickets_extrem$groupe, alternative = "two.sided")
# 	Fligner-Policello test
# data:  tickets_extrem$offre and tickets_extrem$groupe
# U* = -Inf, p-value < 2.2e-16
# alternative hypothesis: true difference in location is not equal to 0

#***************
# Exercice_2-3 #
#***************

food <-
  haven::read_sas("./MATH60619.H2020_R/datasets/nourriture.sas7bdat")
head(food, n = 2)
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
mean(food$desir[food$couleur == 0])
# [1] 61.73529
mean(food$desir[food$couleur == 1])
# [1] 49.58824
# Les moyennes sont différente dans les deux groupes.

# b)
# H0: μ_m = μ_c
# Ha: μ_m <> μ_c
# ou μ_m est la moyenne des plats monochrome dans la population
# et μ_c est la moyenne des plats coloré dans la population

# c, d, e et f
par(mfrow = c(3, 2),
    pch = 20,
    bty = 'l')
# histogram plot desirs for group 0.
hist(
  food$desir[food$couleur == 0],
  freq = FALSE,
  col = "lightblue",
  xlab = "desir",
  main = "Monochrome plates",
  ylab = "density"
)
lines(density(food$desir[food$couleur == 0]),
      lty = 2,
      col = "black",
      lwd = 1)
lines(curve(
  dnorm(x, mean = mean(food$desir[food$couleur == 0]),
        sd = sd(food$desir[food$couleur == 0])),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# histogram plot desirs for group 1.
hist(
  food$desir[food$couleur == 1],
  freq = FALSE,
  col = "lightblue",
  xlab = "desir",
  main = "Colored plates",
  ylab = "density"
)
lines(density(food$desir[food$couleur == 1]),
      lty = 2,
      col = "black",
      lwd = 1)
lines(curve(
  dnorm(x, mean = mean(food$desir[food$couleur == 1]),
        sd = sd(food$desir[food$couleur == 1])),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# Normal Quantile-Q plot desirs for group 0.
qqnorm(food$desir[food$couleur == 0],
       pch = 1,
       main = "Monochrone plates",
       frame = FALSE)
qqline(food$desir[food$couleur == 0], col = "steelblue", lwd = 2)
# Normal Quantile-Q plot desirs for group 1.
qqnorm(food$desir[food$couleur == 1],
       pch = 1,
       main = "Colored plates",
       frame = FALSE)
qqline(food$desir[food$couleur == 1], col = "steelblue", lwd = 2)
# Box plot offer
boxplot(
  food$desir ~ food$couleur,
  horizontal = TRUE,
  col = "lightblue",
  xlab = "desir",
  ylab = "group",
  main = "desirs per group",
  frame = FALSE
)

# Selon les graphiques (Histogram et Q-Q plot) les deux groupes suivent une loi Normal
# et les variances ne sont pas égales, vérifions les variances avec un test de variances.
var.test(food$desir[food$couleur == 0], food$desir[food$couleur == 1])
# 	F test to compare two variances
# data:  food$desir[food$couleur == 0] and food$desir[food$couleur == 1]
# F = 1.2155, num df = 33, denom df = 33, p-value = 0.5782
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#     0.6070705 2.4338766
# sample estimates:
#   ratio of variances
# 1.215539
t.test(desir ~ couleur, data = food, var.equal = TRUE)
# Two Sample t-test
# data:  desir by couleur
# t = 2.0095, df = 66, p-value = 0.04857
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.0783125 24.2158051
# sample estimates:
#   mean in group 0 mean in group 1 
#          61.73529        49.58824 

# La valeur-p provenant du test-t est de 0.04857 < 0.05, donc on rejette H0.
# Alors la moyenne des deux groupes sont significativement différentes.

#***************
# Exercice_2-4 #
#***************

assurance <-
  haven::read_sas("./MATH60619.H2020_R/datasets/assurance.sas7bdat")
head(assurance, n = 2)
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

#*************************************************#
# Classification        IMC (kg/m2)               #
#*************************************************#
# < 18.5              Insuffisance pondérale      #
# 18.5 – 24.9         Corpulence normale          #
# 25.0 – 29.9         Surpoids                    #
# 30.0 – 34.9         Obésité                     #
# 35.0 – 39.9         Obésité de classe II et III #
#*************************************************#

# a)
colnames(assurance)
# [1] "age"    "sexe"   "imc"    "enfant" "fumeur" "region" "frais"

par(mfrow = c(2, 2))
boxplot(
  frais ~ sexe,
  data = assurance,
  boxwex = .3,
  staplewex = .6,
  col = "lightblue",
  xlab = "sex",
  ylab = "fee ($)",
  main = "Fee per sex",
  frame = FALSE
)
boxplot(
  frais ~ enfant,
  data = assurance,
  boxwex = .3,
  staplewex = .6,
  col = "lightblue",
  xlab = "Nbr children",
  ylab = "fee ($)",
  main = "Fee per children",
  frame = FALSE
)
boxplot(
  frais ~ fumeur,
  data = assurance,
  boxwex = .3,
  staplewex = .6,
  col = "lightblue",
  xlab = "smoke",
  ylab = "fee ($)",
  main = "Fee per smokers",
  frame = FALSE
)
boxplot(
  frais ~ region,
  data = assurance,
  boxwex = .3,
  staplewex = .6,
  col = "lightblue",
  xlab = "region",
  ylab = "fee ($)",
  main = "Fee per region",
  frame = FALSE
)

assurance$sexe <- factor(assurance$sexe)
assurance$fumeur <- factor(assurance$fumeur)
par(mfrow = c(1, 1))
plot(
  frais ~ imc,
  data = assurance,
  col = c("red", "blue", "green")[sexe],
  pch = c(19, 4, 1)[sexe]
)
legend(
  x = "topleft",
  legend = levels(assurance$sexe),
  col = c("red", "blue", "green"),
  pch = c(19, 4, 1),
  cex = 0.5
)

par(mfrow = c(1, 1))
plot(
  frais ~ imc,
  data = assurance,
  col = c("red", "blue", "green")[fumeur],
  pch = c(19, 4, 1)[fumeur]
)
legend(
  x = "topleft",
  legend = levels(assurance$fumeur),
  col = c("red", "blue", "green"),
  pch = c(19, 4, 1),
  cex = 0.5
)

par(mfrow = c(1, 1))
assurance$classe <-
  cut(
    assurance$imc,
    breaks = c(0, 18.5, 25, 30, 35, max(assurance$imc)),
    labels = c("<18.5", "18.5-24.9", "25.0-29.9", "30.0-34.9", "35.0-39.9"),
    include.lowest = TRUE
  )
assurance$classe <- factor(assurance$classe)
plot(
  frais ~ imc,
  data = assurance,
  col = c("#333333", "#FFCC00", "#FF3300", "#99CC00", "#3399FF")[classe],
  pch = c(19, 4, 1, 22, 23, 24)[classe]
)
legend(
  x = "topleft",
  legend = levels(assurance$classe),
  col = c("#333333", "#FFCC00", "#FF3300", "#99CC00", "#3399FF"),
  pch = c(19, 4, 1, 22, 23, 24),
  cex = 0.5
)
# b)
# H0: μ_fum = μ_non
# Ha: μ_fum <> μ_non
# ou μ_fum est la moyenne des frais médicaux payés par les fumeurs
# et μ_non est la moyenne des frais médicaux payés par non fumeurs.

par(mfrow = c(3, 2),
    pch = 20,
    bty = 'l')
# histogram plot fee for smokers.
hist(
  assurance$frais[assurance$fumeur == 'oui'],
  freq = FALSE,
  col = "lightblue",
  xlab = "fee",
  main = "Smoker",
  ylab = "density"
)
lines(
  density(assurance$frais[assurance$fumeur == 'oui']),
  lty = 2,
  col = "black",
  lwd = 1
)
lines(curve(
  dnorm(
    x,
    mean = mean(assurance$frais[assurance$fumeur == 'oui']),
    sd = sd(assurance$frais[assurance$fumeur == 'oui'])
  ),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# histogram plot fee for no smokers.
hist(
  assurance$frais[assurance$fumeur == 'non'],
  freq = FALSE,
  col = "lightblue",
  xlab = "fee",
  main = "No smoker",
  ylab = "density"
)
lines(
  density(assurance$frais[assurance$fumeur == 'non']),
  lty = 2,
  col = "black",
  lwd = 1
)
lines(curve(
  dnorm(
    x,
    mean = mean(assurance$frais[assurance$fumeur == 'non']),
    sd = sd(assurance$frais[assurance$fumeur == 'non'])
  ),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# Normal Quantile-Q plot fee for smokers.
qqnorm(assurance$frais[assurance$fumeur == 'oui'],
       pch = 1,
       main = "Smoker",
       frame = FALSE)
qqline(assurance$frais[assurance$fumeur == 'oui'], col = "steelblue", lwd =
         2)
# Normal Quantile-Q plot fee for no smokers.
qqnorm(assurance$frais[assurance$fumeur == 'non'],
       pch = 1,
       main = "No smoker",
       frame = FALSE)
qqline(assurance$frais[assurance$fumeur == 'non'], col = "steelblue", lwd =
         2)
# Box plot offer
boxplot(
  assurance$frais ~ assurance$fumeur,
  horizontal = TRUE,
  col = "lightblue",
  xlab = "fee",
  ylab = "smoker",
  main = "fee per group",
  frame = FALSE
)

var.test(frais ~ fumeur, data = assurance)
# 	F test to compare two variances
# data:  frais by fumeur
# F = 0.2697, num df = 1063, denom df = 273, p-value < 2.2e-16
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.2222063 0.3238772
# sample estimates:
#   ratio of variances
#   0.2696967

# La valeur de valeur-p provenant du test de variance est de 2.2e-16 < 0.05,
# donc on rejette H0, alors les variances dans les deux groupes sont
# significativement différentes. Donc pour faire un test d'égalité de moyenne
# le choix est porté sur un test non paramétrique (Fligner-Policello)

RVAideMemoire::fp.test(frais ~ fumeur, data = assurance, alternative = "two.sided")
# 	Fligner-Policello test
# data:  frais by fumeur
# U* = 139.41, p-value < 2.2e-16
# alternative hypothesis: true difference in location is not equal to 0

# La valeur de valeur-p provenante du test Plicello est de 2.2e-16 < 0.05,
# on rejette H0, alors les deux moyennes sont significativement différentes.

# c)
# H0: μ_fob = μ_fnob
# Ha: μ_fob > μ_fnob
# ou μ_fob est la moyenne des frais des fumeurs et obèse et
# μ_fnob est la moyenne des frais des fumeurs et non obèse.

# fummeur & obèses
# assurance$frais[(assurance$fumeur=="oui") & (assurance$imc >= 30)]
# fumeur & non obèses
# assurance$frais[(assurance$fumeur=="oui") & (assurance$imc < 30)]
assurance$is_obese <- with(assurance, ifelse(imc >= 30, 1, 0))
head(assurance, n = 4)
#     age sexe    imc enfant fumeur region    frais class     classe    is_obese
# 1    19 femme  27.9      0 oui    sudouest  16885 25.0-29.9 25.0-29.9        0
# 2    18 homme  33.8      1 non    sudest     1726 30.0-34.9 30.0-34.9        1
# 3    28 homme  33        3 non    sudest     4449 30.0-34.9 30.0-34.9        1
# 4    33 homme  22.7      0 non    nordouest 21984 18.5-24.9 18.5-24.9        0

par(mfrow = c(3, 2),
    pch = 20,
    bty = 'l')
# histogram plot fee for smokers & obese.
hist(
  assurance$frais[(assurance$fumeur == "oui") &
                    (assurance$imc >= 30)],
  freq = FALSE,
  col = "lightblue",
  xlab = "fee",
  main = "Smoker obese.",
  ylab = "density"
)
lines(
  density(assurance$frais[(assurance$fumeur == "oui") &
                            (assurance$imc >= 30)]),
  lty = 2,
  col = "black",
  lwd = 1
)
lines(curve(
  dnorm(
    x,
    mean = mean(assurance$frais[(assurance$fumeur == "oui") &
                                  (assurance$imc >= 30)]),
    sd = sd(assurance$frais[(assurance$fumeur == "oui") &
                              (assurance$imc >= 30)])
  ),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# histogram plot fee for smokers no obese.
hist(
  assurance$frais[(assurance$fumeur == "oui") & (assurance$imc < 30)],
  freq = FALSE,
  col = "lightblue",
  xlab = "fee",
  main = "Smoker no obese.",
  ylab = "density"
)
lines(
  density(assurance$frais[(assurance$fumeur == "oui") &
                            (assurance$imc < 30)]),
  lty = 2,
  col = "black",
  lwd = 1
)
lines(curve(
  dnorm(
    x,
    mean = mean(assurance$frais[(assurance$fumeur == "oui") &
                                  (assurance$imc < 30)]),
    sd = sd(assurance$frais[(assurance$fumeur == "oui") &
                              (assurance$imc < 30)])
  ),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# Normal Quantile-Q plot fee for smokers obese.
qqnorm(assurance$frais[(assurance$fumeur == "oui") &
                         (assurance$imc >= 30)],
       pch = 1,
       main = "Smoker obese.",
       frame = FALSE)
qqline(assurance$frais[(assurance$fumeur == "oui") &
                         (assurance$imc >= 30)], col = "steelblue", lwd = 2)
# Normal Quantile-Q plot fee for smokers no obese.
qqnorm(assurance$frais[(assurance$fumeur == "oui") &
                         (assurance$imc < 30)],
       pch = 1,
       main = "Smoker no obese.",
       frame = FALSE)
qqline(assurance$frais[(assurance$fumeur == "oui") &
                         (assurance$imc < 30)], col = "steelblue", lwd = 2)
# Box plot offer
boxplot(
  assurance$frais[assurance$fumeur == "oui"] ~ assurance$is_obese[assurance$fumeur ==
                                                                    "oui"],
  horizontal = TRUE,
  col = "lightblue",
  xlab = "fee",
  ylab = "smoker & is_obese",
  main = "fee per group",
  frame = FALSE
)

smoker_df <- as.data.frame(assurance[assurance$fumeur == "oui",])
var.test(frais ~ is_obese, data = smoker_df)
# 	F test to compare two variances
# data:  frais by is_obese
# F = 0.7057, num df = 128, denom df = 144, p-value = 0.04444
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.5040902 0.9913143
# sample estimates:
#   ratio of variances
# 0.7057005

t.test(
  frais ~ is_obese,
  data = smoker_df,
  var.eq = FALSE,
  alternative = "greater",
  conf.level = .95
)
# Welch Two Sample t-test
# data:  frais by is_obese
# t = -30.108, df = 271.13, p-value = 1
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   -21301.83       Inf
# sample estimates:
#   mean in group 0 mean in group 1
#         21363.21        41557.99

# La valeur-p provenante du test de Welch est de 1 > 0.05, donc
# on ne rejette pas H0.

t.test(
  frais ~ is_obese,
  data = smoker_df,
  var.eq = FALSE,
  alternative = "greater",
  conf.level = .9
)
# 	Welch Two Sample t-test
# data:  frais by is_obese
# t = -30.108, df = 271.13, p-value = 1
# alternative hypothesis: true difference in means is greater than 0
# 90 percent confidence interval:
#   -21056.47       Inf
# sample estimates:
#  mean in group 0 mean in group 1
#         21363.21        41557.99

t.test(
  frais ~ is_obese,
  data = smoker_df,
  var.eq = FALSE,
  alternative = "greater",
  conf.level = .99
)
# 	Welch Two Sample t-test
# data:  frais by is_obese
# t = -30.108, df = 271.13, p-value = 1
# alternative hypothesis: true difference in means is greater than 0
# 99 percent confidence interval:
#   -21764.43       Inf
# sample estimates:
#   mean in group 0 mean in group 1
#          21363.21        41557.99

# d)
par(mfrow = c(3, 2),
    pch = 20,
    bty = 'l')
# histogram plot imc vs women.
hist(
  assurance$imc[assurance$sexe == "femme"],
  freq = FALSE,
  col = "lightblue",
  xlab = "fee",
  main = "Women vs IMC.",
  ylab = "density"
)
lines(density(assurance$imc[assurance$sexe == "femme"]),
      lty = 2,
      col = "black",
      lwd = 1)
lines(curve(
  dnorm(
    x,
    mean = mean(assurance$imc[assurance$sexe == "femme"]),
    sd = sd(assurance$imc[assurance$sexe == "femme"])
  ),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# histogram plot imc vs men.
hist(
  assurance$imc[assurance$sexe == "homme"],
  freq = FALSE,
  col = "lightblue",
  xlab = "fee",
  main = "Men vs IMC.",
  ylab = "density"
)
lines(density(assurance$imc[assurance$sexe == "homme"]),
      lty = 2,
      col = "black",
      lwd = 1)
lines(curve(
  dnorm(
    x,
    mean = mean(assurance$imc[assurance$sexe == "homme"]),
    sd = sd(assurance$imc[assurance$sexe == "homme"])
  ),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# Normal Quantile-Q plot imc vs Women.
qqnorm(assurance$imc[assurance$sexe == "femme"],
       pch = 1,
       main = "Women vs IMC.",
       frame = FALSE)
qqline(assurance$imc[assurance$sexe == "femme"], col = "steelblue", lwd =
         2)
# Normal Quantile-Q plot imc vs men.
qqnorm(assurance$imc[assurance$sexe == "homme"],
       pch = 1,
       main = "Men vs IMC.",
       frame = FALSE)
qqline(assurance$imc[assurance$sexe == "homme"], col = "steelblue", lwd =
         2)
# Box plot offer
boxplot(
  assurance$imc ~ assurance$sexe,
  horizontal = TRUE,
  col = "lightblue",
  xlab = "imc",
  ylab = "smoker & is_obese",
  main = "IMC per sexe",
  frame = FALSE
)

# H0: μ_imcm = μ_imcw
# Ha: μ_imcm <> μ_imcw
# ou μ_imcm est la moyenne de l'indice de masse corporelle chez les hommes et
# μ_imcw est la moyenne de l'indice de masse corporelle chez les femmes.

var.test(imc ~ sexe, data = assurance)
# 	F test to compare two variances
# data:  imc by sexe
# F = 0.96967, num df = 661, denom df = 675, p-value = 0.691
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.8331483 1.1286986
# sample estimates:
#   ratio of variances
#  0.9696692

t.test(imc ~ sexe, data = assurance, var.eq = FALSE)
# 	Welch Two Sample t-test
# data:  imc by sexe
# t = -1.6974, df = 1336, p-value = 0.08986
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.21910025  0.08808432
# sample estimates:
#   mean in group femme mean in group homme
#           30.37976            30.94527

# La valeur-p provenante du test de Welch est de 0.08986 > 0.05, on ne rejette
# pas H0. Alors on a pas assez de preuves pour conclure qu'il n'y a une différence
# entre entre l'indice de masse corporelle chez les hommes et les femmes.

# e)
# H0: μ_imcn = μ_imcs
# Ha: μ_imcn <> μ_imcs
# ou μ_imcn est la moyenne de l'indice corporelle chez les habitants du Nord
# et μ_imcs est la moyenne de l'indice corporelle chez les habitants du Sud.

assurance$direction <-
  with(assurance, ifelse(region %in% c("sudouest", "sudest"), "S", "N"))

par(mfrow = c(3, 2),
    pch = 20,
    bty = 'l')
# histogram plot imc vs direction Nord
hist(
  assurance$imc[assurance$direction == "N"],
  freq = FALSE,
  col = "lightblue",
  xlab = "fee",
  main = "North vs IMC.",
  ylab = "density"
)
lines(density(assurance$imc[assurance$direction == "N"]),
      lty = 2,
      col = "black",
      lwd = 1)
lines(curve(
  dnorm(
    x,
    mean = mean(assurance$imc[assurance$direction == "N"]),
    sd = sd(assurance$imc[assurance$direction == "N"])
  ),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# histogram plot imc vs direction Sud
hist(
  assurance$imc[assurance$direction == "S"],
  freq = FALSE,
  col = "lightblue",
  xlab = "fee",
  main = "South vs IMC.",
  ylab = "density"
)
lines(density(assurance$imc[assurance$direction == "S"]),
      lty = 2,
      col = "black",
      lwd = 1)
lines(curve(
  dnorm(
    x,
    mean = mean(assurance$imc[assurance$direction == "S"]),
    sd = sd(assurance$imc[assurance$direction == "S"])
  ),
  from = 20,
  to = 200,
  add = TRUE
),
col = "red",
lwd = 2)
# Normal Quantile-Q plot imc vs direction Nord
qqnorm(assurance$imc[assurance$direction == "N"],
       pch = 1,
       main = "North vs IMC.",
       frame = FALSE)
qqline(assurance$imc[assurance$direction == "N"], col = "steelblue", lwd =
         2)
# Normal Quantile-Q plot imc vs direction Sud.
qqnorm(assurance$imc[assurance$direction == "S"],
       pch = 1,
       main = "South vs IMC.",
       frame = FALSE)
qqline(assurance$imc[assurance$direction == "S"], col = "steelblue", lwd =
         2)
# Box plot offer
boxplot(
  assurance$imc ~ assurance$direction,
  horizontal = TRUE,
  col = "lightblue",
  xlab = "imc",
  ylab = "Direction",
  main = "IMC per direction",
  frame = FALSE
)

var.test(imc ~ direction , data = assurance)
# 	F test to compare two variances
# data:  imc by direction
# F = 0.78249, num df = 648, denom df = 688, p-value = 0.001586
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.6723606 0.9109823
# sample estimates:
#   ratio of variances
# 0.7824924

t.test(imc ~ direction, data = assurance, var.eq = FALSE)
# 	Welch Two Sample t-test
# data:  imc by direction
# t = -8.8715, df = 1330.8, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.501108 -2.233108
# sample estimates:
#   mean in group N mean in group S
#        29.18906        32.05617

# La valeur-p provenante du test de Welch est de 2.2e-16 < 0.05,
# donc on rejette H0, on peut conclure qu'il y a une différence
# de moyenne entre l'indice de masse corporelle chez les habitants
# du Nord et les habitants du Sud.
