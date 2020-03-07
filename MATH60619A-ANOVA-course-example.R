library(dplyr)

#*********************
# ANOVA à un facteur #
#*********************
# Lorsque X contient que les variables catégorielles, le modèle peut être utilisé pour tester les différentes
# moyennes dans les sous-groupes.

# Objectif -> comparer la perception de la qualité du service offert aux clients.
# fiabilite: la capacité de réaliser le service de manière fiable etprécise,
# assurance: la connaissance et la courtoisie des employés et leurcapacité à inspirer confiance,
# tangibles: l’apparence des locaux, des équipements, du personnelet du matériel de communication,
# empathie: la capacité de faire attention au client de manièreindividuelle,
# reactivite: la volonté d’aider les clients et de fournir un servicerapide.

# Pour cet exemple, nous utiliserons seulement le score de la dimension fiabilité.
# Plus le score est élevé, plus le client estime que la banque est fiable.
# Le fichier servqual.sas7bdat contient les données.
# La variable fiabilite contient le score pour la dimension fiabilité
# La variable banque identifie la banque:
# 1.United Bank of India
# 2.State Bank of India
# 3.Allahabad Bank
data <-
  haven::read_sas("./MATH60619.H2020_R/datasets/servqual.sas7bdat")
df <- as.data.frame(data)
head(df, n = 4)
#   banque fiabilite tangible assurance reactivite empathie
# 1      1      2.74     1.36      1.72       1.7      1.09
# 2      1      1.48     0.92      1.11       1.03     0.53
# 3      1      2.06     1.07      2.03       1.46     0.46
# 4      1      1.66     1.15      1.05       1        1.2

df %>% group_by(banque) %>% summarise(
  n = n(),
  mean = mean(fiabilite),
  sd = sd(fiabilite),
  min = min(fiabilite),
  max = max(fiabilite)
)
#   banque     n  mean     sd   min   max
# 1      1    34  2.13  0.551  1     3.57
# 2      2    33  1.94  0.340  1.13  2.6
# 3      3    33  2.30  0.590  0.5   3.28

# On veut tester si les scores moyens (dans la sous-population μ1, μ2 et μ3) des trois banques
# sont significativement différents les uns des autres.
# Les moyennes observées sont:
# X1_bar = 2.13 pour United Bank of India
# X2_bar = 1.94 pour State Bank of India
# X3_bar = 2.30 pour Allahabad Bank

# H0: μ1 = μ2 = μ3
# Ha: au moins deux moyennes différentes entre elles
# où μi est la moyenne du score de fiabilité pour la banque i.

# fiabilite = β0 + β1.banque1 + β2.banque2 + E
# E(fiabilite|banque1) = β0 + β1
# E(fiabilite|banque2) = β0 + β2
# E(fiabilite|banque3) = β0.
# où μ1 = β0 + β1 ,μ2 = β0 + β2 et μ3 = β0
# H0: μ1 = μ2 = μ3 devient β0 = β1 = 0

levels(as.factor(df$banque))
df$banque <- factor(df$banque)
df$banque <- relevel(df$banque, ref = "3")
oneway <- lm(fiabilite ~ banque, data = df)
summary(oneway)
# Residuals:
#     Min      1Q  Median      3Q     Max
# -1.7954 -0.2850  0.0400  0.3225  1.4385
# Coefficients:
#   Estimate  Std. Error    t value                     Pr(>|t|)
# (Intercept)    2.29545    0.08813  26.045 < 0.0000000000000002 ***
#   banque1     -0.16398    0.12372  -1.325              0.18814
#   banque2     -0.35545    0.12464  -2.852              0.00531 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 0.5063 on 97 degrees of freedom
# Multiple R-squared:  0.0775,	Adjusted R-squared:  0.05848
# F-statistic: 4.075 on 2 and 97 DF,  p-value: 0.01999
anova(oneway)
# Analysis of Variance Table
# Response: fiabilite
#           Df Sum Sq Mean Sq F value  Pr(>F)
# banque     2  2.089 1.04449  4.0747 0.01999 *
# Residuals 97 24.864 0.25633
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# La valeur-p du test-F est 0.02, donc on rejette H0: μ1=μ2=μ3.
# On conclut qu’il y a une différence significative entre les scores moyens pour la fiabilité des trois banques.

# Le test-F (ANOVA) est valide sous les postulas suivants:
# 1- Indépendance
# 2- Linéarité
# 3- Normalité: si la taille d'échantillon est grande, la normalité est toujour valide.
# 4- Homoscédasticité: l'égalite des variances dans les différents groupes.

#************
# Normalité #
#************
# banque-1:
par(mfrow = c(2, 2),
    pch = 20,
    bty = 'l')
hist(
  df$fiabilite[df$banque == 1],
  freq = FALSE,
  col = "lightblue",
  xlab = "fiabilite",
  main = "Fiabilite of bank-1",
  ylab = "density"
)
lines(density(df$fiabilite[df$banque == 1]),
      lty = 2,
      col = "black",
      lwd = 1)
lines(curve(
  dnorm(
    x,
    mean = mean(df$fiabilite[df$banque == 1]),
    sd = sd(df$fiabilite[df$banque == 1])
  ),
  from = 0,
  to = 8,
  add = TRUE
),
col = "red",
lwd = 2)
# banque-2:
hist(
  df$fiabilite[df$banque == 2],
  freq = FALSE,
  col = "lightblue",
  xlab = "fiabilite",
  main = "Fiabilite of bank-2",
  ylab = "density"
)
lines(density(df$fiabilite[df$banque == 2]),
      lty = 2,
      col = "black",
      lwd = 1)
lines(curve(
  dnorm(
    x,
    mean = mean(df$fiabilite[df$banque == 2]),
    sd = sd(df$fiabilite[df$banque == 2])
  ),
  from = 0,
  to = 8,
  add = TRUE
),
col = "red",
lwd = 2)
# banque-3:
hist(
  df$fiabilite[df$banque == 3],
  freq = FALSE,
  col = "lightblue",
  xlab = "fiabilite",
  main = "Fiabilite of bank-3",
  ylab = "density"
)
lines(density(df$fiabilite[df$banque == 3]),
      lty = 2,
      col = "black",
      lwd = 1)
lines(curve(
  dnorm(
    x,
    mean = mean(df$fiabilite[df$banque == 3]),
    sd = sd(df$fiabilite[df$banque == 3])
  ),
  from = 0,
  to = 8,
  add = TRUE
),
col = "red",
lwd = 2)
#*******************
# Homoscédasticité #
#*******************
# Levene's test for equality of variance
car::leveneTest(mod, center = "mean")
# Levene's Test for Homogeneity of Variance (center = "mean")
#       Df F value  Pr(>F)
# group  2  3.6442 0.02978 *
#   97
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# -> Dans ce cas, on rejette l’hypothèse d’égalité des variances (la valeur-p du test de Levene est 0.0298)

par(mfrow=c(1,1))
boxplot(df$fiabilite ~ df$banque, 
        horizontal=TRUE, 
        col="lightblue", 
        xlab="fiabilite", 
        ylab="banck",
        main="Fiabilite by bank",
        frame=FALSE)
#**************************
# Welch's ANOVA statistic #
#**************************
oneway.test(fiabilite ~ banque, data=df, var.equal=FALSE)
# One-way analysis of means (not assuming equal variances)
# data:  fiabilite and banque
# F = 4.9023, num df = 2.000, denom df = 60.622, p-value = 0.01063
# -> La valeur-p de Welch ANOVA est de 0.01063, on rejette H0: μ1 = μ2 = μ3

#****************************
# Kruskal's ANOVA statistic #
#****************************
# Kruskal-Wallis test as a one-way ANOVA with ranks
anova(lm(rank(fiabilite) ~ banque, data=df))
# Analysis of Variance Table
# Response: rank(fiabilite)
#             Df Sum Sq Mean Sq F value   Pr(>F)
# banque       2   8435  4217.5  5.4637 0.005643 **
#   Residuals 97  74875   771.9
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Function doesn't work correctly in presence of ties
kruskal.test(fiabilite ~ banque, data=df)
# 	Kruskal-Wallis rank sum test
# data:  fiabilite by banque
# Kruskal-Wallis chi-squared = 10.024, df = 2, p-value = 0.006659
# Alternative (here gives same output)
coin::kruskal_test(fiabilite ~ banque, data = df)
# 	Asymptotic Kruskal-Wallis Test
# data:  fiabilite by banque (3, 1, 2)
# chi-squared = 10.024, df = 2, p-value = 0.006659










