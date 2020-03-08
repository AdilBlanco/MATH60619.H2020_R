library(dplyr)
#************************************************************************************
#                                     ANOVA à un facteur                            #
#************************************************************************************
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

#************************************************************************************
#                               ANOVA et tests multiples                            # ???????
#************************************************************************************
?pairwise.t.test()
#Compute p-value for mean differences
pairwise.t.test(
  x = df$fiabilite,
  g = df$banque,
  p.adjust.method = "holm", #bonferroni
  pool.sd = FALSE
)
#        3     1    
#  1 0.245     -    
#  2 0.013 0.183

# I.C
emmeans::emmeans(oneway, "banque", )

#************************************************************************************
#                                   ANOVA à deux facteur                            #
#************************************************************************************
# Le fichier delai.sas7bdat contient les données simulées correspondant à cette étude, soit:
# temps: temps (en minutes) de délai estimé par le participant. 
# eval: évaluation du service en score standardisé.
# stade: niveau du premier facteur (stade d’avancement)
# 1.près du but 
# 2.loin du but
# delai: niveau du deuxième facteur (type de délai).
# 1.procédural 
# 2.correctionnel
# 3.inconnu

#************
# En ANOVA à deux facteurs, la notion d’interaction fait référence à laquestion suivante : 
# «à quel point l’effet d’un facteur est-il différent selon les niveaux de l’autre (et vice-versa)? »
#************
df2 <-
  haven::read_sas("./MATH60619.H2020_R/datasets/delai.sas7bdat")

df2 <- as.data.frame(delai)
head(df2, n=5)
#   delai stade temps  eval
# 1     1     1    11 -1.02
# 2     1     1    11 -0.69
# 3     1     1    15 -0.18
# 4     1     1    20 -0.21
# 5     1     1    16  0.10

agg <- df2 %>% group_by(stade, delai) %>% summarise(n = n(),
                                                    mean = mean(eval))
#  stade delai     n    mean
# 1 2     3        16  0.261 
# 2 2     1        17  0.325 
# 3 2     2        19 -0.783 
# 4 1     3        19  0.0226
# 5 1     1        20 -0.435 
# 6 1     2        18  0.491 

xtabs(mean ~ stade + delai, as.data.frame(agg))
#      delai
# stade           3           1           2
#     2  0.26062500  0.32529412 -0.78315789
#     1  0.02263158 -0.43500000  0.49111111

table(agg)
aggregate(df2$eval, list(df2$stade, df2$delai), mean)

sapply(df2, class)
#     delai     stade     temps      eval 
#  "numeric" "numeric" "numeric" "numeric"

# Cast categorical variables to factors
levels(as.factor(df2$delai))
df2$delai <- relevel(df2$delai, ref = "3")
df2$delai <- as.factor(df2$delai)

levels(as.factor(df2$stade))
df2$stade <- relevel(df2$stade, ref = "2")
df2$stade <- as.factor(df2$stade)

twoway <- lm(eval ~ stade + delai + stade*delai ,data=df2)
summary(twoway)
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.25263 -0.29062  0.04938  0.29737  1.01938 
# Coefficients:
#               Estimate Std. Error t value       Pr(>|t|)    
# (Intercept)    0.26062    0.11372   2.292          0.024 *  
# stade1        -0.23799    0.15435  -1.542          0.126    
# delai1         0.06467    0.15845   0.408          0.684    
# delai2        -1.04378    0.15435  -6.762 0.000000000840 ***
# stade1:delai1 -0.52230    0.21527  -2.426          0.017 *  
# stade1:delai2  1.51226    0.21497   7.035 0.000000000226 ***
#   ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 0.4549 on 103 degrees of freedom
# Multiple R-squared:  0.5133,	Adjusted R-squared:  0.4897 
# F-statistic: 21.73 on 5 and 103 DF,  p-value: 0.000000000000008514

anova(twoway)
# Analysis of Variance Table
# Response: eval
#              Df  Sum Sq Mean Sq F value                Pr(>F)    
# stade         1  0.3267  0.3267  1.5790               0.21175    
# delai         2  1.6142  0.8071  3.9005               0.02329 *  
# stade:delai   2 20.5388 10.2694 49.6284 0.0000000000000008074 ***        --> rejetter β4=β4=0
# Residuals   103 21.3134  0.2069                                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Il y a effectivement une interaction significative entre les variables delai et stade sur la variable eval.
# Donc l’effet de la variable delai sur la variable eval dépend du niveau considéré de la variable stade. 
# Puisqu’on inclut une interaction, les effets globaux de stade et de delai sont sans intérêt.

#************************************************************************************
#                               ANOVA et tests multiples                            #
#************************************************************************************
# Matrix of p-value for pairwise differences
pvals <- pairwise.t.test(
                x = df2$eval,
                g = df2$stade:df2$delai,
                data=df2,
                p.adjust.method = "none"
              )
pvals
#     2:3              2:1              2:2              1:3    1:1             
# 2:1 0.6840           -                -                -      -               
# 2:2 0.00000000083971 0.00000000006257 -                -      -               
# 1:3 0.1262           0.0489           0.00000033148616 -      -               
# 1:1 0.00001418420514 0.00000178756524 0.0187           0.0022 -               
# 1:2 0.1434           0.2836           0.00000000000014 0.0023 0.00000000870901

#Remove the p-values not of interest
pvals$p.value[3,2] <- pvals$p.value[3,4] <- pvals$p.value[3,3] <- NA
pvals$p.value[4,1] <- pvals$p.value[4,3] <- NA
pvals$p.value[5,1] <- pvals$p.value[5,2] <- NA

#     2:3              2:1              2:2              1:3    1:1             
# 2:1 0.6840           -                -                -      -               
# 2:2 0.00000000083971 0.00000000006257 -                -      -               
# 1:3 0.1262           -                -                -      -               
# 1:1 -                0.00000178756524 -                0.0022 -               
# 1:2 -                -                0.00000000000014 0.0023 0.00000000870901

id <- which(!is.na(pvals$p.value))
# Adjust p-values (Holm stepdown procedure with Bonferroni)
pvals$p.value[id] <- p.adjust(pvals$p.value[id],"holm")
pvals
#     2:3              2:1              2:2              1:3    1:1             
# 2:1 0.6840           -                -                -      -               
# 2:2 0.00000000083971 0.00000000006257 -                -      -               
# 1:3 0.1262           0.0489           0.00000033148616 -      -               
# 1:1 0.00001418420514 0.00000178756524 0.0187           0.0022 -               
# 1:2 0.1434           0.2836           0.00000000000014 0.0023 0.00000000870901

# Averages within each level of other variable
# Also compute mean differences with unadjusted p-values
simp1 <- emmeans::emmeans(twoway, pairwise ~ stade | delai)
simp2 <- emmeans::emmeans(twoway, pairwise ~ delai | stade)
rawpval <- c(simp1$contrasts$p.value, simp2$contrasts$p.value)










