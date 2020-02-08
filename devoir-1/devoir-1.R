
df <- read.csv("./MATH60619.H2020_R/devoir-1/renfe_fr.csv", header=TRUE)
head(df, n=4)
#    prix type     classe    tarif dest duree jour
# 1 143.4  AVE Preferente    Promo    0   190    6
# 2 181.5  AVE Preferente Flexible    0   190    2
# 3  86.8  AVE Preferente    Promo    0   165    7
# 4  86.8  AVE Preferente    Promo    0   190    7

dim(df)
# [1] 10000     7
colnames(df)
# [1] "prix"   "type"   "classe" "tarif"  "dest"   "duree"  "jour"  

summary(df)
#      prix             type              classe           tarif           dest            duree      
# Min.   : 32.3   AVE     :9174   Preferente : 809   AdultoIda: 397   Min.   :0.0000   Min.   :150.0  
# 1st Qu.: 75.4   AVE-TGV : 429   Turista    :7197   Flexible :1544   1st Qu.:0.0000   1st Qu.:150.0  
# Median : 85.1   REXPRESS: 397   TuristaPlus:1916   Promo    :8059   Median :1.0000   Median :170.0  
# Mean   : 86.1                   TuristaSolo:  78                    Mean   :0.5062   Mean   :185.8  
# 3rd Qu.:100.4                                                       3rd Qu.:1.0000   3rd Qu.:190.0  
# Max.   :214.2                                                       Max.   :1.0000   Max.   :562.0  

ave <- df[df$type == "AVE", ]
tgv <- df[df$type == "AVE-TGV", ]
express <- df[df$type == "REXPRESS", ]


df1k <- df[1:1000,]
summary(df1k)
#       prix              type             classe          tarif          dest       duree      
# Min.   : 40.95   AVE     :921   Preferente :420   AdultoIda: 29   Min.   :0   Min.   :150.0  
# 1st Qu.: 75.40   AVE-TGV : 50   Turista    :580   Flexible :147   1st Qu.:0   1st Qu.:165.0  
# Median : 88.95   REXPRESS: 29   TuristaPlus:  0   Promo    :824   Median :0   Median :170.0  
# Mean   : 92.02                  TuristaSolo:  0                   Mean   :0   Mean   :182.9  
# 3rd Qu.:100.40                                                    3rd Qu.:0   3rd Qu.:190.0  
# Max.   :214.20                                                    Max.   :0   Max.   :544.0  

# Non ça ne serait pas un bon échantillon vu l'abscence des classes "TuristaPlus" et "TuristaSolo" 
# ainsi l'échantillon contient que les trajets Barcelone vers Madrid (0).

df4 <- read.csv("./MATH60619.H2020_R/devoir-1/renfe_simu_fr.csv", header=TRUE)
head(df4, n=4)
#     difmoy     statW        icbi       icbs       valp
# 1 -0.4818434 -1.174404 -1.28609052 0.32240363 0.24026152
# 2  0.8749708  2.173267  0.08578143 1.66416017 0.02978365
# 3 -0.7291113 -1.824994 -1.51223959 0.05401705 0.06803169
# 4 -0.5833163 -1.448819 -1.37252279 0.20589020 0.14741949

# (4-a) 
(nrow(df4[(df4$icbi < -0.28) & (df4$icbs > -0.28), ]) / nrow(df4)) * 100
# (4-b)
par(mfrow=c(1,1), pch=20, bty='l')
hist(df4$difmoy, col="cornflowerblue")
abline(v=-0.28,col="red")
# (4-c)
(nrow(df4[df4$valp < 0.05, ]) / nrow(df4)) * 100

# (5-a)
# H0: tgv = rexpr
# H1: tgv <> rexpr

# tgv_df <- df[df$type=="AVE-TGV", ]
# expr_df <- df[df$type=="REXPRESS", ]

# source('./MATH60619.H2020_R/devoir-1/rquery_t_test.r')
# rquery.t.test(tgv_df$prix, expr_df$prix)

df5 <- df[df$type=="AVE-TGV" | df$type=="REXPRESS", ]
nrow(df5)

summary(df5[df5$type=="AVE-TGV", ])
summary(df5[df5$type=="REXPRESS", ])
tgv_df <- df5[df5$type=="AVE-TGV", ]
expr_df <- df5[df5$type=="REXPRESS", ]

boxplot(df5$prix ~ df5$type, xlab = "type train")

par(mfrow=c(1,2), pch=20, bty='l')
hist(tgv_df$prix, freq = FALSE, col="cornflowerblue")
lines(density(tgv_df$prix), lty=1, col = "red")
hist(expr_df$prix, col="cornflowerblue")

#******************************************************************************
df6 <- df[df$type=="AVE" | df$type=="AVE-TGV", ]
entrants <- df6[df6$dest==1, ] # madrid->barcelone
sortants <- df6[df6$dest==0, ] # barcelone->madrid

par(mfrow=c(1,3), pch=20, bty='l')
# Vérification de la normalité des échantillons
hist(entrants$prix, freq=FALSE, col="lightblue",
     xlab="prix de billet en (Euro)", main="Histogramme Madrid-Barcelone", ylab="Densité")
lines(density(entrants$prix), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(entrants$prix), 
                  sd=sd(entrants$prix)), from=20, to=200, add=TRUE), col="red", lwd=2)

hist(sortants$prix, freq=FALSE, col="lightblue",
     xlab="prix de billet en (Euro)", main="Histogramme Barcelone-Madrid", ylab="Densité")
lines(density(sortants$prix), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(sortants$prix), 
                  sd=sd(sortants$prix)), from=20, to=200, add=TRUE), col="red", lwd=2)

boxplot(df6$prix ~ df6$dest, xlab="Déstination", 
        ylab = "prix de billet en (Euro)",
        frame = FALSE)

# H0: µ0 == µ1 (hypthèse null)
# Ha: µ0 <> µ1 (hypthèse alternative)
# + ou µ0 est le prix moyen du billet pour un train de grand vitesse allant 
#   de barcelone à madrid et µ1 est le prix moyen du billet pour un train de 
#   grand vitesse allant de madrid à barcelone.
t.test(prix ~ dest, data=df6, var.equal=T, alternative='two.sided')
# 	Two Sample t-test
# data:  prix by dest
# t = -2.2963, df = 9601, p-value = 0.02168
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -1.7568352 -0.1387103
#sample estimates:
#  mean in group 0 mean in group 1 
#         87.38419        88.33197 

# La valeur-p du test bilatéral de test-t pour deux échantillons est 0.02168.
# On rejette l'hypothèse nulle, donc le prix moyen d'une direction est plus chère
# que l'autre à niveau de confiance de 5%.
mean(df6$prix[df6$dest==0]) # 87.38419 !!!!
mean(df6$prix[df6$dest==1]) # 88.33197 !!!!

wilcox.test(prix ~ dest, data=df6)
# Wilcoxon rank sum test with continuity correction
# data:  prix by dest
# W = 11184249, p-value = 0.01208
# alternative hypothesis: true location shift is not equal to 0

#******************************************************************************
df7 <- df[df$type=="AVE-TGV", ]
unique(df7$jour)

weekend <- df7[df7$jour %in% c(6, 7), ]
weekday <- df7[df7$jour %in% c(1, 2, 3, 4, 5), ]
df7$flag <- with(df7, ifelse(jour %in% c(6, 7), 0, 1))

par(mfrow=c(1,3), pch=20, bty='l')
# Vérification de la normalité des échantillons
hist(weekday$prix, freq=FALSE, col="lightblue",
     xlab="prix de billet en (Euro)", main="Histogramme jour de la semaine", ylab="Densité")
lines(density(weekday$prix), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(weekday$prix), 
                  sd=sd(weekday$prix)), from=20, to=200, add=TRUE), col="red", lwd=2)

hist(weekend$prix, freq=FALSE, col="lightblue",
     xlab="prix de billet en (Euro)", main="Histogramme fin de semaine", ylab="Densité")
lines(density(weekend$prix), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(weekend$prix), 
                  sd=sd(weekend$prix)), from=20, to=200, add=TRUE), col="red", lwd=2)

boxplot(df7$prix ~ df7$flag, xlab="jours de semaine", 
        ylab = "prix de billet en (Euro)",
        frame = FALSE)

# H0: µ0 == µ1 ~ µ0 - µ1 = 0 (hypthèse null)
# Ha: µ0 > µ1 ~  µ0 - µ1 > 0 (hypthèse alternative)
# + ou µ1 est le prix moyen du billet pour les trains AVE-TGV en jours de la semaine
#   et µ0 est le prix moyen du billet pour les trains AVE-TGV en fin de semaine. 
var(weekday$prix)
var(weekend$prix)
mean(weekday$prix)
mean(weekend$prix)
t.test(prix ~ flag, data=df7, var.equal=F, alternative='greater')
# 	Welch Two Sample t-test
# data:  prix by flag
# t = 3.0033, df = 279.16, p-value = 0.001456
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#  2.441746      Inf
# sample estimates:
#   mean in group 0 mean in group 1 
#          92.74634        87.32627 

# La valeur-p du test unilatérale de test Welch pour deux échantillons est 0.001456.
# On rejette l'hypothèse nulle, donc le prix moyen du billet pour les trains AVE-TGV
# en fin de semaine est plus chère que les jours de la semiane à niveau de confiance de 5%.

#******************************************************************************
