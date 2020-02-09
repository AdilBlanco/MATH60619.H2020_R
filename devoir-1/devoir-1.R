
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

#**************************
#  Normality verification  #
#**************************
par(mfrow=c(3,2), pch=20, bty='l')
# histogram plot ticket prices for destination (1) Madrid->Barcelone.
hist(entrants$prix, freq=FALSE, col="lightblue",
     xlab="ticket price (Euro)", main="Madrid-Barcelone Histogram", ylab="density")
lines(density(entrants$prix), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(entrants$prix), 
                  sd=sd(entrants$prix)), from=20, to=200, add=TRUE), col="red", lwd=2)
# histogram plot ticket prices for destination (0) Barcelone->Madrid.
hist(sortants$prix, freq=FALSE, col="lightblue",
     xlab="ticket price (Euro)", main="Barcelone-Madrid Histogram", ylab="density")
lines(density(sortants$prix), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(sortants$prix), 
                  sd=sd(sortants$prix)), from=20, to=200, add=TRUE), col="red", lwd=2)
# Normal Qiantile-Q plot ticket prices for destination (1) Madrid->Barcelone.
qqnorm(entrants$prix, pch=1, main="Madrid-Barcelone Normal Q-Q plot", frame=FALSE)
qqline(entrants$prix, col="steelblue", lwd=2)
# Normal Qiantile-Q plot ticket prices for destination (0) Barcelone->Madrid.
qqnorm(sortants$prix, pch=1, main="Barcelone-Madrid Normal Q-Q plot", frame=FALSE)
qqline(sortants$prix, col="steelblue", lwd=2)
# Box plot ticket prices for both destination
boxplot(df6$prix ~ df6$dest, 
        horizontal=TRUE, 
        col="lightblue", 
        xlab="ticket price (Euro)", 
        ylab="destination",
        main="Destinations prices",
        frame=FALSE)

#**********************************
#  homoscedasticity verification  #
#**********************************
var(entrants$prix) 
# [1] 430.1997
var(sortants$prix)
# [1] 386.7636
# var(entrants$prix) and var(sortants$prix) are different.

# H0: µ0 == µ1 (hypthèse null)
# Ha: µ0 <> µ1 (hypthèse alternative)
# + ou µ0 est le prix moyen du billet pour un train de grand vitesse allant 
#   de barcelone à madrid et µ1 est le prix moyen du billet pour un train de 
#   grand vitesse allant de madrid à barcelone.
t.test(prix ~ dest, data=df6, var.equal=F, alternative='two.sided')
# 	Welch Two Sample t-test
# data:  prix by dest
# t = -2.2984, df = 9597.7, p-value = 0.02156
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.756087 -0.139458
# sample estimates:
#   mean in group 0 mean in group 1 
#          87.38419        88.33197 

# La valeur-p du test bilatéral de Welch pour deux échantillons est 0.02168.
# On rejette l'hypothèse nulle, donc le prix moyen d'une direction est plus chère
# que l'autre à niveau de 5% pour le trajet Madrid-Barcelone et celui de Barcelone-Madrid.
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
df8 <- df[(df$type=="AVE" | df$type=="AVE-TGV") & (df$tarif=="Promo"), ]
df8$weekend <- with(df8, ifelse(jour %in% c(6, 7), 1, 0))
head(df8, n=5)
#     prix    type     classe tarif dest duree jour weekend
# 1 143.40     AVE Preferente Promo    0   190    6       1
# 3  86.80     AVE Preferente Promo    0   165    7       1
# 4  86.80     AVE Preferente Promo    0   190    7       1
# 5  68.95 AVE-TGV Preferente Promo    0   175    4       0
# 6  53.20     AVE Preferente Promo    0   190    7       1

# (8-a)
# prix = β0 + β1.dest + β2.classe + β3.duree + β4.weekend + E

# (8-b)
class(df8$classe)
df8$classe <- factor(df8$classe)
mod <- lm(prix ~ dest + classe + duree + weekend, data=df8)
summary(mod)
# Residuals:
#   Min      1Q    Median      3Q     Max 
# -53.473  -7.450   1.841   9.225  68.610 
# Coefficients:
#                   Estimate Std. Error   t value    Pr(>|t|)    
# (Intercept)       135.341077   1.708457  79.218   <2e-16 ***
#   dest              0.433242   0.298252   1.453    0.146    
# classeTurista     -17.764789   0.532803 -33.342   <2e-16 ***
# classeTuristaPlus  -6.927631   0.587443 -11.793   <2e-16 ***
# classeTuristaSolo  -9.069268   9.471460  -0.958    0.338    
# duree              -0.231672   0.009464 -24.480   <2e-16 ***
# weekend            -0.408880   0.347892  -1.175    0.240    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 13.37 on 8052 degrees of freedom
# Multiple R-squared:  0.206,	Adjusted R-squared:  0.2054 
# F-statistic: 348.1 on 6 and 8052 DF,  p-value: < 2.2e-16

# prix_^ = 135.34 + 0.44.dest_^ + ((-17.76) + (-6.93) + (-9.07)).classe_^ + (-0.23).duree_^ 
#          + (-0.41).weekend_^

# dest (binaire): En fixant toutes les autres co-variables, toutes choses étant égales, 
#                 le prix moyen du trajet Madrid->Barcelone augmente de 0.44 par rapport 
#                 au trajet Barcelone->Madrid.
# duree (continue): En fixant toutes les autres co-variables, toutes choses étant égales, 
#                   une augmentation de la duree d'une minute implique une dimunition de 0.41 minutes
#                   du prix en moyenne.
# weekend (binaire): En fixant toutes les autres co-variables, toutes choses étant égales,
#                    le prix moyen en fin de semaine diminue de 0.41 par rappot au jour semaine.
#
#
# # (8-c)
#***************
# Destination  #
#***************
# H0: β1=0
# H1: β1#0
mod1 <- lm(prix ~ dest, data=df8)
summary(mod1)
# Residuals:
#  Min      1Q     Median      3Q     Max 
# -50.260  -7.160   2.540   8.385  87.085 
# Coefficients:
#           Estimate Std. Error    t value   Pr(>|t|)    
# (Intercept)  82.1145     0.2373  345.967   <2e-16 ***
#   dest        0.4454     0.3342    1.333    0.183    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 15 on 8057 degrees of freedom
# Multiple R-squared:  0.0002204,	Adjusted R-squared:  9.628e-05 
# F-statistic: 1.776 on 1 and 8057 DF,  p-value: 0.1827

# On ne rejete pas H0, donc il n'y a pas d'effet significatif de la destination sur le prix.
#***********
#  classe  #
#***********
# H0: β2=0
# H1: β2#0
mod2 <- lm(prix ~ classe, data=df8)
summary(mod2)
# Residuals:
#    Min      1Q   Median      3Q     Max 
# -49.391  -8.041   2.309   6.479  73.838 
# Coefficients:
#                  Estimate Std.   Error   t value   Pr(>|t|)    
#   (Intercept)        95.3616     0.5184  183.944   <2e-16 ***
#   classeTurista     -16.7407     0.5514  -30.361   <2e-16 ***
#   classeTuristaPlus  -7.1706     0.6095  -11.764   <2e-16 ***
#   classeTuristaSolo  -7.4616     9.8297  -0.759     0.448    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 13.88 on 8055 degrees of freedom
# Multiple R-squared:  0.1441,	Adjusted R-squared:  0.1438 
# F-statistic: 452.2 on 3 and 8055 DF,  p-value: < 2.2e-16
!!!!!!!!!!!!
#***********
#   Duree  #
#***********
# H0: β3=0
# H1: β3#0
mod3 <- lm(prix ~ duree, data=df8)
summary(mod3)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -47.392  -7.897   1.803   9.726  83.199 
# Coefficients:
#           Estimate Std.  Error t   value   Pr(>|t|)    
# (Intercept) 113.03652    1.74146   64.91   <2e-16 ***
#   duree     -0.18024     0.01018  -17.71   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 14.72 on 8057 degrees of freedom
# Multiple R-squared:  0.03745,	Adjusted R-squared:  0.03733 
# F-statistic: 313.5 on 1 and 8057 DF,  p-value: < 2.2e-16

# On rejete H0, donc il y a un effet significatif de la duree sur le prix.
#***********
# weekend  #
#***********
# H0: β4=0
# H1: β4#0
mod4 <- lm(prix ~ weekend, data=df8)
summary(mod4)
# Residuals:
#     Min      1Q    Median      3Q     Max 
#   -50.435  -7.335   2.365   7.765  86.465 
# Coefficients:
#            Estimate Std. Error   t value  Pr(>|t|)    
# (Intercept)  82.7349     0.1929  428.938  < 2e-16 ***
#   weekend    -1.5779     0.3852   -4.097 4.23e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 14.99 on 8057 degrees of freedom
# Multiple R-squared:  0.002079,	Adjusted R-squared:  0.001955 
# F-statistic: 16.78 on 1 and 8057 DF,  p-value: 4.233e-05

# On rejete H0, donc il y a un effet significatif de la fin de semaine sur le prix.
#*************
# Le modele  #
#*************
mod <- lm(prix ~ dest + classe + duree + weekend, data=df8)
summary(mod)
# Residuals:
#   Min      1Q    Median      3Q     Max 
# -53.473  -7.450   1.841   9.225  68.610 
# Coefficients:
#                   Estimate Std.  Error    t value   Pr(>|t|)    
#   (Intercept)       135.341077   1.708457  79.218   <2e-16 ***
#   dest                0.433242   0.298252   1.453    0.146    
#   classeTurista     -17.764789   0.532803 -33.342   <2e-16 ***
#   classeTuristaPlus  -6.927631   0.587443 -11.793   <2e-16 ***
#   classeTuristaSolo  -9.069268   9.471460  -0.958    0.338    
#   duree              -0.231672   0.009464 -24.480   <2e-16 ***
#   weekend            -0.408880   0.347892  -1.175    0.240    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 13.37 on 8052 degrees of freedom
# Multiple R-squared:  0.206,	Adjusted R-squared:  0.2054 
# F-statistic: 348.1 on 6 and 8052 DF,  p-value: < 2.2e-16

par(mfrow = c(2,2)) 
plot(mod)



# prix = β0 + β1.dest + β2.classe + β3.duree + β4.weekend + E