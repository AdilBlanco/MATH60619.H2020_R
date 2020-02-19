library(dplyr)
#*******************************************************************************************
#Q-1) Analyse exploratoire des données renf afin:                                          #                     
#     + d'évaluer graphiquement les facteurs déterminant les prix et le temps de parcours  #
#     + de déterminer les caractéristiques distinctives des types de train.                #
#     + d'établir s'il y a des diffèrences entre les différents tarifs                     #
#*******************************************************************************************
df <- read.csv("./renfe_fr.csv", header=TRUE)
df$is_weekend <- with(df, ifelse(jour %in% c(1, 7), 1, 0))
head(df, n=4)
#    prix type     classe    tarif dest duree jour is_weekend
# 1 143.4  AVE Preferente    Promo    0   190    6          1
# 2 181.5  AVE Preferente Flexible    0   190    2          0
# 3  86.8  AVE Preferente    Promo    0   165    7          1
# 4  86.8  AVE Preferente    Promo    0   190    7          1

summary(df)
#      prix             type              classe           tarif           dest            duree      
# Min.   : 32.3   AVE     :9174   Preferente : 809   AdultoIda: 397   Min.   :0.0000   Min.   :150.0  
# 1st Qu.: 75.4   AVE-TGV : 429   Turista    :7197   Flexible :1544   1st Qu.:0.0000   1st Qu.:150.0  
# Median : 85.1   REXPRESS: 397   TuristaPlus:1916   Promo    :8059   Median :1.0000   Median :170.0  
# Mean   : 86.1                   TuristaSolo:  78                    Mean   :0.5062   Mean   :185.8  
# 3rd Qu.:100.4                                                       3rd Qu.:1.0000   3rd Qu.:190.0  
# Max.   :214.2                                                       Max.   :1.0000   Max.   :562.0 

expr_df <- df[df$type=="REXPRESS", ]
table(expr_df$prix, expr_df$type)
#        AVE AVE-TGV REXPRESS
# 43.25   0       0      397

type_percent <- df %>% group_by(type) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# type         n    freq
# 1 AVE       9174  0.917 
# 2 AVE-TGV    429  0.0429
# 3 REXPRESS   397  0.0397
classe_percent <- df %>% group_by(classe) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# classe          n    freq
# 1 Preferente    809  0.0809
# 2 Turista      7197  0.720 
# 3 TuristaPlus  1916  0.192 
# 4 TuristaSolo    78  0.0078
tarif_percent <- df %>% group_by(tarif) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# tarif         n    freq
# 1 AdultoIda   397  0.0397
# 2 Flexible   1544  0.154 
# 3 Promo      8059  0.806 
jour_percent <- df %>% group_by(is_weekend) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
#  is_weekend   n  freq
# 1      0   7479  0.748
# 2      1   2521  0.252
par(mfrow=c(2, 2))
colors <- c("aquamarine", "antiquewhite", "cornflowerblue")
# type
pie(type_percent$freq, 
    labels=type_percent$freq*100, 
    radius=1.2, 
    col=colors, 
    main="Type proportion") 
legend("topright", 
       legend=type_percent$type, 
       fill=colors, 
       adj = c(0, 0), cex=0.4)
# classe
pie(classe_percent$freq, 
    labels=classe_percent$freq*100, 
    radius = 1.2, 
    col=colors,
    main="Classe proportion") 
legend("topright", 
       legend=classe_percent$classe, 
       fill=colors, 
       adj = c(0, 0), cex=0.4)
# tarif
pie(tarif_percent$freq, 
    labels=tarif_percent$freq*100, 
    radius = 1.2, 
    col=colors,
    main="Tarif proportion") 
legend("topright", 
       legend=tarif_percent$tarif, 
       fill=colors, 
       adj = c(0, 0), cex=0.4)
# jour
pie(jour_percent$freq, 
    labels=jour_percent$freq*100, 
    radius = 1.2, 
    col=colors,
    main="Day proportion") 
legend("topright", 
       legend=jour_percent$is_weekend, 
       fill=colors,
       adj = c(0, 0), cex=0.4)
#*******************************
# Facteurs déterminant le prix #
#*******************************
par(mfrow=c(3, 2))
boxplot(prix ~ type, 
        data=df,
        boxwex=.3,
        staplewex=.6,
        col="lightblue", 
        xlab="type", 
        ylab="ticket price (Euro)",
        main="Trains types",
        frame=FALSE)
boxplot(prix ~ classe, 
        data=df,
        boxwex=.3,
        staplewex=.6,
        col="lightblue", 
        xlab="class", 
        ylab="ticket price (Euro)",
        main="Trains classes",
        frame=FALSE)
boxplot(prix ~ tarif, 
        data=df,
        boxwex=.3,
        staplewex=.6,
        col="lightblue", 
        xlab="tarif", 
        ylab="ticket price (Euro)",
        main="Trains tarifs",
        frame=FALSE)
boxplot(prix ~ dest, 
        data=df,
        boxwex=.3,
        staplewex=.6,
        col="lightblue", 
        xlab="destination", 
        ylab="ticket price (Euro)",
        main="Trains destinations",
        frame=FALSE)
boxplot(prix ~ jour,
        data=df,
        boxwex=.3,
        staplewex=.6,
        col="lightblue", 
        xlab="day", 
        ylab="ticket price (Euro)",
        main="Trains days",
        frame=FALSE)
boxplot(prix ~ is_weekend,
        data=df,
        boxwex=.3,
        staplewex=.6,
        col="lightblue", 
        xlab="is_wekend", 
        ylab="ticket price (Euro)",
        main="Trains weekends",
        frame=FALSE)
#********************************************
# Facteurs déterminant le temps de parcours #
#********************************************
par(mfrow=c(3, 2))
boxplot(duree ~ type, 
        data=df,
        boxwex=.4,
        staplewex=.6,
        col="lightblue", 
        xlab="type", 
        ylab="travel time (minute)",
        main="Trains types",
        frame=FALSE)
boxplot(duree ~ classe,
        data=df,
        boxwex=.4,
        staplewex=.6,
        col="lightblue", 
        xlab="class", 
        ylab="travel time (minute)",
        main="Trains classes",
        frame=FALSE)
boxplot(duree ~ tarif,
        data=df,
        boxwex=.4,
        staplewex=.6,
        col="lightblue", 
        xlab="tarif", 
        ylab="travel time (minute)",
        main="Trains tarifs",
        frame=FALSE)
boxplot(duree ~ dest,
        data=df,
        boxwex=.4,
        staplewex=.6,
        col="lightblue", 
        xlab="destination", 
        ylab="travel time (minute)",
        main="Trains destinations",
        frame=FALSE)
boxplot(duree ~ jour,
        data=df,
        boxwex=.4,
        staplewex=.6,
        col="lightblue", 
        xlab="day", 
        ylab="travel time (minute)",
        main="Trains days",
        frame=FALSE)
boxplot(prix ~ is_weekend,
        data=df,
        boxwex=.4,
        staplewex=.6,
        col="lightblue", 
        xlab="is_wekend", 
        ylab="travel time (minute)",
        main="Trains weekends",
        frame=FALSE)

#******************************************************************************************************
#Q-2) Est-ce que ce serait un bon échantillion si on conserve seulement les 1000 première observations #
#******************************************************************************************************
df1k <- df[1:1000,]
summary(df1k)
#       prix              type             classe          tarif          dest       duree      
# Min.   : 40.95   AVE     :921   Preferente :420   AdultoIda: 29   Min.   :0   Min.   :150.0  
# 1st Qu.: 75.40   AVE-TGV : 50   Turista    :580   Flexible :147   1st Qu.:0   1st Qu.:165.0  
# Median : 88.95   REXPRESS: 29   TuristaPlus:  0   Promo    :824   Median :0   Median :170.0  
# Mean   : 92.02                  TuristaSolo:  0                   Mean   :0   Mean   :182.9  
# 3rd Qu.:100.40                                                    3rd Qu.:0   3rd Qu.:190.0  
# Max.   :214.20                                                    Max.   :0   Max.   :544.0  

#******************************************************************************************************
#Q-3) L'histogramme du temps de parcours pour expliquer les valeurs inattendue et le diagramme        # 
#     Quantile-Quantile pour expliquer la robustesse du test-t.                                       #
#******************************************************************************************************
df3 <- df[df$type!="REXPRESS", ]

# d)
# histogram plot duration by hour for AVE-TGV and AVE trains.
par(mfrow=c(1,2), pch=20, bty='l')
hist(df3$duree/60, freq=FALSE, col="lightblue",
     xlab="duration (hour)", main="AVE-TGV/AVE duration Histogram", ylab="density")
lines(density(df3$duree/60), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(df3$duree/60), 
                  sd=sd(df3$duree/60)), from=20, to=200, add=TRUE), col="red", lwd=2)
abline(v=2.833,col="red")

# e)
# Normal Quantile-Q plot duration by hour for AVE-TGV and AVE trains.
qqnorm(df3$duree/60, pch=1, main="AVE-TGV/AVE duration Q-Q plot", frame=FALSE)
qqline(df3$duree/60, col="steelblue", lwd=2)

#*************************************************************************************************************
#Q-4) Comparaison les tarifs moyennes pour les train à grande vitesse pour les deux déstinations (simulation)#
#*************************************************************************************************************
df4 <- read.csv("./renfe_simu_fr.csv", header=TRUE)
head(df4, n=4)
#     difmoy     statW        icbi       icbs       valp
# 1 -0.4818434 -1.174404 -1.28609052 0.32240363 0.24026152
# 2  0.8749708  2.173267  0.08578143 1.66416017 0.02978365
# 3 -0.7291113 -1.824994 -1.51223959 0.05401705 0.06803169
# 4 -0.5833163 -1.448819 -1.37252279 0.20589020 0.14741949

# a) 
(nrow(df4[(df4$icbi < -0.28) & (df4$icbs > -0.28), ]) / nrow(df4)) * 100
# [1] 94.7

# b)
par(mfrow=c(1,1), pch=20, bty='l')
hist(df4$difmoy, 
     col="cornflowerblue",
     xlab="means differences",
     ylab="frequency" ,
     main="Means differences Histogram")
abline(v=-0.28,col="red")

# c)
(nrow(df4[df4$valp < 0.05, ]) / nrow(df4)) * 100
# [1] 10.5

#**************************************************************************************************
#Q-5) Est-ce que le prix moyen du billet pour un train de classe AVE-TGV est le même que REXPRESS #
#**************************************************************************************************
df5 <- df[df$type!="AVE", ]
ave_tgv <- df[df$type=="AVE-TGV", ]

par(mfrow=c(2,2), pch=20, bty='l')
# histogram plot ticket prices for AVE-TGV train.
hist(ave_tgv$prix, freq=FALSE, col="lightblue",
     xlab="ticket price (Euro)", main="AVE-TGV Histogram", ylab="density")
lines(density(ave_tgv$prix), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(ave_tgv$prix), 
                  sd=sd(ave_tgv$prix)), from=20, to=200, add=TRUE), col="red", lwd=2)
# Normal Quantile-Q plot ticket prices for AVE-TGV train.
qqnorm(ave_tgv$prix, pch=1, main="AVE-TGV Normal Q-Q plot", frame=FALSE)
qqline(ave_tgv$prix, col="steelblue", lwd=2)
# Box plot ticket prices for AVE-TGV / REXPRESS train
boxplot(df5$prix ~ df5$type, 
        horizontal=TRUE, 
        col="lightblue", 
        xlab="ticket price (Euro)", 
        ylab="type",
        main="AVE-TGV / REXPRESS",
        frame=FALSE)

mean(df[df$type=="REXPRESS", ]$prix)
# [1] 43.25

t.test(ave_tgv$prix, data=ave_tgv, alternative='two.sided', mu=43.25, conf.level=0.9)
# 	One Sample t-test
# data:  ave_tgv$prix
# t = 50.519, df = 428, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 43.25
# 90 percent confidence interval:
#         87.39137 90.36918
# sample estimates:
#         mean of x 
# 88.88028 

#*****************************************************************************************************
#Q-6) Est-ce que le prix d'une direction est plus chère que l'autre pour les trains à grande vitesse #
#*****************************************************************************************************
df6 <- df[df$type=="AVE" | df$type=="AVE-TGV", ]
entrants <- df6[df6$dest==1, ] # madrid->barcelone
sortants <- df6[df6$dest==0, ] # barcelone->madrid
#***************************
#  Normality verification  #
#***************************  
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
var.test(entrants$prix, sortants$prix, alternative="two.sided")
# F test to compare two variances
# data:  entrants$prix and sortants$prix
# F = 1.1123, num df = 4884, denom df = 4717, p-value = 0.0002293
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#         1.051092 1.177052
# sample estimates:
#         ratio of variances 
# 1.112307 
t.test(prix ~ dest, data=df6, var.equal=F, alternative='two.sided')
# 	Welch Two Sample t-test
# data:  prix by dest
# t = -2.2984, df = 9597.7, p-value = 0.02156
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#     -1.756087 -0.139458
# sample estimates:
#     mean in group 0 mean in group 1 
#       87.38419        88.33197
fp.test(prix ~ dest, data=df6, alternative='two.sided')
# 	Fligner-Policello test
# data:  prix by dest
# U* = 2.5177, p-value = 0.01181
# alternative hypothesis: true difference in location is not equal to 0

#********************************************************************************************************
#Q-7) Est-ce que le prix la fin de semaine est plus chère que les jours semaine pour les trains AVE-TGV #
#********************************************************************************************************
df7 <- df[df$type=="AVE-TGV", ]
weekend <- df7[df7$is_weekend==1, ]
weekday <- df7[df7$is_weekend==0, ]
#***************************
#  Normality verification  #
#***************************
par(mfrow=c(3,2), pch=20, bty='l')
# histogram plot weekdays ticket prices.
hist(weekday$prix, freq=FALSE, col="lightblue",
     xlab="ticket price (Euro)", main="Weekday Histogram", ylab="density")
lines(density(weekday$prix), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(weekday$prix), 
                  sd=sd(weekday$prix)), from=20, to=200, add=TRUE), col="red", lwd=2)
# histogram plot weekend ticket prices.
hist(weekend$prix, freq=FALSE, col="lightblue",
     xlab="ticket price (Euro)", main="Weekend Histogram", ylab="density")
lines(density(weekend$prix), lty=2,col="black", lwd=1)
lines(curve(dnorm(x, mean= mean(weekend$prix), 
                  sd=sd(weekend$prix)), from=20, to=200, add=TRUE), col="red", lwd=2)
# Normal Qiantile-Q plot weekdays ticket prices.
qqnorm(weekday$prix, pch=1, main="Weekday Normal Q-Q plot", frame=FALSE)
qqline(weekday$prix, col="steelblue", lwd=2)
# Normal Qiantile-Q plot weekend ticket prices.
qqnorm(weekend$prix, pch=1, main="Weekend Normal Q-Q plot", frame=FALSE)
qqline(weekend$prix, col="steelblue", lwd=2)
# Box plot ticket prices for group weekday and weekend
boxplot(df7$prix ~ df7$is_weekend, 
        horizontal=TRUE, 
        col="lightblue", 
        xlab="ticket price (Euro)", 
        ylab="is weekend",
        main="Weekday-weekend prices",
        frame=FALSE)
#**********************************
#  homoscedasticity verification  #
#**********************************
var.test(weekend$prix, weekday$prix, alternative="two.sided")
# 	F test to compare two variances
# data:  weekend$prix and weekday$prix
# F = 0.59801, num df = 114, denom df = 313, p-value = 0.001603
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.4459097 0.8197092
# sample estimates:
#   ratio of variances 
# 0.5980118 
fp.test(prix ~ is_weekend, data=df7, alternative="greater")
# 	Fligner-Policello test
# data:  prix by is_weekend
# U* = 3.2382, p-value = 0.0006024
# alternative hypothesis: true difference in location is greater than 0

#********************************************************************************************************
#Q-8) Expliquer le prix des billets 'Promo' pour les trains à grande vitesse en fonction de destination, 
#     classe, duree et une variable additionelle indiquant si le jour est une fin de semaine ou pas.
#********************************************************************************************************
df8 <- df[(df$type=="AVE" | df$type=="AVE-TGV") & (df$tarif=="Promo"), ]
head(df8, n=5)

# b)
mod <- lm(prix ~ dest + classe + duree + is_weekend, data=df8)
summary(mod)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -53.231  -7.782   1.862   9.050  68.991 
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)       135.972720   1.708307  79.595  < 2e-16 ***
#  dest                0.455052   0.297959   1.527  0.12674    
#  classeTurista     -17.589537   0.535832 -32.827  < 2e-16 ***
#  classeTuristaPlus  -6.747759   0.589976 -11.437  < 2e-16 ***
#  classeTuristaSolo  -8.592175   9.467670  -0.908  0.36415    
#  duree              -0.238421   0.009508 -25.075  < 2e-16 ***
#  is_weekend          1.080843   0.365161   2.960  0.00309 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ‘ ’ 1
# Residual standard error: 13.37 on 8052 degrees of freedom
# Multiple R-squared:  0.2067,	Adjusted R-squared:  0.2061 
# F-statistic: 349.7 on 6 and 8052 DF,  p-value: < 2.2e-16

# c)
#*******************
# Le modele global #
#*******************
mod <- lm(prix ~ dest + classe + duree + is_weekend, data=df8)
anova(mod)
# Analysis of Variance Table
# Response: prix
#               Df  Sum Sq Mean Sq  F value    Pr(>F)    
#  dest          1     400     400   2.2367  0.134809    
#  classe        3  261667   87222 488.1248 < 2.2e-16 ***
#  duree         1  111239  111239 622.5282 < 2.2e-16 ***
#  is_weekend    1    1565    1565   8.7611  0.003086 ** 
#  Residuals  8052 1438798     179                       
# ---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# d)
par(mfrow = c(2,2)) 
plot(mod)