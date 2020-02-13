library(ggplot2)
library(dplyr)
#*******************************************************************************************
#Q-1) Analyse exploratoire des données renf afin:                                          #                     
#     + d'évaluer graphiquement les facteurs déterminant les prix et le temps de parcours  #
#     + de déterminer les caractéristiques distinctives des types de train.                #
#     + d'établir s'il y a des diffèrences entre les différents tarifs                     #
#*******************************************************************************************
# fin de semaine 1 et 7
df <- read.csv("./MATH60619.H2020_R/devoir-1/renfe_fr.csv", header=TRUE)
df$is_weekend <- with(df, ifelse(jour %in% c(1, 7), 1, 0))
head(df, n=4)
#    prix type     classe    tarif dest duree jour is_weekend
# 1 143.4  AVE Preferente    Promo    0   190    6          1
# 2 181.5  AVE Preferente Flexible    0   190    2          0
# 3  86.8  AVE Preferente    Promo    0   165    7          1
# 4  86.8  AVE Preferente    Promo    0   190    7          1

dim(df)
# [1] 10000     8
colnames(df)
# [1] "prix"   "type"   "classe" "tarif"  "dest"   "duree"  "jour"  "is_weekend"

summary(df)
#      prix             type              classe           tarif           dest            duree      
# Min.   : 32.3   AVE     :9174   Preferente : 809   AdultoIda: 397   Min.   :0.0000   Min.   :150.0  
# 1st Qu.: 75.4   AVE-TGV : 429   Turista    :7197   Flexible :1544   1st Qu.:0.0000   1st Qu.:150.0  
# Median : 85.1   REXPRESS: 397   TuristaPlus:1916   Promo    :8059   Median :1.0000   Median :170.0  
# Mean   : 86.1                   TuristaSolo:  78                    Mean   :0.5062   Mean   :185.8  
# 3rd Qu.:100.4                                                       3rd Qu.:1.0000   3rd Qu.:190.0  
# Max.   :214.2                                                       Max.   :1.0000   Max.   :562.0 

type_percent <- df %>% group_by(type) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# type         n    freq
# <fct>      <int>  <dbl>
# 1 AVE       9174  0.917 
# 2 AVE-TGV    429  0.0429
# 3 REXPRESS   397  0.0397
classe_percent <- df %>% group_by(classe) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# classe          n    freq
# <fct>       <int>    <dbl>
# 1 Preferente    809  0.0809
# 2 Turista      7197  0.720 
# 3 TuristaPlus  1916  0.192 
# 4 TuristaSolo    78  0.0078
tarif_percent <- df %>% group_by(tarif) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# tarif         n    freq
# <fct>       <int>  <dbl>
# 1 AdultoIda   397  0.0397
# 2 Flexible   1544  0.154 
# 3 Promo      8059  0.806 
jour_percent <- df %>% group_by(is_weekend) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
#  is_weekend   n  freq
#     <dbl>  <int> <dbl>
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
        boxwex=.3,      # determines the width of the box  
        staplewex=.6,   # determines the width of the whisker
        col="lightblue", 
        xlab="type", 
        ylab="ticket price (Euro)",
        main="Trains types",
        frame=FALSE)
boxplot(prix ~ classe, 
        data=df,
        boxwex=.3,      # determines the width of the box  
        staplewex=.6,   # determines the width of the whisker
        col="lightblue", 
        xlab="class", 
        ylab="ticket price (Euro)",
        main="Trains classes",
        frame=FALSE)
boxplot(prix ~ tarif, 
        data=df,
        boxwex=.3,      # determines the width of the box  
        staplewex=.6,   # determines the width of the whisker
        col="lightblue", 
        xlab="tarif", 
        ylab="ticket price (Euro)",
        main="Trains tarifs",
        frame=FALSE)
boxplot(prix ~ dest, 
        data=df,
        boxwex=.3,      # determines the width of the box  
        staplewex=.6,   # determines the width of the whisker
        col="lightblue", 
        xlab="destination", 
        ylab="ticket price (Euro)",
        main="Trains destinations",
        frame=FALSE)
boxplot(prix ~ jour,
        data=df,
        boxwex=.3,      # determines the width of the box  
        staplewex=.6,   # determines the width of the whisker
        col="lightblue", 
        xlab="day", 
        ylab="ticket price (Euro)",
        main="Trains days",
        frame=FALSE)
boxplot(prix ~ is_weekend,
        data=df,
        boxwex=.3,      # determines the width of the box  
        staplewex=.6,   # determines the width of the whisker
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
        boxwex=.4,      # determines the width of the box  
        staplewex=.6,   # determines the width of the whisker
        col="lightblue", 
        xlab="type", 
        ylab="travel time (minute)",
        main="Trains types",
        frame=FALSE)
boxplot(duree ~ classe,
        data=df,
        boxwex=.4,      # determines the width of the box  
        staplewex=.6,   # determines the width of the whisker
        col="lightblue", 
        xlab="class", 
        ylab="travel time (minute)",
        main="Trains classes",
        frame=FALSE)
boxplot(duree ~ tarif,
        data=df,
        boxwex=.4,      # determines the width of the box  
        staplewex=.6,   # determines the width of the whisker
        col="lightblue", 
        xlab="tarif", 
        ylab="travel time (minute)",
        main="Trains tarifs",
        frame=FALSE)
boxplot(duree ~ dest,
        data=df,
        boxwex=.4,      # determines the width of the box  
        staplewex=.6,   # determines the width of the whisker
        col="lightblue", 
        xlab="destination", 
        ylab="travel time (minute)",
        main="Trains destinations",
        frame=FALSE)
boxplot(duree ~ jour,
        data=df,
        boxwex=.4,      # determines the width of the box  
        staplewex=.6,   # determines the width of the whisker
        col="lightblue", 
        xlab="day", 
        ylab="travel time (minute)",
        main="Trains days",
        frame=FALSE)
boxplot(prix ~ is_weekend,
        data=df,
        boxwex=.4,      # determines the width of the box  
        staplewex=.6,   # determines the width of the whisker
        col="lightblue", 
        xlab="is_wekend", 
        ylab="travel time (minute)",
        main="Trains weekends",
        frame=FALSE)

# pairs(~prix + type + classe + tarif + dest + duree + jour, data=df, main="Simple Scatterplot Matrix")

# ave <- df[df$type=="AVE", ]
# tgv <- df[df$type=="AVE-TGV", ]
# exp <- df[df$type=="REXPRESS", ]

# summary(ave)
#          prix         type          classe            tarif             duree      
# Min.   : 32.30   AVE     :9174   Preferente : 779   AdultoIda:   0   Min.   :150.0  
# 1st Qu.: 75.40   AVE-TGV :   0   Turista    :6501   Flexible :1446   1st Qu.:150.0  
# Median : 85.10   REXPRESS:   0   TuristaPlus:1817   Promo    :7728   Median :167.0  
# Mean   : 87.82                   TuristaSolo:  77                    Mean   :170.4  
# 3rd Qu.:100.40                                                       3rd Qu.:190.0  
# Max.   :214.20                                                       Max.   :194.0  
# summary(tgv)
# prix              type             classe             tarif             duree      
# Min.   : 40.95   AVE     :  0    Preferente : 30    AdultoIda:  0   Min.   :175.0  
# 1st Qu.: 75.40   AVE-TGV :429    Turista    :299    Flexible : 98   1st Qu.:175.0  
# Median : 85.10   REXPRESS:  0    TuristaPlus: 99    Promo    :331   Median :179.0  
# Mean   : 88.88                   TuristaSolo:  1                    Mean   :177.1  
# 3rd Qu.:102.15                                                      3rd Qu.:179.0  
# Max.   :181.50                                                      Max.   :179.0  
# summary(exp)
#       prix             type        classe             tarif            duree    
# Min.   :43.25   AVE     :  0     Preferente :  0    AdultoIda:397   Min.   :544  
# 1st Qu.:43.25   AVE-TGV :  0     Turista    :397    Flexible :  0   1st Qu.:544  
# Median :43.25   REXPRESS:397     TuristaPlus:  0    Promo    :  0   Median :544  
# Mean   :43.25                    TuristaSolo:  0                    Mean   :552  
# 3rd Qu.:43.25                                                       3rd Qu.:562  
# Max.   :43.25                                                       Max.   :562

# ptyp_clas_agg <- aggregate(prix ~ type + classe, data=df, length)
#       type      classe prix
# 1      AVE  Preferente  779
# 2  AVE-TGV  Preferente   30
# 3      AVE     Turista 6501
# 4  AVE-TGV     Turista  299
# 5 REXPRESS     Turista  397
# 6      AVE TuristaPlus 1817
# 7  AVE-TGV TuristaPlus   99
# 8      AVE TuristaSolo   77
# 9  AVE-TGV TuristaSolo    1
# ptyp_tar_agg <- aggregate(prix ~ type + tarif, data=df, length)
#       type     tarif prix
# 1 REXPRESS AdultoIda  397
# 2      AVE  Flexible 1446
# 3  AVE-TGV  Flexible   98
# 4      AVE     Promo 7728
# 5  AVE-TGV     Promo  331
# dtyp_clas_agg <- aggregate(duree ~ type + classe, data=df, mean)
#       type      classe    duree
# 1      AVE  Preferente 173.3030
# 2  AVE-TGV  Preferente 176.7333
# 3      AVE     Turista 169.0075
# 4  AVE-TGV     Turista 176.8863
# 5 REXPRESS     Turista 552.0252
# 6      AVE TuristaPlus 173.8569
# 7  AVE-TGV TuristaPlus 177.6667
# 8      AVE TuristaSolo 175.5974
# 9  AVE-TGV TuristaSolo 175.0000
# dtyp_tar_agg <- aggregate(duree ~ type + tarif, data=df, mean)
#       type     tarif    duree
# 1 REXPRESS AdultoIda 552.0252
# 2      AVE  Flexible 172.3071
# 3  AVE-TGV  Flexible 177.2857
# 4      AVE     Promo 170.0290
# 5  AVE-TGV     Promo 176.9819

# p1 <- ggplot2::ggplot(data=ptyp_clas_agg, aes(x=type, fill=classe))
# p1 <- p1 + geom_bar(aes(y=(..count..)/sum(..count..))) + theme_light()
# p2 <- ggplot2::ggplot(data=ptyp_tar_agg, aes(x=type, fill=tarif))
# p2 <- p2 + geom_bar(aes(y=(..count..)/sum(..count..))) + theme_light()
# gridExtra::grid.arrange(p1, p2, ncol=2, nrow=2)
# p3 <- ggplot2::ggplot(data=dtyp_clas_agg, aes(x=type, fill=classe))
# p3 <- p3 + geom_bar(aes(y=duree), stat = "identity") + theme_light()
# p4 <- ggplot2::ggplot(data=dtyp_tar_agg, aes(x=type, fill=tarif))
# p4 <- p4 + geom_bar(aes(y=(..count..)/sum(..count..))) + theme_light()
# gridExtra::grid.arrange(p3, p4, ncol=2, nrow=2)

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

# Non ça ne serait pas un bon échantillon vu l'abscence des classes "TuristaPlus" et "TuristaSolo" 
# ainsi l'échantillon contient que les trajets Barcelone vers Madrid (0).

#*************************************************************************************************************
#Q-4) Comparaison les tarifs moyennes pour les train à grande vitesse pour les deux déstinations (simulation)#
#*************************************************************************************************************

df4 <- read.csv("./MATH60619.H2020_R/devoir-1/renfe_simu_fr.csv", header=TRUE)
# df4 <- df # copy dataframe
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

# select AVE-TGV and REXPRESS train
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

# H0: µ0 == 43.25 (hypthèse null)
# Ha: µ0 <> 43.25  (hypthèse alternative)
# + ou µ0 est le prix moyen du billet pour un train de type AVE-TGV.
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

# La valeur-p du test bilatéral de test-t pour un échantillon simple est 2.2e-16.
# On rejette l'hypothèse nulle, donc le prix moyen de ticket pour un train AVE-TGV 
# n'est pas le même que RESXPRESS.
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

shapiro.test(entrants$prix)
# Shapiro-Wilk normality test
# data:  entrants$prix
# W = 0.92469, p-value < 2.2e-16
shapiro.test(sortants$prix)
# Shapiro-Wilk normality test
# data:  sortants$prix
# W = 0.92412, p-value < 2.2e-16

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

# H0: var(entrants) == var(sortants)
# Ha: var(entrants) <> var(sortants)
# + var(entrants) est la variance de la destination madrid->barcelone (1)
#   et var(sortants) est la variance de la destination bqrcelone->madrid (0) 
# La valeur-p du test bilatéral de test-F pour les deux échantillons est 0.0002293.
# On rejette l'hypothèse nulle, donc la variance de la destination madrid->barcelone
# est différente que la variance de la destination barcelone->madrid à niveau de 5%.

# H0: µ0 == µ1 (hypthèse null)
# Ha: µ0 <> µ1 (hypthèse alternative)
# + ou µ0 est le prix moyen du billet pour un train de grand vitesse allant 
#   de barcelone à madrid et µ1 est le prix moyen du billet pour un train de 
#   grand vitesse allant de madrid à barcelone.

# fp.test(entrants$prix, sortants$prix, delta = 0, alternative = "two.sided")
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

# La valeur-p du test bilatéral de test Welch pour deux échantillons est 0.02156.
# On rejette l'hypothèse nulle, donc le prix moyen d'une direction est plus chère
# que l'autre à niveau de 5% pour le trajet Madrid-Barcelone et celui de Barcelone-Madrid.

wilcox.test(prix ~ dest, data=df6)
# Wilcoxon rank sum test with continuity correction
# data:  prix by dest
# W = 11184249, p-value = 0.01208
# alternative hypothesis: true location shift is not equal to 0

#********************************************************************************************************
#Q-7) Est-ce que le prix la fin de semaine est plus chère que les jours semaine pour les trains AVE-TGV #
#********************************************************************************************************

df7 <- df[df$type=="AVE-TGV", ]
# df$is_weekend <- with(df, ifelse(jour %in% c(1, 7), 1, 0))
weekend <- df7[df7$is_weekend==1, ]
weekday <- df7[df7$is_weekend==0, ]
# df7$is_weekend <- with(df7, ifelse(jour %in% c(1, 7), 1, 0))
head(df7, n=5)
#       prix    type     classe tarif dest duree jour is_weekend
# 5    68.95 AVE-TGV Preferente Promo    0   175    4          0
# 90   98.00 AVE-TGV Preferente Promo    0   175    1          0
# 100  98.00 AVE-TGV Preferente Promo    0   175    7          1
# 101 112.55 AVE-TGV Preferente Promo    0   175    1          0
# 109  98.00 AVE-TGV Preferente Promo    0   175    7          1

# x <- rnorm(100, mean = 5, sd = 3)
# y <- runif(100, min = 2, max = 4)
# par(mfrow=c(2,2), pch=20, bty='l')
# hist(x)
# qqnorm(x, pch=1)
# hist(y)
# qqnorm(y, pch=1)
# shapiro.test(x)
# shapiro.test(y)
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
# # histogram plot weekend ticket prices.
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

# H0: µ0 == µ1 ~ µ0 - µ1 = 0 (hypthèse null)
# Ha: µ0 > µ1 ~  µ0 - µ1 > 0 (hypthèse alternative)
# + ou µ1 est le prix moyen du billet pour les trains AVE-TGV en jours de la semaine
#   et µ0 est le prix moyen du billet pour les trains AVE-TGV en fin de semaine. 
wilcox.test(prix ~ is_weekend, data=df7, alternative="greater")
# Wilcoxon rank sum test with continuity correction
# data:  prix by is_weekend
# W = 14922, p-value = 0.9997
# alternative hypothesis: true location shift is greater than 0

# La valeur-p du test unilatéral de Wilcoxon pour deux échantillons est 0.9997
# On ne rejette pas l'hypothèse nulle, donc le prix moyen du billet pour les trains AVE-TGV
# en fin de semaine n'est plus chère que les jours de la semiane à niveau de 5%.

#********************************************************************************************************
#Q-8) Expliquer le prix des billets 'Promo' pour les trains à grande vitesse en fonction de destination, 
#     classe, duree et une variable additionelle indiquant si le jour est une fin de semaine ou pas.
#********************************************************************************************************

df8 <- df[(df$type=="AVE" | df$type=="AVE-TGV") & (df$tarif=="Promo"), ]
df8$is_weekend <- with(df8, ifelse(jour %in% c(6, 7), 1, 0))
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
df8$classe <- factor(df8$classe)
mod <- lm(prix ~ dest + classe + duree + is_weekend, data=df8)
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

# prix_^ = 135.34 + 0.44.dest_^ + (-17.76 - 6.93 - 9.07).classe_^ - 0.23.duree_^ - 0.41.weekend_^

# dest (binaire): En fixant toutes les autres co-variables, toutes choses étant égales, 
#                 le prix moyen du trajet Madrid->Barcelone augmente de 0.44 par rapport 
#                 au trajet Barcelone->Madrid.
# duree (continue): En fixant toutes les autres co-variables, toutes choses étant égales, 
#                   une augmentation de la duree d'une minute implique une dimunition de 0.41 minutes
#                   du prix en moyenne.
# weekend (binaire): En fixant toutes les autres co-variables, toutes choses étant égales,
#                    le prix moyen en fin de semaine diminue de 0.41 par rappot au jour semaine.
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
mod <- lm(prix ~ dest + classe + duree + is_weekend, data=df8)
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
# 1) Risiduals vs Fitted (Linéarité - indépendnce): 
# Les résidus ne suivent pas une forme quadratique, les résidus sont bien éparpillés autour de la ligne.
# 2) Normal Q-Q (normalité):
# Les résidus sont bien alignés sur la ligne droite. 
# 3) Scale-Location (homoscédasticité):
# La variance devrait être constante
# La ligne horizontale avec des points répartis également.!?
# 4) Residuals vs Leverage: 
# Notre graphique ne montre aucun cas influent, car aucuns des points se trouve 
# en dehors de la distance de Cook.