
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

