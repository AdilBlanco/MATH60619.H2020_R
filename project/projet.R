options(scipen = 999)
library(dplyr)
# SalePrice : Prix de vente en (USD)
# LotArea : surface du lot (en pieds carrés)
# Bedrooms : nombre de chambres à coucher (excluant le sous-sol)
# GarageArea : surface du garage (en pieds carrés)
# OverallCond : score reflétant l’état global du logement et variant de plus qu’excellent (10) à très mauvais (1)
# OverallQual : score reflétant la qualité globale des matériaux utilisés ainsi que la finition du logement,
#               et variantde plus qu’excellent (10) à très mauvais (1)
# housetype : type de logement, soit individuel unifamilial (1FmCon), soit logement unifamilial converti en
#             bi-familial(2FmCon), soit duplex (Duplx), soit maison de ville (Twnhs)
# YearBuilt : année de construction
# YearRemodAdd : année de rénovation (même que année de construction si aucune modification ou rénovation n’a été effectuée)
# YrSold : année de vente du logement
# HalfBath : nombre de salles de bain partielles
# FullBath : nombre de salles de bain complètes
# Garage: variable binaire égale à 1 si le logement possède un garage

ameshousing <-
  read.csv("./MATH60619.H2020_R/project/datasets/ameshousing.csv")
head(ameshousing)
#   SalePrice LotArea Bedrooms GarageArea OverallCond OverallQual housetype YearBuilt YearRemodAdd YrSold HalfBath FullBath  Garage
# 1    208500    8450        3        548           5           7      1Fam      2003         2003   2008        1        3       0
# 2    181500    9600        3        460           8           6      1Fam      1976         1976   2007        1        2       0
# 3    223500   11250        3        608           5           7      1Fam      2001         2002   2008        1        3       0
# 4    140000    9550        3        642           5           7      1Fam      1915         1970   2006        0        2       0
# 5    250000   14260        4        836           5           8      1Fam      2000         2000   2008        1        3       0
# 6    143000   14115        1        480           5           5      1Fam      1993         1995   2009        1        2       0

dim(ameshousing)
# [1] 1460   13

sapply(ameshousing, class)
# SalePrice      LotArea     Bedrooms   GarageArea  OverallCond  OverallQual    housetype    YearBuilt
# "integer"    "integer"    "integer"    "integer"    "integer"    "integer"     "factor"    "integer"
# YearRemodAdd    YrSold     HalfBath     FullBath       Garage
# "integer"    "integer"    "integer"    "integer"    "integer"

# convert integer to numeric
ameshousing <- ameshousing %>% mutate_if(is.integer,as.numeric)
sapply(ameshousing, class)
# SalePrice      LotArea     Bedrooms   GarageArea  OverallCond  OverallQual    housetype    YearBuilt YearRemodAdd 
# "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric"     "factor"    "numeric"    "numeric" 
# YrSold     HalfBath     FullBath       Garage 
# "numeric"    "numeric"    "numeric"    "numeric" 

summary(ameshousing)
#    SalePrice         LotArea          Bedrooms       GarageArea      OverallCond     OverallQual      housetype
# Min.   : 34900   Min.   :  1300   Min.   :0.000   Min.   :   0.0   Min.   :1.000   Min.   : 1.000   1Fam  :1220
# 1st Qu.:129975   1st Qu.:  7554   1st Qu.:2.000   1st Qu.: 334.5   1st Qu.:5.000   1st Qu.: 5.000   2fmCon:  31
# Median :163000   Median :  9478   Median :3.000   Median : 480.0   Median :5.000   Median : 6.000   Duplex:  52
# Mean   :180921   Mean   : 10517   Mean   :2.866   Mean   : 473.0   Mean   :5.575   Mean   : 6.099   Twnhs :  43
# 3rd Qu.:214000   3rd Qu.: 11602   3rd Qu.:3.000   3rd Qu.: 576.0   3rd Qu.:6.000   3rd Qu.: 7.000   TwnhsE: 114
# Max.   :755000   Max.   :215245   Max.   :8.000   Max.   :1418.0   Max.   :9.000   Max.   :10.000
# YearBuilt     YearRemodAdd      YrSold        HalfBath         FullBath        Garage
# Min.   :1872   Min.   :1950   Min.   :2006   Min.   :0.0000   Min.   :0.00   Min.   :0.00000
# 1st Qu.:1954   1st Qu.:1967   1st Qu.:2007   1st Qu.:0.0000   1st Qu.:1.00   1st Qu.:0.00000
# Median :1973   Median :1994   Median :2008   Median :0.0000   Median :2.00   Median :0.00000
# Mean   :1971   Mean   :1985   Mean   :2008   Mean   :0.4404   Mean   :1.99   Mean   :0.05548
# 3rd Qu.:2000   3rd Qu.:2004   3rd Qu.:2009   3rd Qu.:1.0000   3rd Qu.:2.00   3rd Qu.:0.00000
# Max.   :2010   Max.   :2010   Max.   :2010   Max.   :4.0000   Max.   :6.00   Max.   :1.00000

nrow(ameshousing[ameshousing$YearBuilt==ameshousing$YrSold, ])         # new 64
nrow(ameshousing[(ameshousing$YearBuilt != ameshousing$YearRemodAdd) & 
                   (ameshousing$YearBuilt!=ameshousing$YrSold), ])     # remodled 695
nrow(ameshousing[(ameshousing$YearBuilt == ameshousing$YearRemodAdd) & 
                   (ameshousing$YearBuilt!=ameshousing$YrSold), ])     # Never remodled 701 

# create new column status (new house, old house never emdled & old house remodled )
ameshousing$status  <- with(ameshousing, ifelse(YearBuilt==YrSold, 
                                                "New", 
                                                ifelse((YearBuilt!=YearRemodAdd) & (YearBuilt!=YrSold), 
                                                       "Remodled", 
                                                          "NeverRemod"
                                                       )
                                                )
                            )

ameshousing %>% group_by(status) %>% summarise(n = n(), mean = mean(SalePrice))
# status         n    mean
#   <chr>      <int>   <dbl>
# 1 NeverRemod   701 175126.
# 2 New           64 264302.
# 3 Remodled     695 179088.

# add is remodled or not
# ameshousing$isRemod <-
#   with(ameshousing, ifelse(YearBuilt == YearRemodAdd, 0, 1))
# add is new or not
# ameshousing$isNew <-
#   with(ameshousing, ifelse(YearBuilt == YrSold, 1, 0))
# group Twnhs & TwnhsE
ameshousing$housetype[ameshousing$housetype == "TwnhsE"] <- "Twnhs"
# add house age column
ameshousing$HouseAge <- ameshousing$YrSold - ameshousing$YearBuilt
# add remodled age column
ameshousing$RemodAge <-
  ameshousing$YrSold - ameshousing$YearRemodAdd

# Aggregation
agg_Bedrooms <- ameshousing %>% group_by(Bedrooms) %>% summarise(n = n(), mean = mean(SalePrice))
xtabs(mean ~ Bedrooms, as.data.frame(agg_Bedrooms))
# Bedrooms
#        0        1        2        3        4        5        6        8 
# 221493.2 173162.4 158197.7 181056.9 220421.3 180819.0 143779.0 200000.0 
agg_OverallCond <- ameshousing %>% group_by(OverallCond) %>% summarise(n = n(), mean = mean(SalePrice))
xtabs(mean ~ OverallCond, as.data.frame(agg_OverallCond))
# OverallCond
#       1        2        3        4        5        6        7        8        9 
# 61000.0 141986.4 101929.4 120438.4 203146.9 153961.6 158145.5 155651.7 216004.5 
agg_OverallQual <- ameshousing %>% group_by(OverallQual) %>% summarise(n = n(), mean = mean(SalePrice))
xtabs(mean ~ OverallQual, as.data.frame(agg_OverallQual))
# OverallQual
#        1         2         3         4         5         6         7         8         9        10 
# 50150.00  51770.33  87473.75 108420.66 133523.35 161603.03 207716.42 274735.54 367513.02 438588.39  
agg_housetype <- ameshousing %>% group_by(housetype) %>% summarise(n = n(), mean = mean(SalePrice))
xtabs(mean ~ housetype, as.data.frame(agg_housetype))
# housetype
#     1Fam   2fmCon   Duplex    Twnhs 
# 185763.8 128432.3 133541.1 169347.5 
agg_YrSold <- ameshousing %>% group_by(YrSold) %>% summarise(n = n(), mean = mean(SalePrice))
xtabs(mean ~ YrSold, as.data.frame(agg_YrSold))
# YrSold
# 2006     2007     2008     2009     2010 
# 182549.5 186063.2 177360.8 179432.1 177393.7 
agg_HalfBath <- ameshousing %>% group_by(HalfBath) %>% summarise(n = n(), mean = mean(SalePrice))
xtabs(mean ~ HalfBath, as.data.frame(agg_HalfBath))
# HalfBath
#        0        1        2        3        4 
# 162489.8 207875.0 193360.2 170000.0 194201.0 
agg_FullBath <- ameshousing %>% group_by(FullBath) %>% summarise(n = n(), mean = mean(SalePrice))
xtabs(mean ~ FullBath, as.data.frame(agg_FullBath))
# FullBath
#        0        1        2        3        4        6 
# 194201.0 124160.1 174382.0 254481.2 319021.9 179000.0 
agg_Garage <- ameshousing %>% group_by(Garage) %>% summarise(n = n(), mean = mean(SalePrice))
xtabs(mean ~ Garage, as.data.frame(agg_Garage))
# Garage
#        0        1 
# 185479.5 103317.3 
agg_status <- ameshousing %>% group_by(status) %>% summarise(n = n(), mean = mean(SalePrice))
xtabs(mean ~ status, as.data.frame(agg_status))
# status
# NeverRemod      New   Remodled 
# 175126.0   264302.2   179088.2 
agg_YrSold_status <- ameshousing %>% group_by(status, YrSold) %>% summarise(n = n(), mean = mean(SalePrice))
tab <- xtabs(mean ~ status + YrSold, as.data.frame(agg_YrSold_status))
tab
# YrSold
# status         2006     2007     2008     2009     2010
# NeverRemod 170065.6 177366.7 171762.1 180335.4 175729.5
# New        249813.5 261336.0 321783.4 252351.1 394432.0
# Remodled   184707.8 183742.8 175967.7 174005.3 176614.0

status <- unique(as.vector(as.data.frame(tab)$status))
colors <- c("cornflowerblue", "lightgoldenrod", "lightpink")
par(mfrow = c(1, 1))
barplot(tab, main="New houses Distribution by YrSold & status",
        xlab="YrSold", ylab="mean(SalePrice)", col=colors, beside=TRUE)
legend("topleft", legend=status, col=colors, bg="white", lwd=2, cex = 0.6)

par(mfrow = c(2, 2))
plot(agg_YearBuilte$YearBuilt, agg_YearBuilte$mean, xaxt = 'n',
     col="cornflowerblue", lwd=1, xlab="YearBuilt", ylab="mean(SalePrice)", main="mean(SalePrice) by YearBuilt")
axis(side=1, at=c(1872, 1940, 2010), labels=c("1872", "1940", "2010"))

agg_YearRemodAdd <- ameshousing %>% group_by(YearRemodAdd) %>% summarise(n = n(), mean = mean(SalePrice))
plot(agg_YearRemodAdd$YearRemodAdd, agg_YearRemodAdd$mean, xaxt = 'n',
     col="cornflowerblue", lwd=1, xlab="YearRemodAdd", ylab="mean(SalePrice)", main="mean(SalePrice) by YearRemodAdd")
axis(side=1, at=c(1950, 1980, 2010), labels=c("1950", "1980", "2010"))

agg_YearBuilte <- ameshousing %>% group_by(YearBuilt) %>% summarise(n = n(), mean = mean(SalePrice))
plot(agg_YearBuilte$YearBuilt, agg_YearBuilte$mean, type="o", xaxt = 'n',
     col="cornflowerblue", lwd=1, xlab="YearBuilt", ylab="mean(SalePrice)", main="mean(SalePrice) by YearBuilt")
axis(side=1, at=c(1872, 1940, 2010), labels=c("1872", "1940", "2010"))

agg_YearRemodAdd <- ameshousing %>% group_by(YearRemodAdd) %>% summarise(n = n(), mean = mean(SalePrice))
plot(agg_YearRemodAdd$YearRemodAdd, agg_YearRemodAdd$mean, type="o", xaxt = 'n',
     col="cornflowerblue", lwd=1, xlab="YearRemodAdd", ylab="mean(SalePrice)", main="mean(SalePrice) by YearRemodAdd")
axis(side=1, at=c(1950, 1980, 2010), labels=c("1950", "1980", "2010"))

par(mfrow = c(1, 2))
agg_HouseAge <- ameshousing %>% group_by(HouseAge) %>% summarise(n = n(), mean = mean(SalePrice))
plot(agg_HouseAge$HouseAge, agg_HouseAge$mean,
     col="cornflowerblue", lwd=2, xlab="HouseAge", ylab="mean(SalePrice)", main="mean(SalePrice) by HouseAge")

agg_HouseAge <- ameshousing %>% group_by(HouseAge) %>% summarise(n = n(), mean = mean(SalePrice))
plot(agg_HouseAge$HouseAge, agg_HouseAge$mean, type="o", 
     col="cornflowerblue", lwd=2, xlab="HouseAge", ylab="mean(SalePrice)", main="mean(SalePrice) by HouseAge")

# par(mfrow = c(1, 2))
# agg_HouseAge_status <- ameshousing %>% group_by(status, HouseAge) %>% summarise(n = n(), mean = mean(SalePrice))
# v <- agg_HouseAge_status$mean[agg_HouseAge_status$status=="NeverRemod"]
# t <- agg_HouseAge_status$mean[agg_HouseAge_status$status=="New"]
# s <- agg_HouseAge_status$mean[agg_HouseAge_status$status=="Remodled"]
# plot(v, type="o", lwd=2, col = "red", xlab="HouseAge", xlim=c(0, 140), ylim=c(100000,460000), ylab="mean(SalePrice)", main="mean(SalePrice) by HouseAge")
# lines(t, type="o", lwd=2, col="blue")
# lines(s, type="o", lwd=2, col="green")

# plot_YrSold <- tapply(ameshousing$SalePrice, ameshousing$YrSold, FUN=mean)
# barplot(plot_YrSold, col="lightblue", main="mean(SalePrice) by YrSold", 
#         xlab="YrSold", ylab="mean(SalePrice)")

# histogram SalePrice
# par(mfrow = c(1, 1))
# hist(ameshousing$SalePrice, col="lightblue")
#abline(
#   v = mean(ameshousing$SalePrice),
#   col = "green",
#   lwd = 2,
#   lty = 2
# )
# abline(v = median(ameshousing$SalePrice), col = "red")

# correlation
par(mfrow = c(1, 1))
ameshousingCor <-
  cor(ameshousing[, sapply(ameshousing, class) %in% c("integer", "numeric")])
corrplot::corrplot(ameshousingCor, method = "number", type = "upper")

# plot SalePrice vs LotArea by housetype
# par(mfrow = c(1, 1))
# ameshousing$housetype <- factor(ameshousing$housetype)
# plot(
#   SalePrice ~ LotArea,
#   data = ameshousing,
#   col = c("red", "blue", "green", "black")[housetype],
#   pch = c(4, 4, 4, 4)[housetype]
# )
# legend(
#   x = "right",
#   legend = levels(ameshousing$housetype),
#   col = c("red", "blue", "green", "black"),
#   pch = c(4, 4, 4, 4),
#   cex = 0.75
# )

plot(SalePrice ~ LotArea, data=ameshousing, col="cornflowerblue")
plot(SalePrice ~ GarageArea, data=ameshousing, col="cornflowerblue")
plot(SalePrice ~ YearBuilt, data=ameshousing, col="cornflowerblue")
plot(SalePrice ~ YearRemodAdd, data=ameshousing, col="cornflowerblue")

plot(SalePrice ~ HouseAge, data = ameshousing) #redendance (YearBuilt), chech corelation heatmap
plot(SalePrice ~ RemodAge, data = ameshousing) #redendance (RemodAge), chech corelation heatmap

par(mfrow = c(2, 3))
# boxplot OverallCond
boxplot(
  SalePrice ~ OverallCond,
  data = ameshousing,
  boxwex = .3,
  staplewex = .6,
  col = "lightblue",
  xlab = "score",
  ylab = "SalePrice (USD)",
  main = "Score overall condition",
  frame = FALSE
)
# boxplot OverallQual
boxplot(
  SalePrice ~ OverallQual,
  data = ameshousing,
  boxwex = .3,
  staplewex = .6,
  col = "lightblue",
  xlab = "score",
  ylab = "SalePrice (USD)",
  main = "Score overall materials quality",
  frame = FALSE
)
# bxplot Garage
boxplot(
  SalePrice ~ Garage,
  data = ameshousing,
  boxwex = .3,
  staplewex = .6,
  col = "lightblue",
  xlab = "garage",
  ylab = "SalePrice (USD)",
  main = "House has a garage",
  frame = FALSE
)
# bxplot Remodled
boxplot(
  SalePrice ~ status,
  data = ameshousing,
  boxwex = .3,
  staplewex = .6,
  col = "lightblue",
  xlab = "status",
  ylab = "SalePrice (USD)",
  main = "House status",
  frame = FALSE
)
# bxplot HalfBath
boxplot(
  SalePrice ~ HalfBath,
  data = ameshousing,
  boxwex = .3,
  staplewex = .6,
  col = "lightblue",
  xlab = "partial bathrooms",
  ylab = "SalePrice (USD)",
  main = "Partial bathrooms",
  frame = FALSE
)
# bxplot FullBath
boxplot(
  SalePrice ~ FullBath,
  data = ameshousing,
  boxwex = .3,
  staplewex = .6,
  col = "lightblue",
  xlab = "full bathroom",
  ylab = "SalePrice (USD)",
  main = "Full bathrooms",
  frame = FALSE
)

# factorisation
# ameshousing$isRemod <- factor(ameshousing$isRemod)
# ameshousing$Garage <- factor(ameshousing$Garage)

# par(mfrow = c(1, 1))
# plot(ameshousing)

# par(mfrow = c(2, 2))
# plot(SalePrice ~ LotArea, data=ameshousing)
# plot(SalePrice ~ GarageArea, data=ameshousing)
# plot(SalePrice ~ YearBuilt, data=ameshousing)
# plot(SalePrice ~ YearRemodAdd, data=ameshousing)

# par(mfrow = c(1, 1))
# plot(SalePrice ~ HouseAgeReno, data=ameshousing, col=c("red","blue")[isRem], pch=c(19,4)[isRem])
# legend(x="topleft",  legend=levels(ameshousing$isRem), col=c("red","blue"), pch=c(19,4))

par(mfrow = c(1, 2),
    pch = 20,
    bty = 'l')
hist(
  ameshousing$SalePrice,
  freq = FALSE,
  col = "lightblue",
  xlab = "SalePrice (USD)",
  main = "SalePrice",
  ylab = "density"
)
lines(
  density(ameshousing$SalePrice),
  lty = 2,
  col = "black",
  lwd = 1
)
lines(curve(
  dnorm(
    x,
    mean = mean(ameshousing$SalePrice),
    sd = sd(ameshousing$SalePrice)
  ),
  from = 34900,
  to = 755000,
  add = TRUE
),
col = "red",
lwd = 2)

hist(
  log(ameshousing$SalePrice),
  freq = FALSE,
  col = "lightblue",
  xlab = "log(SalePrice) (USD)",
  main = "log(SalePrice)",
  ylab = "density"
)
lines(density(log(ameshousing$SalePrice)),
      lty = 2,
      col = "black",
      lwd = 1)
lines(curve(
  dnorm(x, mean = mean(log(
    ameshousing$SalePrice
  )),
  sd = sd(log(
    ameshousing$SalePrice
  ))),
  from = log(34900),
  to = log(755000),
  add = TRUE
),
col = "red",
lwd = 2)


colnames(ameshousing)
# [1] "SalePrice"    "LotArea"      "Bedrooms"     "GarageArea"   "OverallCond"  "OverallQual"  "housetype"   
# [8] "YearBuilt"    "YearRemodAdd" "YrSold"       "HalfBath"     "FullBath"     "Garage"       "status"      
# [15] "HouseAge"     "RemodAge"    

sapply(ameshousing, class)

ameshousing$housetype <- factor(ameshousing$housetype)
ameshousing$Garage <- factor(ameshousing$Garage)
ameshousing$status <- factor(ameshousing$status)
mod <- lm(SalePrice ~ LotArea+Bedrooms+GarageArea+OverallCond+OverallQual+housetype+YearBuilt+YearRemodAdd+
            YrSold+HalfBath+FullBath+Garage+status+HouseAge+RemodAge, data=ameshousing)
summary(mod)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -323350  -21959   -2274   17331  365646 
# Coefficients: (2 not defined because of singularities)
# Estimate   Std. Error t value             Pr(>|t|)    
# (Intercept)      626871.3229 1600240.5377   0.392             0.695311    
# LotArea               0.8748       0.1116   7.836  0.00000000000000898 ***
# Bedrooms           2195.1982    1494.4685   1.469             0.142083    
# GarageArea           81.9962       7.3502  11.156 < 0.0000000000000002 ***
# OverallCond        2744.8003    1132.0957   2.425             0.015450 *  
# OverallQual       29312.7253    1124.5008  26.067 < 0.0000000000000002 ***
# housetype2fmCon  -14846.6346    7626.0625  -1.947             0.051749 .  
# housetypeDuplex  -17907.0385    6053.6709  -2.958             0.003146 ** 
# housetypeTwnhs   -21210.7374    3907.2352  -5.429  0.00000006655482594 ***
# YearBuilt           243.0738      66.4043   3.661             0.000261 ***
# YearRemodAdd        -44.2235      80.0948  -0.552             0.580939    
# YrSold             -567.5281     797.2056  -0.712             0.476644    
# HalfBath          12344.0997    2058.8121   5.996  0.00000000255559340 ***
# FullBath          21574.9840    1900.1104  11.355 < 0.0000000000000002 ***
# Garage1           26166.9114    5691.1760   4.598  0.00000464299853296 ***
# statusNew         12542.8233    5539.8706   2.264             0.023716 *  
# statusRemodled    13954.3393    2655.7842   5.254  0.00000017085341819 ***
# HouseAge                  NA           NA      NA                   NA    
# RemodAge                  NA           NA      NA                   NA    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 39910 on 1443 degrees of freedom
# Multiple R-squared:  0.7503,	Adjusted R-squared:  0.7476 
# F-statistic:   271 on 16 and 1443 DF,  p-value: < 0.00000000000000022

anova(mod)
# Analysis of Variance Table
# Response: SalePrice
#                Df        Sum Sq       Mean Sq   F value                Pr(>F)    
# LotArea         1  640993235747  640993235747  402.3315 < 0.00000000000000022 ***
# Bedrooms        1  174398912096  174398912096  109.4648 < 0.00000000000000022 ***
# GarageArea      1 3096105112931 3096105112931 1943.3289 < 0.00000000000000022 ***
# OverallCond     1    1123280993    1123280993    0.7050             0.4012316    
# OverallQual     1 2536502784215 2536502784215 1592.0839 < 0.00000000000000022 ***
# housetype       3   32408277420   10802759140    6.7806             0.0001538 ***
# YearBuilt       1   67986275584   67986275584   42.6729      0.00000000008945 ***
# YearRemodAdd    1   28001540226   28001540226   17.5757      0.00002928808550 ***
# YrSold          1     288609829     288609829    0.1812             0.6704491    
# HalfBath        1   19552949928   19552949928   12.2728             0.0004736 ***
# FullBath        1  227135635226  227135635226  142.5660 < 0.00000000000000022 ***
# Garage          1   37977244554   37977244554   23.8371      0.00000116497751 ***
# status          2   46454638662   23227319331   14.5791      0.00000053890657 ***
# Residuals    1443 2298982837200    1593196699                                    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
mod



