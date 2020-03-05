options(scipen = 999)
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

# add is new or not (binary)
ameshousing$isRemod <-
  with(ameshousing, ifelse(YearBuilt == YearRemodAdd, 0, 1))
# group Twnhs & TwnhsE
ameshousing$housetype[ameshousing$housetype == "TwnhsE"] <- "Twnhs"
# add house age column
ameshousing$HouseAge <- ameshousing$YrSold - ameshousing$YearBuilt
# add remodled age column
ameshousing$RemodAge <-
  ameshousing$YrSold - ameshousing$YearRemodAdd

# histogram SalePrice
par(mfrow = c(1, 1))
hist(ameshousing$SalePrice, col = "lightblue")
abline(
  v = mean(ameshousing$SalePrice),
  col = "green",
  lwd = 2,
  lty = 2
)
abline(v = median(ameshousing$SalePrice), col = "red")

# correlation
par(mfrow = c(1, 1))
ameshousingCor <-
  cor(ameshousing[, sapply(ameshousing, class) %in% c("integer", "numeric")])
corrplot::corrplot(ameshousingCor, method = "number", type = "upper")

# plot SalePrice vs LotArea by housetype
par(mfrow = c(1, 1))
ameshousing$housetype <- factor(ameshousing$housetype)
plot(
  SalePrice ~ LotArea,
  data = ameshousing,
  col = c("red", "blue", "green", "black")[housetype],
  pch = c(4, 4, 4, 4)[housetype]
)
legend(
  x = "right",
  legend = levels(ameshousing$housetype),
  col = c("red", "blue", "green", "black"),
  pch = c(4, 4, 4, 4),
  cex = 0.75
)

plot(SalePrice ~ LotArea, data = ameshousing)
plot(SalePrice ~ GarageArea, data = ameshousing)
plot(SalePrice ~ YearBuilt, data = ameshousing)
plot(SalePrice ~ YearRemodAdd, data = ameshousing)
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
  SalePrice ~ isRemod,
  data = ameshousing,
  boxwex = .3,
  staplewex = .6,
  col = "lightblue",
  xlab = "remodled",
  ylab = "SalePrice (USD)",
  main = "House is remodled",
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
