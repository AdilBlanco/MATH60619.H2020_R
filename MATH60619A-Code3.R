# MATH60619A Statistical Analysis and Inference
# Chapter 3 - Linear Regression

#  define colors 
hecblue <- rgb(red = 0, green = 60, blue = 113, max = 255)
heccyan <- rgb(red = 0, green = 159, blue = 223, max = 255)

# fetch data from website
url <- "https://lbelzile.bitbucket.io/MATH60619A/intention.txt"
intention <- read.table(file = url, header = TRUE)
# Cast categorical variables to factors
intention$educ <- factor(intention$educ)
intention$revenue <- factor(intention$revenue)
intention$marital <- factor(intention$marital)

str(intention)
# Mean of each variable
colMeans(intention)
# Intention to buy (discrete response)
with(intention, 
     plot(x = sort(unique(intention)), 
          y = table(intention), 
          type = "h",
          xlab = "intention to buy",
          ylab = 'counts')
)
# Table of counts for each factor
table(intention$revenue)
table(intention$educ)
# Histograms of continuous covariates
par(mfrow = c(1,3), bty = "l")
with(intention, {
   hist(age, freq = FALSE, main = "")
   # lines(density(age))
   hist(emotion, freq = FALSE, main = "")
   hist(fixation, freq = FALSE, main = "")})

par(mfrow = c(1,2), pch = 20)

plot(intention ~ fixation, data = intention, 
     xlab = "fixation time (in seconds)")
abline(lm(intention ~ fixation, data = intention))
plot(intention ~ emotion, data = intention, 
     xlab = "emotion")
abline(lm(intention ~ emotion, data = intention))

# Alternative plots using ggplot2 (grammar of graphics)
library(ggplot2)
ols <- lm(intention ~ fixation, data = intention)
fitted <- fitted(ols)
res <- resid(ols)
vlines <- data.frame(x1 = intention$fixation, y1 = fitted, y2 = fitted + res)
ggg <- ggplot2::ggplot(data = intention, aes(x = fixation, y = intention)) + 
   geom_point() +  geom_smooth(method = "lm", se = FALSE, col ="black") +
   labs(title = "", 
        x = "fixation time (in seconds)", 
        y = "buying intention")+
   geom_segment(aes(x = x1, y = y1, xend = x1, yend = y2), color = hecblue, 
                data = vlines, show.legend = FALSE) +
labs(title = "Buying intention following the viewing of a candy advertisment", 
     x = "fixation duration (in seconds)", 
     y = "buying intention")
print(ggg)

# Fit linear model with lm function
# Get coefficient table with "summary"
# Get F-table of SS3 with "anova"
summary(lm(intention ~ fixation, data = intention))

# Compare two-sample t-test with linear regression
summary(lm(intention ~ sex, data = intention))
t.test(intention ~ sex, data = intention, var.equal = TRUE)
# Same p-values, different parametrization

modlin <- lm(intention ~ fixation + emotion + educ + 
             age + revenue + marital, 
          data = intention)
summary(modlin) # includes R2, global F-test and Wald tests for individual parameters
anova(modlin) # F-tests


################################
#####   Diagnostic plots   #####
################################

# Default diagnostic plots
par(mfrow = c(2, 2))
plot(modlin)
dev.off()
# Added variable plots - perform linear regressions manually
lm1 <- lm(intention ~ emotion + educ + age + revenue + marital,data = intention)
lm2 <- lm(fixation ~ emotion + educ + age + revenue + marital, data = intention)
par(mfrow = c(1,1))
plot(resid(lm1) ~ resid(lm2), xlab = "fixation | other", ylab = "intention | other", bty = "l")
abline(lm(resid(lm1) ~ resid(lm2)), lwd = 2, col = 2)
abline(a = 0, b = coef(modlin)[2], lty = 2, lwd = 2, col = "blue")


# Manually create diagnostics
library(car)
# Residuals versus fitted and explanatories
car::residualPlots(modlin)
qqPlot(rstudent(modlin),
       distribution = "t", 
       df = modlin$df.residual-1, 
       ylab = "externally studentized residuals")
plot(abs(rstudent(modlin)) ~ fitted(modlin),
     ylab = "|externally studentized residuals|",
     xlab = "fitted values")
car::gamLine(y = abs(rstudent(modlin)), x= fitted(modlin), spread = TRUE)
#the package car has a function avPlot to produce the added variable plot 
car::avPlot(modlin, variable = "fixation")
car::avPlot(modlin, variable = "emotion")



### model with an interaction between revenue and fixation
lm_interac <- lm(intention ~ fixation * factor(revenue),
                 data = intention)
anova(lm_interac) # interaction term is not significant

# Bixi multicollinearity example
bixi <- read.table("https://lbelzile.bitbucket.io/MATH60619A/bixidat.txt", header = TRUE)
# Table of coefficients
summary(lm(lognuser ~ farenheit, data = bixi))
summary(lm(lognuser ~ celcius, data = bixi))
summary(lm(lognuser ~ celcius + farenheit, data = bixi))
summary(bixi_col <- lm(lognuser ~ celcius + rfarenheit, data = bixi))
# Variance inflation factor
car::vif(bixi_col)
car::avPlots(lm(lognuser~ celcius + rfarenheit, data = bixi), id= FALSE,
             pch = 20, main = "", ylab = "log rentals | others")

###########################
#####   Predictions   #####
###########################

### confidence interval for the mean
newdat <- intention[1,]
newdat <- newdat[rep(1,7), ]
newdat$fixation <- 0:6
pred.modlin.conf <- predict(object = modlin, newdata = newdat,
                            se.fit = TRUE,
                            interval = "confidence")

### prediction intervals
pred.modlin.pred <- predict(modlin, newdata = newdat,            
                            se.fit = TRUE,
                            interval = "prediction")

plot(newdat$fixation, pred.modlin.pred$fit[,"fit"], type = "l", 
     ylim = range(pred.modlin.pred$fit),
     xlab = "fixation time (in seconds)",
     ylab = "buying intention score", lwd=2,
     xlim = c(0, 6))
lines(newdat$fixation, pred.modlin.pred$fit[,"lwr"], lty = 2, lwd = 2)
lines(newdat$fixation, pred.modlin.pred$fit[,"upr"], lty = 2, lwd = 2)

lines(newdat$fixation, pred.modlin.conf$fit[,"fit"], col = "grey", lwd = 2, lty = 3)
lines(newdat$fixation, pred.modlin.conf$fit[,"lwr"], col = "grey", lty = 2, lwd = 2)
lines(newdat$fixation, pred.modlin.conf$fit[,"upr"], col = "grey", lty = 2, lwd = 2)

rug(intention$fixation)

      ################################################################
###               Analysis of the insurance data             ###
################################################################

insurance <- read.csv("https://lbelzile.bitbucket.io/MATH60619A/insurance.csv", 
                      header = TRUE, 
                      stringsAsFactors = TRUE)
summary(insurance)

dwidth <- 6
dheight <- 3.5
attach(insurance)
obese <- factor(I(bmi>30), 
                labels = c("normal","obese"))
nsregion <- factor(I(region %in% c("northwest","northeast")), 
                   labels = c("south", "north"))
smokstat <- I(smoker == "yes") + I(obese == "obese")*I(smoker == "yes") + 1


# Exploratory data analysis
par(mfrow = c(3, 2), mar = c(4,4,1,1))
hist(charges, 
     breaks = 30, 
     xlab = "health costs (in USD)",
     main = "", 
     ylab = "frequency")
rug(charges)
boxplot(charges ~ region, 
        xlab = "region", 
        ylab = "health costs (in USD)",
        frame = FALSE)
# Question 2
# Clearly unequal variance, smokers pay higher health costs
boxplot(charges ~ smoker, 
        ylab = "health costs (in USD)",
        frame = FALSE)

# Heteroscedasticity and existence of subgroups
plot(charges ~ age, 
     col = scales::alpha(smokstat, 0.5),
     bty = "l",
     pch = 20,
     ylab = "health costs in (USD)",
     xlab = "age (in years)")
plot(charges ~ bmi, 
     col = scales::alpha(I(smoker == "yes") + 1, 0.5), 
     pch = 20,
     bty = "l",
     ylab = "health costs in (USD)",
     xlab = "body mass index (in kg/m${}^2$)")
abline(v = 30)

smokstat <- factor(smokstat, 
                   labels = c("non-smoker/bmi$ < 30$", "smoker/bmi$ < 30$", "smoker/bmi$ \\geq 30$"))
boxplot(charges ~ smokstat, 
        ylab = "health costs in (USD)",
        frame = FALSE, 
        xlab = "Smoker and weight categories")



par(mfrow = c(2,2), mar = c(4,4,1,1))
hist(bmi, 
     freq = FALSE,
     main = "",
     xlab = "body mass index (in kg/m${}^2$)",
     breaks = floor(nrow(insurance)/30))
rug(bmi)
lines(xbmi <- seq(min(bmi), max(bmi), length = 100L),
      dnorm(xbmi, mean = mean(bmi), sd = sd(bmi)),
      col = 2)
boxplot(bmi ~ smoker, 
        ylab = "body mass index (in kg/m${}^2$)",
        frame = FALSE)
boxplot(bmi ~ sex,
        ylab = "body mass index (in kg/m${}^2$)",
        xlab = "gender",
        frame = FALSE)
boxplot(bmi ~ region,
        ylab = "body mass index (in kg/m${}^2$)",
        frame = FALSE)

### health cost for smokers vs health cost for non smokers
par(mfrow=c(1,2))

#smoker
hist(insurance$charges[insurance$smoker=="yes"], freq=FALSE, col=rgb(1,0,0,0.5),
     xlab="health costs in (USD)",  ylab="density", ylim=c(0,5e-5))
lines(density(insurance$charges[insurance$smoker=="yes"]), lty=2,col="red", lwd=2)
lines(curve(dnorm(x, mean= mean(insurance$charges[insurance$smoker=="yes"]), 
                  sd=sd(insurance$charges[insurance$smoker=="yes"])), from=0, to=60000, add=TRUE), col="red", lwd=2)

#non smoker
hist(insurance$charges[insurance$smoker=="no"], freq=FALSE, col=rgb(0,0,1,0.5),
     xlab="health costs in (USD)",  ylab="density", ylim=c(0,8e-5))
lines(density(insurance$charges[insurance$smoker=="no"]), lty=2,col="blue", lwd=2)
lines(curve(dnorm(x, mean= mean(insurance$charges[insurance$smoker=="no"]), 
                  sd=sd(insurance$charges[insurance$smoker=="no"])), from=0, to=60000, add=TRUE), col="blue", lwd=2)

### BMI for women vs for men
par(mfrow=c(1,2))

#women
hist(insurance$bmi[insurance$sex=="female"], freq=FALSE, col=rgb(1,0,0,0.5),
     xlab="body mass index (in kg/m${}^2$)",  ylab="density", ylim=c(0,0.07))
lines(density(insurance$bmi[insurance$sex=="female"]), lty=2,col="red", lwd=2)
lines(curve(dnorm(x, mean= mean(insurance$bmi[insurance$sex=="female"]), 
                  sd=sd(insurance$bmi[insurance$sex=="female"])), from=10, to=60, add=TRUE), col="red", lwd=2)

#men
hist(insurance$bmi[insurance$sex=="male"], freq=FALSE, col=rgb(0,0,1,0.5),
     xlab="body mass index (in kg/m${}^2$)",  ylab="density", ylim=c(0,0.07))
lines(density(insurance$bmi[insurance$sex=="male"]), lty=2,col="blue", lwd=2)
lines(curve(dnorm(x, mean= mean(insurance$bmi[insurance$sex=="male"]), 
                  sd=sd(insurance$bmi[insurance$sex=="male"])), from=10, to=60, add=TRUE), col="blue", lwd=2)

### Pearson correlation 
par(mfrow=c(1,1))

#correlation between charges and age
plot(cbind(age,charges))
cor(cbind(age,charges))

#correlation between charges and age, for those who do not smoke and are not obese
plot(cbind(age,charges)[which(smokstat==levels(smokstat)[1]),])
cor(cbind(age,charges)[which(smokstat==levels(smokstat)[1]),])

#correlation between charges and age, for those who smoke and are not obese
plot(cbind(age,charges)[which(smokstat==levels(smokstat)[2]),])
cor(cbind(age,charges)[which(smokstat==levels(smokstat)[2]),])

#correlation between charges and age, for those who smoke and are obese
plot(cbind(age,charges)[which(smokstat==levels(smokstat)[3]),])
cor(cbind(age,charges)[which(smokstat==levels(smokstat)[3]),])


attach(insurance)
### Linear regression model

mod0 <- lm(charges ~ age)
#linear model to explain charges with age (only)
summary(mod0) # summary of the fitted model
confint(mod0) # confidence intervals for the linear effects
plot(mod0) # different diagnostic plots for the fitted model with age

mod1 <- lm(charges ~ age + factor(nsregion) + factor(smokstat))
#linear model to explain charges with age, region (north or south), and the smoksat variable
summary(mod1) # summary of the fitted model
confint(mod1) # confidence intervals for the linear effects
plot(mod1) # different diagnostic plots for the fitted model with age

### Linear regression model with interaction
obese <- factor(I(bmi>30), 
                labels = c("normal","obese"))

mod.interac <- lm(charges ~ as.factor(smoker)+as.factor(obese)+as.factor(smoker)*as.factor(obese))

# obese2 with three levels
obese2 <- I((bmi >= 25)&(bmi<30))+ 2*I(bmi>=30)+1
obese2 <- factor(obese2, 
                 labels = c("normal","overweight","obese"))

mod.interac2 <- lm(charges ~ as.factor(smoker)+as.factor(obese2)+as.factor(smoker)*as.factor(obese2))

