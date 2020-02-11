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


# Q1 EDA
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
     xlab="health costs in (USD)", main="Histogramme", ylab="Densit?", ylim=c(0,5e-5))
lines(density(insurance$charges[insurance$smoker=="yes"]), lty=2,col="red", lwd=2)
lines(curve(dnorm(x, mean= mean(insurance$charges[insurance$smoker=="yes"]), 
                  sd=sd(insurance$charges[insurance$smoker=="yes"])), from=0, to=60000, add=TRUE), col="red", lwd=2)

#non smoker
hist(insurance$charges[insurance$smoker=="no"], freq=FALSE, col=rgb(0,0,1,0.5),
     xlab="health costs in (USD)", main="Histogramme", ylab="Densit?", ylim=c(0,8e-5))
lines(density(insurance$charges[insurance$smoker=="no"]), lty=2,col="blue", lwd=2)
lines(curve(dnorm(x, mean= mean(insurance$charges[insurance$smoker=="no"]), 
                  sd=sd(insurance$charges[insurance$smoker=="no"])), from=0, to=60000, add=TRUE), col="blue", lwd=2)

### BMI for women vs for men
par(mfrow=c(1,2))

#women
hist(insurance$bmi[insurance$sex=="female"], freq=FALSE, col=rgb(1,0,0,0.5),
     xlab="body mass index (in kg/m${}^2$)", main="Histogramme", ylab="Densit?", ylim=c(0,0.07))
lines(density(insurance$bmi[insurance$sex=="female"]), lty=2,col="red", lwd=2)
lines(curve(dnorm(x, mean= mean(insurance$bmi[insurance$sex=="female"]), 
                  sd=sd(insurance$bmi[insurance$sex=="female"])), from=10, to=60, add=TRUE), col="red", lwd=2)

#men
hist(insurance$bmi[insurance$sex=="male"], freq=FALSE, col=rgb(0,0,1,0.5),
     xlab="body mass index (in kg/m${}^2$)", main="Histogramme", ylab="Densit?", ylim=c(0,0.07))
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

