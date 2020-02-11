# Exercice 4.1
url <- "https://lbelzile.bitbucket.io/MATH60619A/intention.sas7bdat"
intention <- haven::read_sas(url)
intention$educ <- factor(intention$educ)
intention$revenue <- factor(intention$revenue, ordered = FALSE)
intention$revenue <- relevel(intention$revenue, ref = 3)

linmod1 <- coef(lm(intention ~ revenue, data = intention))
linmod2 <- coef(lm(intention ~ I(revenue == 1) + I(revenue ==2), data = intention))
linmod3 <- coef(lm(intention ~ as.numeric(revenue), data = intention))
linmod1
linmod2
linmod3

linmod4 <- lm(intention ~ fixation + emotion + revenue + educ + age + marital + sex, data = intention)
summary(linmod4)
anova(linmod4)

# Exercise 4.2
url <- "https://lbelzile.bitbucket.io/MATH60619A/auto.csv"
auto <- read.csv(url, header = TRUE)

linmod1 <- lm(mpg ~ horsepower, data = auto)
linmod2 <- lm(mpg ~ horsepower + I(horsepower^2), data = auto)
linmod3 <- lm(mpg ~ horsepower + I(horsepower^2) + I(horsepower^3), data = auto)
hps <- seq(from = 40, to = 250, length.out = 100L)
p1 <- predict(object = linmod1, 
              newdata = data.frame(horsepower=hps),
              interval = "prediction")
p2 <- predict(object = linmod2, 
              newdata = data.frame(horsepower=hps),
              interval = "prediction")
p3 <- predict(object = linmod3, 
              newdata = data.frame(horsepower=hps),
              interval = "prediction")
pdf("Exercise4_2.pdf", width = 8, height = 6)
par(mfrow = c(2,2), # grid of plots (2 x 2)
    mar = c(4,5.5,1,1), # margins for text
    pch = 20, # type of points (small bullet)
    bty = "l") # remove box
with(auto, plot(mpg ~ horsepower,
                col = scales::alpha("black", 0.2),
                ylab = "fuel economy \n(in miles per gallon)"))
matplot(hps, p1, 
        col = "red", 
        add = TRUE, 
        type = "l", 
        lwd = 2,
        lty = c(1,2,2))
matplot(hps, p2, 
      col = "green", 
      add = TRUE, 
      type = "l", 
      lwd = 2,
      lty = c(1,2,2))
matplot(hps, p3, 
      col = "blue", 
      add = TRUE, 
      type = "l", 
      lwd = 2,
      lty = c(1,2,2))
plot(resid(linmod1)~fitted(linmod1),
     xlab = "fitted values", 
     ylab = "residuals")
abline(h=0)
plot(resid(linmod2)~fitted(linmod2),
     xlab = "fitted values", 
     ylab = "residuals") 
abline(h=0)
plot(resid(linmod3)~fitted(linmod3),
     xlab = "fitted values", 
     ylab = "residuals")
abline(h=0)
dev.off()
# test if the cubic term is significant
summary(linmod3)
# the quadratic model is the most adequate
car::qqPlot(rstudent(linmod2), 
            ylab = "jackknife studentized residuals")
plot(abs(rstudent(linmod2)) ~ fitted(linmod2),
     xlab = "fitted values", 
     ylab = "jackknife studentized residuals")
     

# Exercice 4.3
url <- "https://lbelzile.bitbucket.io/MATH60619A/intention.sas7bdat"
intention <- haven::read_sas(url)
intention$educ <- factor(intention$educ, ordered = FALSE)
intention$educ <- relevel(intention$educ, ref = 3)
modmain <- lm(intention ~ fixation + educ, data = intention)
summary(modmain)
modinter <- lm(intention ~ fixation * educ, data = intention)
anova(modinter)
coef(modinter)

library(ggplot2)
intention$pred <- fitted(modmain)
pdf("Exercise4_3.pdf", width = 8, height = 4)
par(bty = "l", pch = 20, mfrow = c(1,2), mar = c(4,4,0.1,0.1))
plot(intention ~ fixation, 
        col = c(2:4)[as.numeric(educ)], 
        pch =c(1,2,5)[as.numeric(educ)], data = intention)
abline(a = 6.92, b = 1.09, col = 2)
abline(a = 6.93, b = 1.09, col = 3, lty = 2)
abline(a = 5.53, b = 1.09, col = 4, lty = 3)

plot(intention ~ fixation, 
     col = c(2:4)[as.numeric(educ)], 
     pch =c(1,2,5)[as.numeric(educ)], data = intention)
abline(a = 6.03, b = 1.64, col = 2)
abline(a = 7.91, b = 0.5, col = 3, lty = 2)
abline(a = 4.59, b = 1.75, col = 4, lty = 3)
dev.off()
