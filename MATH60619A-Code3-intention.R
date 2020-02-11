url <- "https://lbelzile.bitbucket.io/MATH60619A/intention.txt"
intention <- read.table(file = url, header = TRUE)

#the package car has a function avPlot to produce the added variable plot (diagramme de régression partielle)
car::avPlot(lm0 <-
              lm(intention~fixation+emotion+factor(educ)+age+factor(revenue)+marital,
                 data = intention), variable = "fixation")

#Below, is how to reproduce the plot by explicitely computing the residuals of both models
lm1 <- lm(intention~emotion+factor(educ)+age+factor(revenue)+marital,
          data = intention)
lm2 <- lm(fixation~emotion+factor(educ)+age+factor(revenue)+marital,
          data = intention)

par(mfrow=c(1,1))
plot(resid(lm1)~resid(lm2), xlab = "fixation | other", ylab = "intention | other", bty = "l")
abline(lm(resid(lm1) ~ resid(lm2)), lwd=2 , col=2)
abline(a=0, b= coef(lm0)[2], lty=2, lwd=2, col=3)
  