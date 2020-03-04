
# Use pdf to create a plot
# Set dimensions for plots (in inches)
dwidth <- 6
dheight <- 4

# Exercise 2.1
# Load 'ticket' data
url <- "https://lbelzile.bitbucket.io/math60619A/tickets.txt"
tickets <- read.table(url, header = TRUE)
# Cast binary variable to factor
tickets$group <- as.factor(tickets$group)
# Add meaningful labels to the indicator
levels(tickets$group) <- c("cash", "credit")

#Default in class
ttest_tickets <- t.test(offer ~ group, 
                        data = tickets, 
                        alternative = "less")
#
ttest_tickets <- t.test(offer ~ group, 
                        data = tickets, 
                        alternative = "two.sided")
# Exercise 2.2
# Print first five observations
head(tickets, n = 5)
# Copy dataset
tickets_mod <- tickets
# Replace observation by 180
tickets_mod[1,1] <- 180 
# Same works with command
#tickets_mod$offer[1] <- 180
# Run Levene's test (equality of variance)
var.test(offer ~ group, data = tickets)
var.test(offer ~ group, data = tickets_mod)
# p-value goes from 5.595e-09 to 0.8482 because of outlier
# with original data, variance clearly different
# for the modified, there is not enough evidence - variance is huge

# Run t-test (Welch by default unless)
ttest_ori <- t.test(offer ~ group, data = tickets)
ttest_mod <- t.test(offer ~ group, data = tickets_mod)
# Rather than print the whole, just output the specific values
ttest_ori$p.value
ttest_mod$p.value # much larger, do not reject at level 5%
# Same for confidence interval
ttest_ori$conf.int
ttest_mod$conf.int #includes zero

# The added variability decreases the power of the test
#pdf("E2p2.pdf", width = dwidth, height = dheight)
# One could run the normal t-test by imputing with "var.equal = TRUE"
par(mfrow = c(1,2), mar = c(4,4,1,1)) #change margins,
# mfrow = c(1,2) gives two plots side by side
boxplot(offer ~ group,data = tickets_mod, 
        main = "Box-and-whiskers plot", 
        ylab = "Amount offered (in dollars)",
        frame = FALSE)
# Fancy graphs - package ggplot2 needed
# install.packages("ggplot2")
# library(ggplot2)
# print(ggplot(tickets, aes(x = offer, fill = group)) +
#    geom_histogram(aes(color = group), alpha = 0.5,
#                   position = "identity", bins = 10))

# Histogram using default R plot
# Attach dataset - variables (columns) are now visible
# avoids having tickets_mod$group everywhere, now group
attach(tickets_mod)
hist(x = offer[group == "cash"], 
     breaks = 10,
     xlim = range(offer),
     xlab = "Amount offered (in dollars)",
     main = "Histogram",
     col = rgb(1, 0, 0, 0.5))
#Add the second group to the plot
hist(x = offer[group == "credit"], 
     breaks = 10,
     add = TRUE,
     col = rgb(0, 0, 1, 0.5))
legend(x = "topright", 
       col = c("red", "blue"), 
       legend = c("cash", "credit"), 
       bty = "n",
       lty = 1)
# Detach dataset (don't forget)
detach(tickets_mod)
dev.off()
# Warning message for Wilcoxon because of ties
# so ranks are not uniquely defined
wilcox.test(offer ~ group, data = tickets)
wilcox.test(offer ~ group, data = tickets_mod)
# The test is much less sensitive to outliers


# Exercice 2.3
# Load 'food' data
url <- "https://lbelzile.bitbucket.io/math60619a/food.txt"
food <- read.table(url, header = TRUE)
# Cast variables to factor
food$color <- as.factor(food$color)
food$balance <- as.factor(food$balance)
# Add more meaningful labels
levels(food$color) <-  c("monochrome", "colored")
levels(food$balance) <-  c("balanced", "unbalanced")
attach(food)
# Compute mean and variance
tapply(attractiveness, INDEX = color, FUN = mean)
tapply(attractiveness, INDEX = color, FUN = var)
# mean seem different, but variance too large to conclude anything
# t-test will have no power, idem for variance test     
var.test(attractiveness ~ color) #cannot reject null
ttest <- t.test(attractiveness ~ color, var.equal = TRUE)
ttest$conf.int

# split graphical console
#pdf("E2p3.pdf", width = dwidth, height = dheight)
par(mfrow = c(1, 2), mar = c(4,4,1,1), pch = 20)
# standardize using (x - mean)/std.dev
qqnorm(scale(attractiveness[color == 'monochrome']),
       sub = "attractiveness score (monochrome)",
       panel.first = abline(a = 0, b = 1),
       main = "")

qqnorm(scale(attractiveness[color == 'colored']),
       sub = "attractiveness score (colored)",
       panel.first = abline(a = 0, b = 1),
       main = "")
dev.off()
# Unstandardized Q-Q plot
# line passes through first and third quartile
# (more robust, but harder to make comparisons)
qqnorm(attractiveness[color == 'colored'],
       sub = "attractiveness score (colored)")
qqline(attractiveness[color == 'colored'])

# Now same thing with Wilcoxon-Mann-Whitney test
wilcox.test(attractiveness ~ color)
# almost same p-value, same conclusions

detach(food)

# Exercise 2.4
insurance <- read.csv("https://lbelzile.bitbucket.io/math60619a/insurance.csv", 
                      header = TRUE, 
                      stringsAsFactors = TRUE)

summary(insurance)
attach(insurance)
# Create binary variables for obesity, region, and smoking status 
# i.e., (non smoking, smoking obese, smoking non-obese)
obese <- factor(I(bmi >= 30), 
                labels = c("normal","obese"))
nsregion <- factor(I(region %in% c("northwest","northeast")), 
                   labels = c("south", "north"))
smokstat <- I(smoker == "yes") + I(obese == "obese")*I(smoker == "yes") + 1
southeast <- factor(I(region == "southeast"), labels = c("other", "southeast"))

# Q2.4(a) EDA
par(mfrow = c(3, 2), mar = c(4,4,1,1))
hist(charges, 
     breaks = 30, 
     xlab = "medical costs (in USD)",
     main = "", 
     ylab = "frequency")
rug(charges)
boxplot(charges ~ region, 
        xlab = "region", 
        ylab = "medical costs (in USD)",
        frame = FALSE)
# Question 2.4(a)
# Clearly unequal variance, smokers pay higher medical costs
boxplot(charges ~ smoker, 
        ylab = "medical costs (in USD)",
        frame = FALSE)

# Heteroscedasticity and existence of subgroups
plot(charges ~ age, 
     col = scales::alpha(smokstat, 0.5),
     bty = "l",
     pch = 20,
     ylab = "medical costs in (USD)",
     xlab = "age (in years)")
plot(charges ~ bmi, 
     col = scales::alpha(I(smoker == "yes") + 1, 0.5), 
     pch = 20,
     bty = "l",
     ylab = "medical costs in (USD)",
     xlab = "body mass index (in kg/m${}^2$)")
abline(v = 30)

smokstat <- factor(smokstat, 
                   labels = c("non-smoker/bmi$ < 30$", "smoker/bmi$ < 30$", "smoker/bmi$ \\geq 30$"))
boxplot(charges ~ smokstat, 
        ylab = "medical costs in (USD)",
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



#Q2.4(b)
# Two-sample t-test - two-sided alternative hypothesis
var.test(charges ~ smoker, data = insurance)
# Welch test
t.test(charges ~ smoker, data = insurance)

#Q2.4(c)
var.test(charges ~ obese, 
         subset = smoker == "yes",
         data = insurance)
# label is TRUE for obese, so alternative is <
t.test(charges ~ obese, 
       data = insurance, 
       subset = smoker == "yes",
       alternative = "less", 
       var.equal = FALSE, 
       conf.level = 0.9)
t.test(charges ~ obese, 
       data = insurance, 
       subset = smoker == "yes",
       alternative = "less", 
       var.equal = FALSE, 
       conf.level = 0.95)$conf.int
t.test(charges ~ obese, 
       data = insurance, 
       subset = smoker == "yes",
       alternative = "less", 
       var.equal = FALSE, 
       conf.level = 0.99)$conf.int

# Q2.4(d)
var.test(bmi ~ sex, data = insurance)
t.test(bmi ~ sex, var.equal = TRUE)
t.test(bmi ~ sex) #Welch
wilcox.test(bmi ~ sex)

# Q2.4(e)
pval_vartest <- var.test(bmi ~ nsregion, data = insurance)$p.value
t.test(bmi ~ nsregion, 
       var.equal = pval_vartest  < 0.05)

