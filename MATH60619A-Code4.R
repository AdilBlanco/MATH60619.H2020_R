url <- "https://lbelzile.bitbucket.io/MATH60619A/servqual.sas7bdat"
servqual <- haven::read_sas(url)
servqual$bank <- factor(servqual$bank)
#Summary statistics
with(servqual, aggregate(x = reliability, list(bank), function(x){c(mean(x), sd(x))}))

library(dplyr)
servqual %>% 
   group_by(bank) %>% 
   summarize(mean = mean(reliability),
             sd = sd(reliability))


# Analysis of variance
oneway <- lm(reliability ~ bank, data = servqual)
anova(oneway)
# could also get the p-value from summary
summary(oneway)
# Levene's test for equality of variance
car::leveneTest(oneway, center = "mean")  
# Welch's ANOVA statistic
oneway.test(reliability ~ bank, data = servqual, var.equal = FALSE)

# Kruskal-Wallis test as a one-way ANOVA with ranks
anova(lm(rank(reliability) ~ bank, data = servqual))
# Function doesn't work correctly in presence of ties
kruskal.test(reliability~bank, data = servqual)
# Alternative (here gives same output)
coin::kruskal_test(reliability~bank, data = servqual)
#Box-and-whiskers plot
boxplot(reliability~bank, data = servqual)

#Compute p-value for mean differences
with(servqual, 
     pairwise.t.test(x = reliability, 
                     g = bank, p.adjust.method = "holm", 
                     pool.sd = FALSE)
     )
emmeans::emmeans(oneway, "bank", )


library(emmeans, quietly = TRUE)
url <- "https://lbelzile.bitbucket.io/MATH60619A/delay.sas7bdat"
delay <- haven::read_sas(url)
# Cast categorical variables to factors
delay$delay <- as.factor(delay$delay)
delay$stage <- as.factor(delay$stage)

# Check layout and counts
with(delay, table(stage, delay))
# Interaction plots
par(mfrow = c(1, 2), bty = "l", mar = c(4,4,1,3))
with(delay, interaction.plot(x.factor = stage, trace.factor = delay, response = eval))
with(delay, interaction.plot(x.factor = delay, trace.factor = stage, response = eval))

# Two-way ANOVA
twoway <- lm(eval ~ stage*delay, data = delay)
# Global F-test for interaction
anova(twoway) #interaction is significant


# Matrix of p-value for pairwise differences
pvals <- with(delay, pairwise.t.test(x = eval, 
                            g = stage:delay,
                            p.adjust.method = "none"))
#Remove the p-values not of interest
pvals$p.value[3,2] <- pvals$p.value[3,4] <- pvals$p.value[3,3] <- NA
pvals$p.value[4,1] <- pvals$p.value[4,3] <- NA
pvals$p.value[5,1] <- pvals$p.value[5,2] <- NA
#Find which p-values are non-zero
id <- which(!is.na(pvals$p.value))
# Adjust p-values (Holm stepdown procedure with Bonferroni)
pvals$p.value[id] <- p.adjust(pvals$p.value[id],"holm")
pvals

# Averages within each level of other variable
# Also compute mean differences with unadjusted p-values
simp1 <- emmeans::emmeans(twoway, pairwise ~ stage | delay)
simp2 <- emmeans::emmeans(twoway, pairwise ~ delay | stage)
rawpval <- c(simp1$contrasts$p.value, simp2$contrasts$p.value)
# Step-down Sidak adjustment
stepdownsidak <- function(p){
   cummax(1 - (1 - sort(p))^(length(p):1))[rank(p)]
}
stepdownsidak(rawpval) < 0.05

# Same analysis, this time with time as response

# Two-way ANOVA
twoway2 <- lm(time ~ stage*delay, data = delay)
car::leveneTest(time ~ stage:delay, data = delay, center = "mean")
# Global F-test for interaction
anova(twoway2) #interaction not significant
twoway2 <- lm(time ~ stage + delay, data = delay)
summary(twoway2) # contrast estimates
anova(twoway2) # both main effects are significant
