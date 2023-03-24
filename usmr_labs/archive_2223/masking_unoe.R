library(tidyverse)
braindata <- read_csv("https://uoepsy.github.io/data/usmr_braindata.csv")
head(braindata)
# Y = mass_brain
# X1 = mass_body
# X2 = species
# Y and X1 are marginally associated
plot(mass_brain ~ mass_body, data = braindata)
cor.test(braindata$mass_brain, braindata$mass_body)
# Y and X2 are marginally associated
boxplot(mass_brain ~ species, data = braindata)
summary(aov(mass_brain ~ species, data = braindata))
# X1 and X2 are marginally associated
boxplot(mass_body ~ species, data = braindata)
summary(aov(mass_body ~ species, data = braindata))

# order and significance
# species is a strong predictor and explains most of the variation in mass_brain. 
# Once you account for that, there is not much left to be explained for mass_body.
# The association between mass_body and species leads to mass_body explaining some 
# variation in mass_brain but it also leaves some to species which is a strong predictor
fit1 <- lm(mass_brain ~ mass_body + species, data = braindata)
summary(fit1)
anova(fit1)
fit2 <- lm(mass_brain ~ species + mass_body, data = braindata)
summary(fit2)
anova(fit2)

# check effect after removing the effect of a predictor
fit.mass.brain = lm(mass_brain ~ mass_body, data = braindata)
braindata$res.mass.brain = resid(fit.mass.brain)
boxplot(res.mass.brain ~ species, data = braindata)
fit.mass.body = lm(mass_body ~ species, data = braindata)
braindata$res.mass.body = resid(fit.mass.body)
summary(lm(mass_brain ~ res.mass.body + species, data = braindata))
summary(lm(mass_brain ~ species + res.mass.body, data = braindata))
anova(lm(mass_brain ~ res.mass.body + species, data = braindata))
anova(lm(mass_brain ~ species + res.mass.body, data = braindata))
# association after accounting for species
plot(res.mass.brain ~ res.mass.body, data = braindata)
boxplot(res.mass.brain ~ species, data = braindata)
boxplot(res.mass.body ~ species, data = braindata)
