# Baysian analysis
require(rstanarm)
library(bayestestR)
require(BayesFactor)

setwd("~/Desktop/UKB_amygdala_depression_data/")
load("UKB_file_for_analyses.RData")

bd_no_missing <- subset(bd, !is.na(bd$Depression_score_2) &
                          !is.na(bd$Age.when.attended.assessment.centre_2.0) & !is.na(bd$Sex_0.0) & 
                          !is.na(bd$Townsend.deprivation.index.at.recruitment_0.0) & !is.na(bd$College)
                        & !is.na(bd$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0) &
                          !is.na(bd$Body.mass.index..BMI._2.0))



bd_model_1 <- subset(bd_no_missing, select = c("Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0",
                                               "Depression_score_2"))

bd_model_2 <- subset(bd_no_missing, select = c("Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0",
                                               "Depression_score_2", "Age.when.attended.assessment.centre_2.0", "Sex_0.0", "Townsend.deprivation.index.at.recruitment_0.0", "College",
                                               "Body.mass.index..BMI._2.0"))


colnames(bd_model_2) <- c("Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0", "Depression score", "Age",
                          "Sex", "TDI", "College education", "BMI")

bf = regressionBF(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~ ., data = bd_model_2)
length(bf)
head(bf, n=6)
which.max(bf)
bf2 = head(bf) / max(bf)
plot(bf2)


bf = regressionBF(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~ ., data = bd_model_2, whichModels = "bottom")
plot(bf, )


model_bayes_1 <- stan_glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0~., data=bd_model_1)

describe_posterior(model_bayes_1)
plot(model_bayes_1, ci_level = 0.5)

model_bayes_2 <- stan_glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0~., data=bd_model_2)
plot(model_bayes_2, ci_level = 0.5)

describe_posterior(model_bayes_2)


# Analyse amygdala as independent variable

model_1 <- glm(Depression_score_2 ~
               Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0,
               data = bd_no_missing)
summary(model_1)
summ(model_1, digits = getOption("jtools-digits", 4))




sink(file = "Amy_DV.txt")
summ(model_2, digits = getOption("jtools-digits", 4))
sink(file = NULL)


# Univariable tests for covariates

model_age <- glm(Depression_score_2 ~
                   Age.when.attended.assessment.centre_2.0,
                 data = bd_no_missing)
summary(model_age)
summ(model_age, digits = getOption("jtools-digits", 4))


model_sex <- glm(Depression_score_2 ~
                   Sex_0.0,
                 data = bd_no_missing)
summary(model_sex)
summ(model_sex, digits = getOption("jtools-digits", 4))


model_TDI <- glm(Depression_score_2 ~
                   Townsend.deprivation.index.at.recruitment_0.0,
                 data = bd_no_missing)
summary(model_TDI)
summ(model_TDI, digits = getOption("jtools-digits", 4))


model_college <- glm(Depression_score_2 ~
                       College,
                     data = bd_no_missing)
summary(model_college)
summ(model_college, digits = getOption("jtools-digits", 4))

model_BMI <- glm(Depression_score_2 ~
                   Body.mass.index..BMI._2.0,
                 data = bd_no_missing)
summary(model_BMI)
summ(model_BMI, digits = getOption("jtools-digits", 4))


my.formula <- y ~ x
Plot_1 <- ggplot(bd_no_missing, aes(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0, Depression_score_2)) + 
  geom_jitter(alpha = I(1/4), aes(color = Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)) + 
  geom_smooth(aes(), method = "lm",se = TRUE, formula = my.formula, colour="black") +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", color = "black", size = 10),
        axis.title.y = element_text(face = "bold", color = "black", size = 10), 
        legend.position = "none") + 
  labs(y = "Depression Score", x = "Median BOLD effect for faces > shapes")

model_2 <- glm(Depression_score_2 ~
                 Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0+
                 Age.when.attended.assessment.centre_2.0+
                 Sex_0.0+
                 Townsend.deprivation.index.at.recruitment_0.0+
                 College +
                 Body.mass.index..BMI._2.0,
               data = bd_no_missing)
summary(model_2)
