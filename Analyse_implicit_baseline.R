require(ggplot2)
require(lme4)
library(ggpubr)
require(jtools)
require(table1)
require(sensemakr)

setwd("~/Desktop/UKB_amygdala_depression_data/")
load("UKB_file_for_analyses.RData")

bd_no_missing <- subset(bd, !is.na(bd$Depression_score_2) &
                          !is.na(bd$Age.when.attended.assessment.centre_2.0) & !is.na(bd$Sex_0.0) & 
                          !is.na(bd$Townsend.deprivation.index.at.recruitment_0.0) & !is.na(bd$College)
                        & !is.na(bd$Median.BOLD.effect..in.group.defined.mask..for.shapes.activation_2.0) &
                          !is.na(bd$Body.mass.index..BMI._2.0))


# Shapes vs implicit baseline

model_1 <- glm(Median.BOLD.effect..in.group.defined.mask..for.shapes.activation_2.0 ~
                 Depression_score_2,
               data = bd_no_missing)
summary(model_1)
summ(model_1, digits = getOption("jtools-digits", 4))



model_2 <- glm(Median.BOLD.effect..in.group.defined.mask..for.shapes.activation_2.0 ~
                 Depression_score_2+
                 Age.when.attended.assessment.centre_2.0+
                 Sex_0.0+
                 Townsend.deprivation.index.at.recruitment_0.0+
                 College +
                 Body.mass.index..BMI._2.0,
               data = bd_no_missing)
summary(model_2)
summ(model_2, digits = getOption("jtools-digits", 4))


# Shapes vs implicit baseline

model_1 <- glm(Median.BOLD.effect..in.group.defined.mask..for.faces.activation_2.0 ~
                 Depression_score_2,
               data = bd_no_missing)
summary(model_1)
summ(model_1, digits = getOption("jtools-digits", 4))



model_2 <- glm(Median.BOLD.effect..in.group.defined.mask..for.faces.activation_2.0 ~
                 Depression_score_2+
                 Age.when.attended.assessment.centre_2.0+
                 Sex_0.0+
                 Townsend.deprivation.index.at.recruitment_0.0+
                 College +
                 Body.mass.index..BMI._2.0,
               data = bd_no_missing)
summary(model_2)
summ(model_2, digits = getOption("jtools-digits", 4))


bd_no_missing$Diff_BOLD <- bd_no_missing$Median.BOLD.effect..in.group.defined.mask..for.faces.activation_2.0 - bd_no_missing$Median.BOLD.effect..in.group.defined.mask..for.shapes.activation_2.0

model_1 <- glm(Diff_BOLD ~
                 Depression_score_2,
               data = bd_no_missing)
summary(model_1)
summ(model_1, digits = getOption("jtools-digits", 4))



model_2 <- glm(Diff_BOLD ~
                 Depression_score_2+
                 Age.when.attended.assessment.centre_2.0+
                 Sex_0.0+
                 Townsend.deprivation.index.at.recruitment_0.0+
                 College +
                 Body.mass.index..BMI._2.0,
               data = bd_no_missing)
summary(model_2)
summ(model_2, digits = getOption("jtools-digits", 4))

model_3 <- glm(Depression_score_2 ~
                 Median.BOLD.effect..in.group.defined.mask..for.faces.activation_2.0*bd_no_missing$Median.BOLD.effect..in.group.defined.mask..for.shapes.activation_2.0+
                 Age.when.attended.assessment.centre_2.0+
                 Sex_0.0+
                 Townsend.deprivation.index.at.recruitment_0.0+
                 College +
                 Body.mass.index..BMI._2.0,
               data = bd_no_missing)
summary(model_3)
summ(model_3, digits = getOption("jtools-digits", 4))


