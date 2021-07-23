require(sjmisc)

setwd("~/Desktop/UKB_amygdala_depression_data/")
load("UKB_file_for_analyses.RData")

bd_no_missing <- subset(bd, !is.na(bd$Depression_score_2) &
                          !is.na(bd$Age.when.attended.assessment.centre_2.0) & !is.na(bd$Sex_0.0) & 
                          !is.na(bd$Townsend.deprivation.index.at.recruitment_0.0) & !is.na(bd$College)
                        & !is.na(bd$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0) &
                          !is.na(bd$Body.mass.index..BMI._2.0))



median(bd_no_missing$Age.when.attended.assessment.centre_2.0)

bd_no_missing <- dicho(
  bd_no_missing,
  Age.when.attended.assessment.centre_2.0,
  dich.by = "median",
  as.num = FALSE,
  var.label = "Age_group",
  val.labels = c("younger", "older"),
  append = TRUE,
  suffix = "_d"
)

bd_no_missing_young <- subset(bd_no_missing, Age.when.attended.assessment.centre_2.0_d == 0)
bd_no_missing_old <- subset(bd_no_missing, Age.when.attended.assessment.centre_2.0_d == 1)





model_1_young <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                 Depression_score_2,
               data = bd_no_missing_young)
summary(model_1_young)
summ(model_1_young, digits = getOption("jtools-digits", 4))



model_2_young <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                 Depression_score_2+
                 Age.when.attended.assessment.centre_2.0+
                 Sex_0.0+
                 Townsend.deprivation.index.at.recruitment_0.0+
                 College +
                 Body.mass.index..BMI._2.0,
               data = bd_no_missing_young)
summary(model_2_young)
summ(model_2_young, digits = getOption("jtools-digits", 4))



sink(file = "Young_subsamble.txt")
summ(model_2_young, digits = getOption("jtools-digits", 4))
sink(file = NULL)

#Univariable testing of covariates
model_age <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Age.when.attended.assessment.centre_2.0,
                 data = bd_no_missing_young)
summary(model_age)
summ(model_age, digits = getOption("jtools-digits", 4))


model_sex <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Sex_0.0,
                 data = bd_no_missing_young)
summary(model_sex)
summ(model_sex, digits = getOption("jtools-digits", 4))


model_TDI <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Townsend.deprivation.index.at.recruitment_0.0,
                 data = bd_no_missing_young)
summary(model_TDI)
summ(model_TDI, digits = getOption("jtools-digits", 4))


model_college <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                       College,
                     data = bd_no_missing_young)
summary(model_college)
summ(model_college, digits = getOption("jtools-digits", 4))

model_BMI <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Body.mass.index..BMI._2.0,
                 data = bd_no_missing_young)
summary(model_BMI)
summ(model_BMI, digits = getOption("jtools-digits", 4))





model_1_old <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                       Depression_score_2,
                     data = bd_no_missing_old)
summary(model_1_old)
summ(model_1_old, digits = getOption("jtools-digits", 4))



model_2_old <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                       Depression_score_2+
                       Age.when.attended.assessment.centre_2.0+
                       Sex_0.0+
                       Townsend.deprivation.index.at.recruitment_0.0+
                       College +
                       Body.mass.index..BMI._2.0,
                     data = bd_no_missing_old)
summary(model_2_old)
summ(model_2_old, digits = getOption("jtools-digits", 4))


sink(file = "Old_subsamble.txt")
summ(model_2_old, digits = getOption("jtools-digits", 4))
sink(file = NULL)



#Univariable testing of covariates
model_age <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Age.when.attended.assessment.centre_2.0,
                 data = bd_no_missing_old)
summary(model_age)
summ(model_age, digits = getOption("jtools-digits", 4))


model_sex <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Sex_0.0,
                 data = bd_no_missing_old)
summary(model_sex)
summ(model_sex, digits = getOption("jtools-digits", 4))


model_TDI <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Townsend.deprivation.index.at.recruitment_0.0,
                 data = bd_no_missing_old)
summary(model_TDI)
summ(model_TDI, digits = getOption("jtools-digits", 4))


model_college <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                       College,
                     data = bd_no_missing_old)
summary(model_college)
summ(model_college, digits = getOption("jtools-digits", 4))

model_BMI <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Body.mass.index..BMI._2.0,
                 data = bd_no_missing_old)
summary(model_BMI)
summ(model_BMI, digits = getOption("jtools-digits", 4))









model_1_age_interaction <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                     Depression_score_2*Age.when.attended.assessment.centre_2.0_d,
                   data = bd_no_missing)
summary(model_1_age_interaction)
summ(model_1_age_interaction, digits = getOption("jtools-digits", 4))


model_2_age_interaction <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                     Depression_score_2*Age.when.attended.assessment.centre_2.0_d+
                     Sex_0.0+
                     Townsend.deprivation.index.at.recruitment_0.0+
                     College +
                     Body.mass.index..BMI._2.0,
                   data = bd_no_missing)
summary(model_2_age_interaction)
summ(model_2_age_interaction, digits = getOption("jtools-digits", 4))

sink(file = "Age_interaction.txt")
summ(model_2_age_interaction, digits = getOption("jtools-digits", 4))
sink(file = NULL)


model_age_group <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Age.when.attended.assessment.centre_2.0_d,
                 data = bd_no_missing)
summary(model_age_group)
summ(model_age_group, digits = getOption("jtools-digits", 4))


model_sex <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Sex_0.0,
                 data = bd_no_missing)
summary(model_sex)
summ(model_sex, digits = getOption("jtools-digits", 4))


model_TDI <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Townsend.deprivation.index.at.recruitment_0.0,
                 data = bd_no_missing)
summary(model_TDI)
summ(model_TDI, digits = getOption("jtools-digits", 4))


model_college <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                       College,
                     data = bd_no_missing)
summary(model_college)
summ(model_college, digits = getOption("jtools-digits", 4))

model_BMI <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Body.mass.index..BMI._2.0,
                 data = bd_no_missing)
summary(model_BMI)
summ(model_BMI, digits = getOption("jtools-digits", 4))
