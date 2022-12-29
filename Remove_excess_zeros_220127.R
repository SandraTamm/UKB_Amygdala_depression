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
                        & !is.na(bd$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0) &
                          !is.na(bd$Body.mass.index..BMI._2.0))

# Demographics
table1(~ factor(Sex_0.0) + Age.when.attended.assessment.centre_2.0+
         Townsend.deprivation.index.at.recruitment_0.0+College + 
         Body.mass.index..BMI._2.0 +
         Depression_score_2,
       data=bd_no_missing)


# Get IQR for Depression score
summary(bd$Depression_score_2)



hist(bd_no_missing$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0, 
     breaks = 600)


participant_zeros <- subset(bd_no_missing, bd_no_missing$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 == 0)

bd_no_missing <- subset(bd_no_missing, bd_no_missing$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 != 0)


model_1 <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                 Depression_score_2,
               data = bd_no_missing)
summary(model_1)
summ(model_1, digits = getOption("jtools-digits", 4))



my.formula <- y ~ x
Plot_1 <- ggplot(bd_no_missing, aes(Depression_score_2, Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)) + 
  geom_jitter(alpha = I(1/4), aes(color = Depression_score_2)) + 
  geom_smooth(aes(), method = "lm",se = TRUE, formula = my.formula, colour="black") +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", color = "black", size = 10),
        axis.title.y = element_text(face = "bold", color = "black", size = 10), 
        legend.position = "none") + 
  labs(x = "Depression Score", y = "Median BOLD effect for faces > shapes")

model_2 <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                 Depression_score_2+
                 Age.when.attended.assessment.centre_2.0+
                 Sex_0.0+
                 Townsend.deprivation.index.at.recruitment_0.0+
                 College +
                 Body.mass.index..BMI._2.0,
               data = bd_no_missing)
summary(model_2)
summ(model_2, digits = getOption("jtools-digits", 4))

partial_f2(model_2)



# Univariable tests for covariates

model_age <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Age.when.attended.assessment.centre_2.0,
                 data = bd_no_missing)
summary(model_age)
summ(model_age, digits = getOption("jtools-digits", 4))


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

Plot_2 <- ggplot(bd_no_missing, aes(Age.when.attended.assessment.centre_2.0, Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)) + 
  geom_point(alpha = I(2/4), aes(color = factor(Age.when.attended.assessment.centre_2.0))) + 
  geom_smooth(aes(), method = "lm",se = TRUE, formula = my.formula, colour="black") +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", color = "black", size = 10),
        axis.title.y = element_text(face = "bold", color = "black", size = 10), 
        legend.position = "none") + 
  labs(x = "Age", y = "Median BOLD effect for faces > shapes")

Plot_3 <- ggplot(bd_no_missing, aes(Sex_0.0, Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)) + 
  geom_jitter(alpha = I(2/4), aes(color = factor(Sex_0.0))) + 
  stat_summary(fun.data = mean_cl_normal, geom = "crossbar",
               width = .5, color = "black")+
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", color = "black", size = 10),
        axis.title.y = element_text(face = "bold", color = "black", size = 10), 
        legend.position = "none") + 
  scale_x_continuous(breaks=c(0,0.5,1),
                     labels=c("Female", "", "Male")) +
  labs(x = "Sex", y = "Median BOLD effect for faces > shapes")


Plot_4 <- ggplot(bd_no_missing, aes(Townsend.deprivation.index.at.recruitment_0.0, Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)) + 
  geom_point(alpha = I(2/4), aes(color = factor(Townsend.deprivation.index.at.recruitment_0.0))) +
  geom_smooth(aes(), method = "lm",se = TRUE, formula = my.formula, colour="black") +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", color = "black", size = 10),
        axis.title.y = element_text(face = "bold", color = "black", size = 10), 
        legend.position = "none") + 
  labs(x = "Townsend deprivation index", y = "Median BOLD effect for faces > shapes")

Plot_5 <- ggplot(bd_no_missing, aes(College, Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)) + 
  geom_jitter(alpha = I(2/4), aes(color = factor(College))) + 
  stat_summary(fun.data = mean_cl_normal, geom = "crossbar",
               width = .5, color = "black")+
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", color = "black", size = 10),
        axis.title.y = element_text(face = "bold", color = "black", size = 10), 
        legend.position = "none") + 
  labs(x = "Attended college", y = "Median BOLD effect for faces > shapes")



Plot_6 <- ggplot(bd_no_missing, aes(Body.mass.index..BMI._2.0, Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)) + 
  geom_point(alpha = I(2/4), aes(color = factor(Body.mass.index..BMI._2.0))) + 
  geom_smooth(aes(), method = "lm",se = TRUE, formula = my.formula, colour="black") +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", color = "black", size = 10),
        axis.title.y = element_text(face = "bold", color = "black", size = 10), 
        legend.position = "none") + 
  labs(x = "Body Mass Index", y = "Median BOLD effect for faces > shapes")

ggarrange(Plot_1, Plot_2, Plot_3, Plot_4, Plot_5, Plot_6, 
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 2)



# Sensitivity analysis without 0

sensitivity_no_zeros <- subset(bd_no_missing, Depression_score_2 > 0)


model_sensitivity_1 <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                             Depression_score_2,
                           data = sensitivity_no_zeros)
summary(model_sensitivity_1)
summ(model_sensitivity_1, digits = getOption("jtools-digits", 4))


model_sensitivity_1_adj <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                                 Depression_score_2+
                                 Age.when.attended.assessment.centre_2.0+
                                 Sex_0.0+
                                 Townsend.deprivation.index.at.recruitment_0.0+
                                 College +
                                 Body.mass.index..BMI._2.0,
                               data = sensitivity_no_zeros)
summary(model_sensitivity_1_adj)
summ(model_sensitivity_1_adj, digits = getOption("jtools-digits", 4))



# Univariable tests for covariates (sample without 0 values for depression)

model_age <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Age.when.attended.assessment.centre_2.0,
                 data = sensitivity_no_zeros)
summary(model_age)
summ(model_age, digits = getOption("jtools-digits", 4))


model_sex <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Sex_0.0,
                 data = sensitivity_no_zeros)
summary(model_sex)
summ(model_sex, digits = getOption("jtools-digits", 4))


model_TDI <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Townsend.deprivation.index.at.recruitment_0.0,
                 data = sensitivity_no_zeros)
summary(model_TDI)
summ(model_TDI, digits = getOption("jtools-digits", 4))


model_college <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                       College,
                     data = sensitivity_no_zeros)
summary(model_college)
summ(model_college, digits = getOption("jtools-digits", 4))

model_BMI <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Body.mass.index..BMI._2.0,
                 data = sensitivity_no_zeros)
summary(model_BMI)
summ(model_BMI, digits = getOption("jtools-digits", 4))


# Compare 12 to 0

t.test(subset(bd_no_missing, Depression_score_2 == 0)$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0,
       subset(bd_no_missing, Depression_score_2 == 12)$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)


# Compare 7-12 to 0

t.test(subset(bd_no_missing, Depression_score_2 == 0)$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0,
       subset(bd_no_missing, Depression_score_2 > 6)$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)

#Count observations
length(subset(bd_no_missing, Depression_score_2 > 6)$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)



# Test PHQ-2
# Calculate PHQ-2 (remove 2 because initially coded 1-4)
bd_no_missing$PHQ_2_2.0 <- bd_no_missing$Frequency.of.depressed.mood.in.last.2.weeks_2.0 + bd_no_missing$Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0 -2
hist(bd_no_missing$PHQ_2_2.0)


model_1_PHQ2 <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                      PHQ_2_2.0,
                    data = bd_no_missing)
summary(model_1_PHQ2)
summ(model_1_PHQ2, digits = getOption("jtools-digits", 4))

model_2_PHQ2 <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                      PHQ_2_2.0+
                      Age.when.attended.assessment.centre_2.0+
                      Sex_0.0+
                      Townsend.deprivation.index.at.recruitment_0.0+
                      College +
                      Body.mass.index..BMI._2.0,
                    data = bd_no_missing)
summary(model_2_PHQ2)
summ(model_2_PHQ2, digits = getOption("jtools-digits", 4))

my.formula <- y ~ x
Plot_PHQ2 <- ggplot(bd_no_missing, aes(PHQ_2_2.0, Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)) + 
  geom_jitter(alpha = I(1/4), aes(color = PHQ_2_2.0)) + 
  geom_smooth(aes(), method = "lm",se = TRUE, formula = my.formula, colour="black") +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", color = "black", size = 10),
        axis.title.y = element_text(face = "bold", color = "black", size = 10), 
        legend.position = "none") + 
  labs(x = "Depression Score (PHQ-2)", y = "Median BOLD effect for faces > shapes")




# Investigate depression diagnosis as predictor

Plot_Depression_diagnosis <- ggplot(bd_no_missing, aes(Depression, Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)) + 
  geom_jitter(alpha = I(2/4), aes(color = factor(Depression))) + 
  stat_summary(fun.data = mean_cl_normal, geom = "crossbar",
               width = .5, color = "black")+  
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", color = "black", size = 10),
        axis.title.y = element_text(face = "bold", color = "black", size = 10), 
        legend.position = "none") + 
  labs(x = "Depression Diagnosis", y = "Median BOLD effect for faces > shapes")


model_1_diagnosis <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                           Depression,
                         data = bd_no_missing)
summary(model_1_diagnosis)
summ(model_1_diagnosis, digits = getOption("jtools-digits", 4))

model_2_diagnosis <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                           Depression+
                           Age.when.attended.assessment.centre_2.0+
                           Sex_0.0+
                           Townsend.deprivation.index.at.recruitment_0.0+
                           College +
                           Body.mass.index..BMI._2.0,
                         data = bd_no_missing)
summ(model_2_diagnosis, digits = getOption("jtools-digits", 4))




model_2_antidepressants <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                                 Depression_score_2 + 
                                 Antidepressants_2+
                                 Age.when.attended.assessment.centre_2.0+
                                 Sex_0.0+
                                 Townsend.deprivation.index.at.recruitment_0.0+
                                 College +
                                 Body.mass.index..BMI._2.0,
                               data = bd_no_missing)
summary(model_2_antidepressants)
summ(model_2_antidepressants, digits = getOption("jtools-digits", 4))


model_AD <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                  Antidepressants_2,
                data = bd_no_missing)
summary(model_AD)
summ(model_AD, digits = getOption("jtools-digits", 4))


model_3_antidepressants <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                                 Antidepressants_2+
                                 Age.when.attended.assessment.centre_2.0+
                                 Sex_0.0+
                                 Townsend.deprivation.index.at.recruitment_0.0+
                                 College +
                                 Body.mass.index..BMI._2.0,
                               data = bd_no_missing)
summary(model_3_antidepressants)
summ(model_3_antidepressants, digits = getOption("jtools-digits", 4))

# Look at CIDI defined depression

load("MH_follow_up.RData")

MHData <- merge(data, bd_no_missing)

Plot_CIDI_Depression <- ggplot(MHData, aes(CIDI_depression, Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)) + 
  geom_jitter(alpha = I(2/4), aes(color = factor(CIDI_depression))) + 
  stat_summary(fun.data = mean_cl_normal, geom = "crossbar",
               width = .5, color = "black")+  
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", color = "black", size = 10),
        axis.title.y = element_text(face = "bold", color = "black", size = 10), 
        legend.position = "none") + 
  labs(x = "CIDI Depression", y = "Median BOLD effect for faces > shapes")


model_1_CIDI <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                      CIDI_depression,
                    data = MHData)
summary(model_1_CIDI)
summ(model_1_CIDI, digits = getOption("jtools-digits", 4))

model_2_CIDI <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                      CIDI_depression+
                      Age.when.attended.assessment.centre_2.0+
                      Sex_0.0+
                      Townsend.deprivation.index.at.recruitment_0.0+
                      College +
                      Body.mass.index..BMI._2.0,
                    data = MHData)
summ(model_2_CIDI, digits = getOption("jtools-digits", 4))


sink(file = "CIDI_model.txt")
summ(model_2_CIDI, digits = getOption("jtools-digits", 4))
sink(file = NULL)


# Univariable tests for covariates

model_age <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Age.when.attended.assessment.centre_2.0,
                 data = MHData)
summary(model_age)
summ(model_age, digits = getOption("jtools-digits", 4))


model_sex <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Sex_0.0,
                 data = MHData)
summary(model_sex)
summ(model_sex, digits = getOption("jtools-digits", 4))


model_TDI <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Townsend.deprivation.index.at.recruitment_0.0,
                 data = MHData)
summary(model_TDI)
summ(model_TDI, digits = getOption("jtools-digits", 4))


model_college <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                       College,
                     data = MHData)
summary(model_college)
summ(model_college, digits = getOption("jtools-digits", 4))

model_BMI <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                   Body.mass.index..BMI._2.0,
                 data = MHData)
summary(model_BMI)
summ(model_BMI, digits = getOption("jtools-digits", 4))





