require(ggplot2)
require(lme4)
library(ggpubr)
require(jtools)
require(table1)

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



model_1 <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                 Depression_score_2,
               data = bd_no_missing)
summary(model_1)



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


# Compare 12 to 0

t.test(subset(bd_no_missing, Depression_score_2 == 0)$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0,
       subset(bd_no_missing, Depression_score_2 == 12)$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)


# Test PHQ-2
# Calculate PHQ-2 (remove 2 because initially coded 1-4)
bd_no_missing$PHQ_2_2.0 <- bd_no_missing$Frequency.of.depressed.mood.in.last.2.weeks_2.0 + bd_no_missing$Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0 -2
hist(bd_no_missing$PHQ_2_2.0)


model_1_PHQ2 <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                      PHQ_2_2.0,
               data = bd_no_missing)
summary(model_1_PHQ2)

model_2_PHQ2 <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                      PHQ_2_2.0+
                 Age.when.attended.assessment.centre_2.0+
                 Sex_0.0+
                 Townsend.deprivation.index.at.recruitment_0.0+
                 College +
                 Body.mass.index..BMI._2.0,
               data = bd_no_missing)
summary(model_2_PHQ2)
summ(model_2_PHQ2)

my.formula <- y ~ x
Plot_PHQ2 <- ggplot(bd_no_missing, aes(PHQ_2_2.0, Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0)) + 
  geom_jitter(alpha = I(1/4), aes(color = PHQ_2_2.0)) + 
  geom_smooth(aes(), method = "lm",se = TRUE, formula = my.formula, colour="black") +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", color = "black", size = 10),
        axis.title.y = element_text(face = "bold", color = "black", size = 10), 
        legend.position = "none") + 
  labs(x = "Depression Score (PHQ-2)", y = "Median BOLD effect for faces > shapes")



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


model_2_diagnosis <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                      Depression+
                      Age.when.attended.assessment.centre_2.0+
                      Sex_0.0+
                      Townsend.deprivation.index.at.recruitment_0.0+
                      College +
                      Body.mass.index..BMI._2.0,
                    data = bd_no_missing)
summary(model_2_diagnosis)

model_1_antidepressants <- glm(Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0 ~
                          Depression + Antidepressants_2,
                         data = bd_no_missing)
summary(model_1_antidepressants)


