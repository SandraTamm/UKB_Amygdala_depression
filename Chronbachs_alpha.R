require(ltm)

setwd("~/Desktop/UKB_amygdala_depression_data/")
load("UKB_file_for_analyses.RData")

bd_no_missing <- subset(bd, !is.na(bd$Depression_score_2) &
                          !is.na(bd$Age.when.attended.assessment.centre_2.0) & !is.na(bd$Sex_0.0) & 
                          !is.na(bd$Townsend.deprivation.index.at.recruitment_0.0) & !is.na(bd$College)
                        & !is.na(bd$Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0) &
                          !is.na(bd$Body.mass.index..BMI._2.0))

alpha_data <- subset(bd_no_missing, select = c("Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0", "Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0",
                                               "Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0", "Frequency.of.depressed.mood.in.last.2.weeks_2.0"))


cronbach.alpha(alpha_data)

corrmatrix <- cor(alpha_data, method = "pearson", use = "complete.obs")


omega(corrmatrix, nfactors = 1, pc = "pa")
