# Find medication info

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


summary(bd_no_missing$Antidepressants_2)

participants_with_AD <- subset(bd_no_missing, bd_no_missing$Antidepressants_2 == TRUE)
participants_with_AD <- subset(participants_with_AD, select = "eid")

setwd("/Users/sandratamm/Desktop/UKB_test")
load("UKB_data_labeled.RData")

participants_with_AD <- merge(participants_with_AD, bd)
medication <- participants_with_AD[ , grepl( "Treatment.medication.code_2." , names( participants_with_AD ) ) ]
