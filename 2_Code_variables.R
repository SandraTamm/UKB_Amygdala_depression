# Script to code variables according to OSF plan
setwd("/Users/sandratamm/Desktop/UKB_amygdala_depression_data/")
load("UKB_data_labeled_relevant_columns.RData")



# Replace "I don't know" and "Prefer not to answer" with NA
bd$Frequency.of.depressed.mood.in.last.2.weeks_2.0[bd$Frequency.of.depressed.mood.in.last.2.weeks_2.0 <0] <- NA


bd$Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0[bd$Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0 <0] <- NA

bd$Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0[bd$Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0 <0] <- NA

bd$Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0[bd$Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0 <0] <- NA

# # Make column combining 4 depression items and substract 4 (because variables are coded 1-5 not 0-4)

bd$Depression_score_2 <- bd$Frequency.of.depressed.mood.in.last.2.weeks_2.0 + bd$Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0 + 
  bd$Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0 + bd$Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0 -4


bd$PHQ_2 <- bd$Frequency.of.depressed.mood.in.last.2.weeks_2.0 + bd$Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0 -2



# Define depression 'cases'
bd$Depression_case_2 <- bd$f_Frequency.of.depressed.mood.in.last.2.weeks_2.0




levels(bd$Depression_case_2)[levels(bd$Depression_case_2) == "Nearly every day" |
                               levels(bd$Depression_case_2) == "More than half the days" |
                               levels(bd$Depression_case_2) == "Several days"] <- "yes"


levels(bd$Depression_case_2)[levels(bd$Depression_case_2) == "Not at all"] <- "no"

levels(bd$Depression_case_2)[levels(bd$Depression_case_2) == "Do not know" |
                               levels(bd$Depression_case_2) == "Prefer not to answer"] <- NA




# Tiredness/lethargy or not


levels(bd$f_Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0)[levels(bd$f_Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0) == "Nearly every day" |
                                                                     levels(bd$f_Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0) == "More than half the days" |
                                                                     levels(bd$f_Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0) == "Several days"] <- "yes"

levels(bd$f_Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0)[levels(bd$f_Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0) == "Not at all"] <- "no"


levels(bd$f_Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0)[levels(bd$f_Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0) == "Do not know" |
                                                                     levels(bd$f_Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0) == "Prefer not to answer"] <- NA




 
# # Tenseness


levels(bd$f_Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0)[levels(bd$f_Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0) == "Nearly every day" |
                                                                         levels(bd$f_Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0) == "More than half the days" |
                                                                         levels(bd$f_Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0) == "Several days"] <- "yes"

levels(bd$f_Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0)[levels(bd$f_Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0) == "Not at all"] <- "no"

levels(bd$f_Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0)[levels(bd$f_Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0) == "Do not know" |
                                                                         levels(bd$f_Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0) == "Prefer not to answer"] <- NA 




 
# # unenthusiasm/disintererst



levels(bd$f_Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0)[levels(bd$f_Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0) == "Nearly every day" |
                                                                           levels(bd$f_Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0) == "More than half the days" |
                                                                           levels(bd$f_Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0) == "Several days"] <- "yes"

levels(bd$f_Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0)[levels(bd$f_Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0) == "Not at all"] <- "no"


levels(bd$f_Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0)[levels(bd$f_Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0) == "Do not know" |
                                                                           levels(bd$f_Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0) == "Prefer not to answer"] <- NA




# Save file
setwd("/Users/sandratamm/Desktop/UKB_amygdala_depression_data/")
save(bd, file="UKB_file_for_analyses.RData")


