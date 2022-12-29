# Script to exlude participants with neurologic conditions
library(dplyr)

# Check with Biobank mail if we have participants that have withdrawn their consent before finalising

setwd("/Users/sandratamm/Desktop/UKB_test")
load("UKB_data_labeled.RData")


# Remove participants with stroke
bd <- subset(bd, Stroke == FALSE)

# Remove participants with brain malignancy
bd <- subset(bd, Brain_tumour == FALSE)

# Remove participants with other neurological disorder
bd <- subset(bd, Neurological_disease == FALSE)

# Remove participants who withdrawn their consent
setwd("/Users/sandratamm/Desktop/UKB_amygdala_depression_data/")
no_concent <- read.csv("w6818_20210201.csv", header = F)


bd$consent <- NA


for(i in 1:length(bd$eid)){
  if(bd$eid[i] %in% no_concent$V1){
    bd$consent[i] <- FALSE
    }else{
      bd$consent[i] <- TRUE
  }
}

bd <- subset(bd, bd$consent == TRUE)





bd$Depression <- apply(bd, 1, function(bd) any(bd %in% "depression"
))



bd <- subset(bd, select = c("eid", 
"Body.mass.index..BMI._2.0",
"Sex_0.0",
"Median.BOLD.effect..in.group.defined.mask..for.faces.shapes.contrast_2.0",
"Age.when.attended.assessment.centre_2.0",
"Median.z.statistic..in.group.defined.mask..for.faces.shapes.contrast_2.0",
"Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0",
"Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0",
"Median.BOLD.effect..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0",
"Median.z.statistic..in.group.defined.amygdala.activation.mask..for.faces.shapes.contrast_2.0",
"Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0",
"Frequency.of.depressed.mood.in.last.2.weeks_2.0",
"Townsend.deprivation.index.at.recruitment_0.0",
"College",
"f_Sex_0.0",
"f_Frequency.of.depressed.mood.in.last.2.weeks_2.0",                                           
"f_Frequency.of.tiredness...lethargy.in.last.2.weeks_2.0",                                     
"f_Frequency.of.tenseness...restlessness.in.last.2.weeks_2.0",                                 
"f_Frequency.of.unenthusiasm...disinterest.in.last.2.weeks_2.0",                               
"f_Ever.depressed.for.a.whole.week_2.0",                                                       
"Smoking.status_2.0",
"Antidepressants_2",
"Depression",
"Median.BOLD.effect..in.group.defined.mask..for.shapes.activation_2.0",
"Median.BOLD.effect..in.group.defined.mask..for.faces.activation_2.0")
)

# save file for later
setwd("/Users/sandratamm/Desktop/UKB_amygdala_depression_data/")
save(bd, file="UKB_data_labeled_relevant_columns.RData")

