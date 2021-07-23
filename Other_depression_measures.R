# CIDI definition depression

data <- read.csv("~/Desktop/UKB_2009312/test_1.csv")

data[data < 0] <- NA

data <- subset(data, !is.na(data$Ever.had.prolonged.feelings.of.sadness.or.depression_0.0))

data$Main_symptom <- data$Ever.had.prolonged.feelings.of.sadness.or.depression_0.0 + data$Ever.had.prolonged.loss.of.interest.in.normal.activities_0.0

data$Main_symptom[data$Main_symptom < 1] <- "No"
data$Main_symptom[data$Main_symptom == 1 | data$Main_symptom == 2] <- "Yes"

data$number_of_symptoms <- rowSums(subset(data, select = c(Ever.had.prolonged.feelings.of.sadness.or.depression_0.0, Ever.had.prolonged.loss.of.interest.in.normal.activities_0.0,
  Weight.change.during.worst.episode.of.depression_0.0, Did.your.sleep.change._0.0, Feelings.of.tiredness.during.worst.episode.of.depression_0.0,
  Feelings.of.worthlessness.during.worst.period.of.depression_0.0, Difficulty.concentrating.during.worst.depression_0.0,
  Thoughts.of.death.during.worst.depression_0.0)), na.rm = TRUE)

data$CIDI_depression <- NA


for(i in 1:length(data$Main_symptom)){
  if(!is.na(data$Main_symptom[i]) & data$Main_symptom[i] == "Yes" & 
     data$number_of_symptoms[i] > 4 & !is.na(data$number_of_symptoms[i]) &
     !is.na(data$Impact.on.normal.roles.during.worst.period.of.depression_0.0[i]) & 
    data$Impact.on.normal.roles.during.worst.period.of.depression_0.0[i] == 3){
    data$CIDI_depression[i] <- TRUE
  }else{
    data$CIDI_depression[i] <- FALSE
  }
}

save(data, file = "MH_follow_up.RData")

