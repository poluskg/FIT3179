library(dplyr)

rm(list=ls())
county_data = read.csv("usElection2016.csv", header=TRUE)
county_data$party = "NONE"

drop = c("romney12", "obama12", "otherpres12", "repgov14", "demgov14", "county",
         "fips", "othergov14", "demsen16", "repsen16", "othersen16", "demhouse16", 
         "rephouse16", "otherhouse16", "demgov16", "repgov16", "othergov16")

county_data = county_data[,!(names(county_data) %in% drop)]

county_data = na.omit(county_data)

# Convert all percentage of total_population for each county to number value
county_data$white_pct = county_data$white_pct/100 * county_data$total_population
county_data$black_pct = county_data$black_pct/100 * county_data$total_population
county_data$hispanic_pct = county_data$hispanic_pct/100 * county_data$total_population
county_data$nonwhite_pct = county_data$nonwhite_pct/100 * county_data$total_population
county_data$foreignborn_pct = county_data$foreignborn_pct/100 * county_data$total_population
county_data$female_pct = county_data$female_pct/100 * county_data$total_population
county_data$age29andunder_pct = county_data$age29andunder_pct/100 * county_data$total_population
county_data$age65andolder_pct = county_data$age65andolder_pct/100 * county_data$total_population
county_data$clf_unemploy_pct = county_data$clf_unemploy_pct/100 * county_data$total_population
county_data$lesshs_pct = county_data$lesshs_pct/100 * county_data$total_population
county_data$lesscollege_pct = county_data$lesscollege_pct/100 * county_data$total_population
county_data$lesshs_whites_pct = county_data$lesshs_whites_pct/100 * county_data$total_population
county_data$lesscollege_whites_pct = county_data$lesscollege_whites_pct/100 * county_data$total_population
county_data$rural_pct = county_data$rural_pct/100 * county_data$total_population

# Group data by state
state_data = county_data %>% group_by(state) %>%
  summarise(
    trump16 = sum(trump16),
    clinton16 = sum(clinton16),
    otherpres16 = sum(otherpres16),
    total_population = sum(total_population), 
    cvap = sum(cvap),
    white_pct = sum(white_pct),
    black_pct = sum(black_pct),
    hispanic_pct = sum(hispanic_pct),
    nonwhite_pct = sum(nonwhite_pct),
    foreignborn_pct = sum(foreignborn_pct),
    female_pct = sum(female_pct), 
    age29andunder_pct = sum(age29andunder_pct),
    age65andolder_pct = sum(age65andolder_pct), 
    clf_unemploy_pct = sum(clf_unemploy_pct),
    lesshs_pct = sum(lesshs_pct), 
    lesscollege_pct = sum(lesscollege_pct),
    lesshs_whites_pct = sum(lesshs_whites_pct),
    lesscollege_whites_pct = sum(lesscollege_whites_pct),
    rural_pct = sum(rural_pct),
    median_hh_inc = median(median_hh_inc),
    party = "NONE"
    )

attach(state_data)

for (i in 1:length(state_data$party)) {
  if (trump16[i] > clinton16[i] & trump16[i] > otherpres16[i]) {
    state_data$party[i] = "#DC143C"
  } else if (clinton16[i] > trump16[i] & clinton16[i] > otherpres16[i]) {
    state_data$party[i] = "#00008B"
  } else if (otherpres16[i] > trump16[i] & otherpres16[i] > clinton16[i]) {
    state_data$party[i] = "#778899"
  }
}

write.csv(state_data, "cleaned_state_data.csv")

