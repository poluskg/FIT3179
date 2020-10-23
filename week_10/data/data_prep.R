library(dplyr)
library(tidyr)

rm(list=ls())
county_data = read.csv("usElection2016.csv", header=TRUE)
county_data$party = "NONE"

drop = c("repgov14", "demgov14", "county", "fips", "othergov14", "demsen16", 
         "repsen16", "othersen16", "demhouse16", "rephouse16", "otherhouse16", 
         "demgov16", "repgov16", "othergov16")

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
    Romney_12 = sum(romney12),
    Obama_12 = sum(obama12),
    Otherpres_12 = sum(otherpres12),
    Trump_16 = sum(trump16),
    Clinton_16 = sum(clinton16),
    Otherpres_16 = sum(otherpres16),
    total_population = sum(total_population),
    votingAgeCap = sum(cvap),
    total_votes_2012 = sum(romney12, obama12, otherpres12),
    total_votes_2016 = sum(trump16, clinton16, otherpres16),
    White = sum(white_pct),
    Black = sum(black_pct),
    Hispanic = sum(hispanic_pct),
    Nonwhite = sum(nonwhite_pct),
    Foreign_born = sum(foreignborn_pct),
    Female_count = sum(female_pct), 
    age29andunder_count = sum(age29andunder_pct),
    age65andolder_count = sum(age65andolder_pct), 
    clf_unemploy_count = sum(clf_unemploy_pct),
    lesshs_count = sum(lesshs_pct), 
    lesscollege_count = sum(lesscollege_pct),
    lesshs_whites_count = sum(lesshs_whites_pct),
    lesscollege_whites_count = sum(lesscollege_whites_pct),
    rural_count = sum(rural_pct),
    median_hh_inc = median(median_hh_inc),
    Party_12 = "NONE",
    Party_16 = "NONE"
    )


# Add state weights
weights = data.frame(
  state=state_data$state, 
  weight=c(0)
)

# Merge datasets
state_data = cbind(weights, state_data)
state_data = state_data[-c(3)]

# Derive colour of state 2012
for (i in 1:length(state_data$Party_12)) {
  if (state_data$Romney_12[i] > state_data$Obama_12[i] & state_data$Romney_12[i] > state_data$Otherpres_12[i]) {
    state_data$Party_12[i] = "#DC143C"
  } else if (state_data$Obama_12[i] > state_data$Romney_12[i] & state_data$Obama_12[i] > state_data$Otherpres_12[i]) {
    state_data$Party_12[i] = "#00008B"
  } else if (state_data$Otherpres_12[i] > state_data$Romney_12[i] & state_data$Otherpres_12[i] > state_data$Obama_12[i]) {
    state_data$Party_12[i] = "#778899"
  }
}

# Derive colour of state 2016
for (i in 1:length(state_data$Party_16)) {
  if (state_data$Trump_16[i] > state_data$Clinton_16[i] & state_data$Trump_16[i] > state_data$Otherpres_16[i]) {
    state_data$Party_16[i] = "#DC143C"
  } else if (state_data$Clinton_16[i] > state_data$Trump_16[i] & state_data$Clinton_16[i] > state_data$Otherpres_16[i]) {
    state_data$Party_16[i] = "#00008B"
  } else if (state_data$Otherpres_16[i] > state_data$Trump_16[i] & state_data$Otherpres_16[i] > state_data$Clinton_16[i]) {
    state_data$Party_16[i] = "#778899"
  }
}


# Fold dataset against demographic and other pct
df <- gather(state_data, Demographic, Dem_pop, White:Foreign_born, factor_key=TRUE)
df <- df[order(df$state, df$Demographic),]


# Write to csv
write.csv(state_data, "cleaned_state_data.csv")
write.csv(df, "folded_data.csv")
