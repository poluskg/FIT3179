library(dplyr)
library(tidyr)
library(stringr)
library(BBmisc)

rm(list=ls())
county_data = read.csv("usElection2016.csv", header=TRUE)
county_data$party = NA

drop = c("repgov14", "demgov14", "fips", "othergov14", "demsen16", 
         "repsen16", "othersen16", "demhouse16", "rephouse16", "otherhouse16", 
         "demgov16", "repgov16", "othergov16")

county_data = county_data[,!(names(county_data) %in% drop)]

# Derive colour of county 2016
for (i in 1:length(county_data$party)) {
  if (county_data$trump16[i] > county_data$clinton16[i] & county_data$trump16[i] > county_data$otherpres16[i]) {
    county_data$party[i] = "Repblican"
  } else if (county_data$clinton16[i] > county_data$trump16[i] & county_data$clinton16[i] > county_data$otherpres16[i]) {
    county_data$party[i] = "Democrat"
  } else if (county_data$otherpres16[i] > county_data$trump16[i] & county_data$otherpres16[i] > county_data$clinton16[i]) {
    county_data$party[i] = "Other"
  }
}

county_data = na.omit(county_data)
write.csv(county_data, "counties.csv")

drop = c("county")
county_data = county_data[,!(names(county_data) %in% drop)]

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
    Rep_12 = sum(romney12),
    Dem_12 = sum(obama12),
    Other_12 = sum(otherpres12),
    Rep_16 = sum(trump16),
    Dem_16 = sum(clinton16),
    Other_16 = sum(otherpres16),
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

# Derive colour of state 2012
for (i in 1:length(state_data$Party_12)) {
  if (state_data$Rep_12[i] > state_data$Dem_12[i] & state_data$Rep_12[i] > state_data$Other_12[i]) {
    state_data$Party_12[i] = "Repblican"
  } else if (state_data$Dem_12[i] > state_data$Rep_12[i] & state_data$Dem_12[i] > state_data$Other_12[i]) {
    state_data$Party_12[i] = "Democrat"
  } else if (state_data$Other_12[i] > state_data$Rep_12[i] & state_data$Other_12[i] > state_data$Dem_12[i]) {
    state_data$Party_12[i] = "Other"
  }
}

# Derive colour of state 2016
for (i in 1:length(state_data$Party_16)) {
  if (state_data$Rep_16[i] > state_data$Dem_16[i] & state_data$Rep_16[i] > state_data$Other_16[i]) {
    state_data$Party_16[i] = "Repblican"
  } else if (state_data$Dem_16[i] > state_data$Rep_16[i] & state_data$Dem_16[i] > state_data$Other_16[i]) {
    state_data$Party_16[i] = "Democrat"
  } else if (state_data$Other_16[i] > state_data$Rep_16[i] & state_data$Other_16[i] > state_data$Dem_16[i]) {
    state_data$Party_16[i] = "Other"
  }
}

#<-----MAP----->#
# Calculate difference for colour scale
state_data$VoteDiff = state_data$Rep_16 - state_data$Dem_16
x = state_data$VoteDiff
a = (x-min(x)/max(x)-min(x))
normalised = 2*a-1
state_data$VoteRange = normalised

# Write to csv
write.csv(state_data, "divergingColourData.csv")


#<-----DONUT CHART----->#
# Fold dataset against party votes
PartyVotesdf <- gather(state_data, Res, Votes, Rep_16:Other_16, factor_key=TRUE)
PartyVotesdf <- PartyVotesdf[order(PartyVotesdf$state, PartyVotesdf$Res),]
PartyVotesdf = PartyVotesdf[-c(2:25)]
PartyVotesdf$Party = NULL
# Change names
for (i in 1:length(PartyVotesdf$Res)) {
  if (PartyVotesdf$Res[i] == "Rep_16") {
    PartyVotesdf$Party[i] = "Repblican"
  } else if (PartyVotesdf$Res[i] == "Dem_16") {
    PartyVotesdf$Party[i] = "Democrat"
  } else if (PartyVotesdf$Res[i] == "Other_16") {
    PartyVotesdf$Party[i] = "Other"
  }
}
PartyVotesdf = PartyVotesdf[-c(2)]

write.csv(PartyVotesdf, "PartyVotes.csv")

#<-----DONUT CHART----->#
# Fold dataset against demographic and other pct
Demographicdf <- gather(state_data, Demographic, Dem_pop, White:Foreign_born, factor_key=TRUE)
Demographicdf <- Demographicdf[order(Demographicdf$state, Demographicdf$Demographic),]

#<-----LINE CHART----->#
# Fold dataset against party votes
Partydf <- gather(state_data, Party_Name, Vote_Count, Rep_12:Other_16, factor_key=TRUE)
Partydf <- Partydf[order(Partydf$state, Partydf$Party_Name),]

Partydf$Year = 0

# Set to year values
for (i in 1:length(Partydf$Party_Name)) {
  if (str_sub(Partydf$Party_Name[i], -3, -1) == "_12") {
    Partydf$Year[i] = 2012
  } else {
    Partydf$Year[i] = 2016
  }
}

Partydf$PartyName = 0

# Set to party name values
for (i in 1:length(Partydf$Party_Name)) {
  if (str_sub(Partydf$Party_Name[i], 1, 3) == "Dem") {
    Partydf$PartyName[i] = "DEM"
  } else if (str_sub(Partydf$Party_Name[i], 1, 3) == "Rep") {
    Partydf$PartyName[i] = "REP"
  } else {
    Partydf$PartyName[i] = "OTH"
  }
}

Partydf$Party_Name = Partydf$PartyName
Partydf = Partydf[-c(26)]

# Write to csv
write.csv(Demographicdf, "folded_demographic_data.csv")
write.csv(Partydf, "folded_party_data.csv")

# Open data with weight and map position values
state_weights = read.csv("stateWeights.csv", header=TRUE)
longLatStates = read.csv("statesLongLat.csv", header=TRUE)

# Bind weights to state
weight_data <- cbind(longLatStates, state_weights)
weight_data = weight_data[-c(5, 7:10)]

# Write to csv
write.csv(weight_data, "cleanedWeightStates.csv")

# Demographic votes analysis
demVotes = read.csv("DemographicVotes.csv", header=TRUE)
# By demographic/Sex out of citizen population - how many registered to vote - how many actually voted


# Voter turnout overtime
line = read.csv("VoterTurnout.csv", header=TRUE)
line = subset(line, line$Category != "Eligible Voters")
line$Value = line$Percentage.of.Eligible.Voters
line = line[-c(1, 5)]
write.csv(line, "VoterTurnout.csv")
