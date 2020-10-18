library(dplyr)

rm(list=ls())
county_data = read.csv("usElection2016.csv", header=TRUE)
county_data$party = "NONE"

drop = c("romney12", "obama12", "otherpres12", "repgov14", "demgov14", "county",
         "fips", "othergov14", "demsen16", "repsen16", "othersen16", "demhouse16", 
         "rephouse16", "otherhouse16", "demgov16", "repgov16", "othergov16")

county_data = county_data[,!(names(county_data) %in% drop)]

# Convert all pct values to num then calculate pct from total

state_data = county_data %>% group_by(state) %>%
  summarize(trump16 = sum(trump16), clinton16 = sum(clinton16), otherpres16 = sum(otherpres16),
            total_population = sum(total_population), cvap = sum(cvap), median_hh_inc = sum(median_hh_inc),
            ruralurban_cc = sum(ruralurban_cc), party = "NONE")

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

write.csv(state_data, "state_data.csv")
