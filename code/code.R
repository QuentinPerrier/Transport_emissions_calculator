library(googleway)
library(ggmap)
library(gmapsdistance)
library(geosphere)
library(tidyverse)

cities <- read_csv2("inputs/Database.csv")

API_key <- "AIzaSyCV-Ktuho7nVNUbwkTMvOs-K5__keL_yzQ" #Enter you own Google API key here
#API key can be obtained freely here: https://developers.google.com/maps/documentation/javascript/get-api-key

set.api.key(API_key)


# Get transit time to conference --------------------------------------------

get_time <- function(city, country) {
  
  #' get the transit time in hour (train/bus) between Paris and the conference city
  
  destination <- # input for 'gmapsdistance' is required in the format "City+Country"
    paste(city, country, sep = "+") %>% 
    str_replace_all(" ", "")
  
  result <- gmapsdistance(origin = "Paris+France", 
                destination = destination, 
                mode = "transit")
  time <- ifelse(result$Status == "OK", result$Time / 3600, NA)
  return(time)
}

cities$transit_time <- map2_dbl(cities$City, cities$Country, get_time)


# Estimate transportation mode --------------------------------------------

estimate_transportation_mode <- function(time) {
  #' a function to assume the mode
  #' time is the travel time, in hours
  transport <- case_when(
    time < 0.1 ~ "walking",
    time < 5 ~ "train",
    time >= 5 ~ "plane", #Sets the limit between flights and train. Here, beyond 5 hours of road/train transit, we assume people take a flight
    is.na(time) ~ "plane")
}

cities$transportation_mode <- estimate_transportation_mode(cities$transit_time)


# Compute distance for flights ------------------------------------------------------------------

register_google(key = API_key)
lab_geocode <- geocode("17 avenue de la Belle Gabrielle, Nogent-sur-Marne, France")

get_distance <- function(City, Country) {
  #' Compute distance from lab in kilometers
  conference_location <- paste(City, Country, sep = ", ") # Location for 'geocode' is required in the format "City, Country"
  conference_geocode <- geocode(conference_location) #Get lon/lat
  d <- distm(lab_geocode, conference_geocode, fun = distHaversine)[[1,1]] / 1000
  return(d)
}

get_distance("Cardiff", "Royaume-Uni")

plane_trips <- cities %>% 
  filter(transportation_mode == "plane") %>% 
  mutate(flight_distance = map2_dbl(City, Country, get_distance))


# Estimate emissions ------------------------------------------------------

factors <- read_csv2("inputs/Emission_factors.csv", comment = "#")

compute_emissions <- function(distance) {
  #' input: distance in kilometers
  #' output: emissions in tCO2e
  emissions <- case_when(
    distance < 1000 ~ factors$Facteur[1] * distance,
    distance < 2000 ~ factors$Facteur[2] * distance,
    distance < 3000 ~ factors$Facteur[3] * distance,
    distance >= 3000 ~ factors$Facteur[4] * distance
  )
  emissions_AR <- emissions * 2 # aller-retour
  emissions_AR <- emissions_AR / 10^6 # to get output in tonnes instead of grammes of CO2e
  return(emissions_AR)
}


plane_trips$emissions <- map_dbl(plane_trips$flight_distance, compute_emissions)

results <- left_join(cities, 
                     plane_trips %>% select(COMM_ID, flight_distance, emissions), 
                     by = "COMM_ID")

dir.create("outputs")
saveRDS(results, "outputs/results.rds")

write.table(results, "outputs/results.csv", 
            sep = ";", dec = ",", row.names = F)



# Analysis -----------------------------------------------------------------

results <- readRDS("outputs/results.rds")

n_colloques <- nrow(results)
n_flights <- sum(results$transportation_mode == "plane")
share_flights <- round(n_flights * 100 / n_colloques,0)

global_carbon_budget <- sum(results$emissions, na.rm = T) %>% round(0)
avg_emissions <- round(sum(results$emissions, na.rm = T) / nrow(results), 2)

#Variations annuelles
results %>% 
  filter(transportation_mode == "plane") %>% 
  group_by(Year) %>% 
  summarise(emissions = sum(emissions),
            n_conf = n()) %>% 
  ggplot(aes(x = Year, y = emissions)) + 
  geom_bar(stat = "identity") +
  geom_point(aes(y = n_conf)) +
  labs(x = 'Annee', y = 'Emissions (tCO2e)', title = "Emissions annuelles") +
  theme_bw() 


#Intensité annuelle (emissions divisée par le nombre de conférences)
results %>% 
  group_by(Year) %>% 
  summarise(emissions = sum(emissions, na.rm = T),
            n_conf = n(),
            avg_emissions = emissions / n_conf) %>% 
  ggplot(aes(x = Year, y = avg_emissions)) +
  geom_bar(stat = "identity") +
  labs(x = 'Year', 
       y = 'Emission intensity (tCO2e/conf)', 
       title = "Annual emissions") +
  theme_bw() 

# Year and destination
results %>% 
  filter(transportation_mode == "plane") %>%
  group_by(Country, Year) %>% 
  summarise(emissions = sum(emissions)) %>% 
  group_by(Year) %>% 
  mutate(annual_share = emissions / sum(emissions)) %>% 
  ggplot(aes(x = Year, 
             y = emissions,
             fill = Country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(annual_share > 0.1, Country, "")), position = position_stack(vjust = 0.5)) +
  labs(x = "Country", y = "Emissions (tCO2)", title = "Emissions by destination") +
  theme_bw() +
  guides(fill=FALSE)

#Groupement par destination
results %>% 
  filter(transportation_mode == "plane") %>%
  group_by(Country) %>% 
  summarise(emissions = sum(emissions)) %>% 
  mutate(share = 100 * emissions / sum(emissions)) %>% 
  ggplot(aes(x = reorder(Country, emissions), 
             y = emissions)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(share, 2), "%"), y = emissions + 3), 
            size = 3) +
  labs(x = "Country", y = "Emissions (tCO2)", title = "Emissions by destination") +
  coord_flip() +
  theme_bw()

# Write report -----------------------------------------------------------------

rmarkdown::render('code/report.Rmd', output_file='../outputs/report.pdf')


