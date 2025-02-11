install.packages("tidyverse")
install.packages("nycflights13")
install.packages("remotes")
library(tidyverse)
library(nycflights13)
remotes::install_github("SATVILab/UtilsDataRSV")

##### Display the flights dataset in an alternative form #####
flights_tib <- as_tibble(flights) #already a tibble so using it makes another tibble
flights_tib

##### Rewrite code using dplyr and pipe #####
# flight1 <- flights[flights$month == 1, ] #select rows on condition -> filter()
# carrier_vec <- unique(flight1$carrier) # Gets the carrier names
# carrier_dist_vec_mean <- numeric(length(carrier_vec)) # Makes vector with space
# carrier_dist_vec_sd <- numeric(length(carrier_vec)) # Makes vector with space
# for (i in seq_along(carrier_vec)) { #gets mean and SD for each carrier
#   carrier_dist_vec_mean[i] <- mean(
#     flight1$distance[flight1$carrier == carrier_vec[i]]
#   )
#   carrier_dist_vec_sd[i] <- sd(
#     flight1$distance[flight1$carrier == carrier_vec[i]]
#   )
# }
# dist_tbl <- tibble( #makes dataframe and immediately makes into tibble essentially
#   carrier = carrier_vec,
#   mean_distance = carrier_dist_vec_mean,
#   sd_distance = carrier_dist_vec_sd
# )
# dist_tbl[order(dist_tbl$mean_distance), ] #displays in ascending order of mean dist

# essentially get mean and sd for flights in 1st month then display in ascending order
dist_tbl <- flights |> filter(month == 1) |> 
  group_by(carrier) |>
  summarise(
    mean_distance = mean(distance, na.rm = TRUE), 
    sd_distance = sd(distance, na.rm= TRUE)
    ) |> 
  arrange(mean_distance) #does everything besides displaying 
dist_tbl #displays

##### Explain SD #####
#Explain SD = 0 using code
# Idea is that there is only 1 flight path therefore no difference in distance 
carriers_zero <- c("YV", "F9", "AS", "HA")
flights |> filter(month == 1, carrier %in% carriers_zero) |>
  group_by(carrier) |> 
  summarize(
    num_unique_routes = n_distinct(paste(origin, dest, sep = "-")),
    route = unique(paste(origin, dest, sep = "-"))
    )
#paste(x, y) allows you to join the two columns into a single string

#Explain SD = NA using code
flights |> filter(month == 1, carrier=="OO") |> 
  group_by(carrier) |> 
  count(carrier, name = "number_of_flights")
# count() cant be used inside summarise because it isnt aggregation and works over the whole dataset




##### Construct df where carriers = columns, rows = avg dep_delay in each month ####
#Get the data then pivot it
delay_tbl <- flights |> group_by(month, carrier) |> 
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    .groups = "drop"
    ) #must drop groups at end or else it's a weird tibble
delay_tbl

#pivot the data
delay_wide <- delay_tbl |> 
  pivot_wider(
    names_from = carrier, 
    values_from = avg_delay
    )
# names_from is where the column names come from, values_from are where the column data comes from. Everything else becomes row data 
delay_wide

##### Proportion #####
flights |> drop_na(dep_delay, arr_delay) |> 
  summarize(
    num_flights = n(), 
    num_recovered = sum(dep_delay>0 & arr_delay <=0),
    proportion_delayed_arrived_on_time = num_recovered/num_flights
  )
#drop_na gets rid of na in the params

##### Step-by-step #####
#Step 1

#Filter so we only have multi-carrier routes
multi_carrier_routes <- flights %>% group_by(origin, dest) %>% filter(n_distinct(carrier) > 1) %>% ungroup()
multi_carrier_routes

#I think this is what they ask for in step 1
#routes <- multi_carrier_routes |>group_by(origin, dest) |> select(origin, dest) |> distinct(origin, dest, .keep_all = TRUE) |> summarize(num_flights = n(origin, dest))
routes <- multi_carrier_routes |> 
  group_by(origin, dest) |> 
  summarize(num_flights = n(), .groups = 'drop')
routes

multi_carrier_routes |> 
  group_by(origin, dest) |> 
  summarize(num_carriers = n_distinct(carrier), .groups = 'drop') |> 
  filter(num_carriers > 1)

#Step 2
arr_delay_carrier <- multi_carrier_routes |> group_by(origin, dest, carrier) |> summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE), .groups = "drop") |> left_join(airlines, by = "carrier")
airline_names <- arr_delay_carrier |> select(name) |> distinct(name)
airline_names

#Step 3
best_worst_delay <- arr_delay_carrier |> group_by(origin, dest) |> summarize(
  best_delay = min(avg_arr_delay, na.rm = TRUE),
  worst_delay = max(avg_arr_delay, na.rm = TRUE),
  best_airline = name[which.min(avg_arr_delay)],
  worst_airline = name[which.max(avg_arr_delay)],
  delay_diff = worst_delay - best_delay,
  .groups = "drop"
)

# best_worst_delay |> summarise(max_delay_diff = max(delay_diff, na.rm = TRUE))
# best_worst_delay |> filter(delay_diff == max(delay_diff, na.rm = TRUE))

best_worst_airline <- best_worst_delay |> select(origin, dest, best_airline, worst_airline)
best_worst_airline

#Step 4
greatest_diff <- best_worst_delay |> arrange(desc(delay_diff)) |> filter(delay_diff == max(delay_diff, na.rm = TRUE)) |> select(origin, dest, best_airline, worst_airline, delay_diff)
greatest_diff

#Step 5: determine reason
# Hint: Look at how many flights the airlines did of that route 
# Should find that the worst airline did that route once, and the best did it more frequently 
#Look for 9E and EV in the thing

#airlines_of_intered

multi_carrier_routes |> left_join(airlines, by = "carrier") |> group_by(origin, dest, carrier ,name) |> summarize(flight_count = n(), .groups = "drop") |> filter(origin == "JFK", dest == "ATL", carrier %in% c("9E", "EV"))


##### Missing entries and inconsistencies #####
##### read data ####
weird_data <- structure(list(id = c("id_1", "id_2", "id_3", "id_4", "id_5", 
                      "id_6", "id_7", "id_8", "id_9", "id_10", "id_11", "id_12", "id_13", 
                      "id_14", "id_15", "id_16", "id_17", "id_18", "id_19", "id_20", 
                      "id_21", "id_22", "id_23", "id_24", "id_25", "id_26", "id_27", 
                      "id_28", "id_29", "id_30", "id_31", "id_32", "id_33", "id_34", 
                      "id_35", "id_36", "id_37", "id_38", "id_39", "id_40", "id_41", 
                      "id_42", "id_43", "id_44", "id_45", "id_46", "id_47", "id_48", 
                      "id_49", "id_50"), age = c(50L, 34L, 70L, 33L, 22L, 61L, 69L, 
                                                 73L, 62L, 56L, 71L, 33L, 73L, 44L, 45L, 46L, 24L, 70L, 46L, 76L, 
                                                 47L, 76L, 28L, 48L, 54L, 27L, 45L, 26L, 61L, 28L, 38L, 55L, 33L, 
                                                 36L, 62L, 58L, 72L, 31L, 34L, 51L, 61L, 64L, 26L, 28L, 60L, 29L, 
                                                 42L, 46L, 79L, 72L), gender = c("male", "male", "male", "female", 
                                                                                 "female", "male", "female", "male", "male", "female", "female", 
                                                                                 "male", "male", "female", "male", "male", "male", "male", "female", 
                                                                                 "male", "male", "male", "male", "female", "femal", "male", "female", 
                                                                                 "female", "female", "female", "male", "female", "female", "female", 
                                                                                 "male", "male", "female", "male", "female", "female", "male", 
                                                                                 "female", "female", "male", "male", "female", "male", "male", 
                                                                                 "male", "female"), height = c(174.4, 197.7, 174.1, 194.5, NA, 
                                                                                                               180.4, 170.5, 157.4, 196.8, 165.1, 153, 197.4, 186, 157.1, 177.5, 
                                                                                                               197.7, 179.3, 170.2, 182.4, NA, 165.4, 161, 168.5, 199.2, 157.7, 
                                                                                                               154.6, 157.1, 184.5, 181, 194.6, 183.6, 186.9, 176.1, 183, 191.1, 
                                                                                                               189.3, 199, 172, 165.6, 170.5, 150.5, 159.2, 192.1, 161.6, 162, 
                                                                                                               153.8, 162.3, 186.6, 192.4, 174.9), weight = c(69.4, 62.3, 55.6, 
                                                                                                                                                              69.5, 78.6, 60.8, 72.2, 60.9, 75.1, 67.7, 82.5, 68.7, 67.8, 76.7, 
                                                                                                                                                              87, 61.1, 70.6, 63.3, 81.5, 59.2, 93.2, 87.3, 83.4, 80.9, 68.6, 
                                                                                                                                                              76.5, 93.7, 79.1, 92, 65.6, 85.4, 63.3, 79.7, 74.1, 63.3, 78.2, 
                                                                                                                                                              95.7, 95.1, 63.7, 66.1, 99.3, 81, 96.9, 73.3, 70.3, 83, 57.6, 
                                                                                                                                                              78.6, 61.9, 98.1), blood_type = c("O", "A", "O", "O", "B", "AB", 
                                                                                                                                                                                                "O", "O", "O", "AB", "A", "O", "O", "O", "B", "A", "B", "AB", 
                                                                                                                                                                                                "O", "AB", "A", "AB", "O", "B", "A", "A", "B", "AB", "A", "B", 
                                                                                                                                                                                                "B", "A", "O", "O", "O", "B", "O", "A", "A", "B", "A", "O", "AB", 
                                                                                                                                                                                                "A", "A", "O", "O", "B", "A", "O"), disease_status = c("diseased", 
                                                                                                                                                                                                                                                       "healthy", "healthy", "healthy", "healthy", "healthy", "diseased", 
                                                                                                                                                                                                                                                       "healthy", "diseased", "Healthy", "diseased", "healthy", "diseased", 
                                                                                                                                                                                                                                                       "healthy", "diseased", "healthy", "healthy", "healthy", "healthy", 
                                                                                                                                                                                                                                                       "healthy", "healthy", "diseased", "healthy", "diseased", "healthy", 
                                                                                                                                                                                                                                                       "healthy", "healthy", "healthy", "diseased", "diseased", "healthy", 
                                                                                                                                                                                                                                                       "healthy", "healthy", "diseased", "diseased", "diseased", "healthy", 
                                                                                                                                                                                                                                                       "diseased", "healthy", "healthy", "healthy", "healthy", "healthy", 
                                                                                                                                                                                                                                                       "diseased", "diseased", "diseased", "healthy", "healthy", "diseased", 
                                                                                                                                                                                                                                                       "diseased"), cholesterol = c(228, 223, 213, 198, 166, 151, 195, 
                                                                                                                                                                                                                                                                                    199, 189, 196, 221, 156, 185, 230, 234, 174, 185, 236, 235, 180, 
                                                                                                                                                                                                                                                                                    165, 220, 160, 153, 250, 153, 184, 242, 212, 179, 224, 233, 181, 
                                                                                                                                                                                                                                                                                    199, 220, 214, 214, 248, 191, 162, 203, 173, 199, 187, 248, 189, 
                                                                                                                                                                                                                                                                                    173, 212, 164, 247), glucose = c(96, 78, 101, 119, 103, 91, 86, 
                                                                                                                                                                                                                                                                                                                     NA, 77, 80, 115, 85, 88, 109, NA, 71, 90, 94, 91, 87, 113, 93, 
                                                                                                                                                                                                                                                                                                                     97, 118, 109, 80, 85, 119, 99, 108, 89, 108, 97, 116, 79, 84, 
                                                                                                                                                                                                                                                                                                                     75, 81, 119, NA, 106, 109, 75, 82, 84, 75, 76, 120, 119, 77), 
               smoker = c("yes", "yes", "yes", "yes", "no", "yes", "no", 
                          "yes", "no", "no", "no", "no", "no", "yes", "no", "yes", 
                          "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "no", 
                          "no", "yes", "yes", "yes", "no", "no", "yes", "no", "yes", 
                          "no", "yes", "no", "yes", "yes", "yes", "no", "no", "yes", 
                          "no", "no", "no", "no", "no", "no", "yes"), exercise = c("occasional", 
                                                                                   "regular", "occasional", "regular", "none", "occasional", 
                                                                                   "regular", "none", "occasional", "none", "occasional", "none", 
                                                                                   "none", "regular", "occasional", "none", "regular", "regular", 
                                                                                   "none", "occasional", "none", "occasional", "occasional", 
                                                                                   "occasional", "regular", "occasional", "regular", "regular", 
                                                                                   "regular", "occasional", "occasional", "none", "none", "regular", 
                                                                                   "occasional", "occasional", "none", "none", "none", "none", 
                                                                                   "occasional", "regular", "regular", "none", "regular", "occasional", 
                                                                                   "occasional", "none", "occasional", "regular"), income = c(84820L, 
                                                                                                                                              81547L, 22588L, 72490L, 74533L, 25338L, 41469L, 57315L, 63629L, 
                                                                                                                                              88662L, 62615L, 56261L, 58499L, 82232L, 77584L, 77275L, 38468L, 
                                                                                                                                              54510L, 91326L, 78611L, 31402L, 29586L, 21441L, 58269L, 84173L, 
                                                                                                                                              88295L, 37940L, 43750L, 69750L, 92356L, 82518L, 91455L, 68866L, 
                                                                                                                                              51178L, 68275L, 27689L, 35418L, 81318L, 62405L, 86851L, 25654L, 
                                                                                                                                              47553L, 74474L, 51409L, 22607L, 55360L, 96351L, 21516L, 41927L, 
                                                                                                                                              55810L), education = c("master", "bachelor", "PhD", "master", 
                                                                                                                                                                     "bachelor", "highschool", "PhD", "highschool", "PhD", "PhD", 
                                                                                                                                                                     "bachelor", "highschool", "master", "bachelor", "PhD", "PhD", 
                                                                                                                                                                     "PhD", "bachelor", "master", "highschool", "PhD", "highschool", 
                                                                                                                                                                     "bachelor", "master", "highschool", "highschool", "master", 
                                                                                                                                                                     "master", "bachelor", "PhD", "highschool", "PhD", "master", 
                                                                                                                                                                     "master", "master", "PhD", "highschool", "master", "master", 
                                                                                                                                                                     "highschool", "bachelor", "highschool", "bachelor", "PhD", 
                                                                                                                                                                     "bachelor", "highschool", "master", "highschool", "bachelor", 
                                                                                                                                                                     "bachelor"), region = c("North", "South", "North", "West", 
                                                                                                                                                                                             "North", "West", "South", "South", "West", "South", "West", 
                                                                                                                                                                                             "South", "West", "East", "North", "West", "North", "North", 
                                                                                                                                                                                             "West", "North", "East", "West", "South", "North", "North", 
                                                                                                                                                                                             "East", "East", "North", "North", "West", "South", "West", 
                                                                                                                                                                                             "West", "East", "West", "North", "West", "North", "East", 
                                                                                                                                                                                             "North", "West", "South", "South", "East", "North", "West", 
                                                                                                                                                                                             "West", "East", "North", "East"), marital_status = c("divorced", 
                                                                                                                                                                                                                                                  "single", "divorced", "divorced", "divorced", "divorced", 
                                                                                                                                                                                                                                                  "divorced", "married", "divorced", "married", "divorced", 
                                                                                                                                                                                                                                                  "widowed", "married", "single", "widowed", "widowed", "single", 
                                                                                                                                                                                                                                                  "divorced", "widowed", "widowed", "single", "married", "single", 
                                                                                                                                                                                                                                                  "married", "widowed", "married", "single", "single", "widowed", 
                                                                                                                                                                                                                                                  "married", "widowed", "divorced", "single", "married", "single", 
                                                                                                                                                                                                                                                  "widowed", "widowed", "married", "widowed", "divorced", "married", 
                                                                                                                                                                                                                                                  "married", "divorced", "single", "married", "widowed", "divorced", 
                                                                                                                                                                                                                                                  "divorced", "single", "divorced")), row.names = c(NA, -50L
                                                                                                                                                                                                                                                  ), class = c("tbl_df", "tbl", "data.frame"))


##### Analyze data #####
missing_values <- weird_data |> 
  summarise(
    across(everything(), 
           ~sum(is.na(.)), 
           .names = "missing_{.col}")
    )
missing_values
#~sum is an anonymous function and makes it so we don't need to define a function, the "." means all columns

missing_values |> pivot_longer(
  everything(), 
  names_to = "column", 
  values_to = "missing_count"
  ) |> filter(missing_count>0)

#typos have to be strings/characters, find the uniques, so check to see if a character is unique 
# Maybe do the view_cols thing instead
# typos <- weird_data |> summarize(across(where(is.character), ~list(unique(.)), values_to = "Number_of_typos", .names = "typos_{.col}"))
# typos

UtilsDataRSV::view_cols(weird_data)

# typos |> pivot_longer(everything(), names_to = "column", values_to = "typos") |> mutate(typo_count = map_int(typos, length))  |> filter(is.character & typo_count > 0) It didnt work and im not doing the approach of making a list of valid things. You can just read with viewcols


