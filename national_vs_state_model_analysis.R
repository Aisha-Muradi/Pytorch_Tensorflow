library(readr)
library(dplyr)
library(data.table)

read_predictions <- function(path, type){
  #path - directory containing the files
  #type - either national or state
  # CSV files list
  file_names <- list.files(path, pattern = " ", full.names = TRUE)
  
  # Define column types if known
  column_types <- cols(
    Model = col_character(),
    State = col_character(),
    BestHyper = col_character(),
    Year = col_double(),
    Lag = col_double(),
    Window = col_double(),
    PriorYears = col_double(),
    FIPS = col_double(),
    Prediction = col_double()
  )
  
  # columns to keep
  columns_to_keep <- c("Model", "Year", "Lag", "Window", "FIPS", "Prediction")
  
  # Initialize an empty list for data frames
  data_list <- list()
  
  # For loop to process each file
  for (file_name in file_names) {
    data <- read_csv(file_name, col_types = column_types, show_col_types = FALSE)
    selected_data <- select(data, all_of(columns_to_keep))
    data_list[[length(data_list) + 1]] <- selected_data
  }
  
  
  # Combine all data frames into one
  data <- bind_rows(data_list)
  
  #rename the prediction column
  if (type == "state")
  {
    names(data)[6] <- "Prediction_State"
  }
  else
  {
    names(data)[6] <- "Prediction_National"
  }
  return(data)
}

# Directory containing the files
path_state <- " "
state_results <- read_predictions(path_state, "state")

path_national <- " "
national_results <- read_predictions(path_national, "national")


#merge the two dataframes into one 
merged_models_results <- merge(state_results,national_results, by = c("Model", "Year","Lag","Window","FIPS"), all = TRUE)

#read the original data 
original_data_path <- " "
original_data <- read_csv(original_data_path, show_col_types = FALSE)
original_data <- original_data[c("FIPS", "Year","Population","overdose_death_rate","Deaths")]

#Final dataset with all the models results and original dataset results
#From here now I can calculate the MAE

#Drop linear model in full results?
#Drop 2023-2024?
full_results <- merge(original_data, merged_models_results, by = c("FIPS", "Year"), all = TRUE)

#Calculate the Absolute Error
#National Models
absolute_error_national <- abs(full_results$overdose_death_rate-full_results$Prediction_National)
full_results$Absolute_Error_National <- absolute_error_national


#State Models
absolute_error_state <- abs(full_results$overdose_death_rate-full_results$Prediction_State)
full_results$Absolute_Error_State <-absolute_error_state

#Calculate difference in Absolute Error, National - State
absolute_error_difference <- full_results$Absolute_Error_National-full_results$Absolute_Error_State
full_results$Absolute_Error_National_minus_State <- absolute_error_difference

#calculate difference in RMAE, national - state 
#RMAE_difference <-full_results$

write.csv(full_results, " ")


#remove the na rows?
full_results <- full_results[full_results$Model != "linear",]
full_results <- na.omit(full_results)

write.csv(full_results, " ")

setDT(full_results)  # Convert to data.table if it's not already

#calculate mean absolute error of the difference
# Calculate mean absolute error of the difference, grouped by specified variables
processed <- full_results[, .(MAE_Difference = mean(Absolute_Error_National_minus_State, na.rm = TRUE)), 
                          by = .(Model, Lag, Window, Year)]

#anova on difference of errors
# Convert Lag and Window to factors
processed$Lag <- factor(processed$Lag)
processed$Window <- factor(processed$Window)


#fit the anova model with rate as both Predictiontype and outcometype
# model <- aov(MAE_Difference ~ Model*Lag*Window, data = processed)
model <- aov(MAE_Difference ~ Model + Lag + Window, data = processed)

summary(model)


#Post hoc analysis: Tukey Method
tukey_results <- TukeyHSD(model)
tukey_results

