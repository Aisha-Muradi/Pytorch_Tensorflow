library(readr)
library(dplyr)
library(data.table)

read_predictions <- function(path, type) {
  #path - directory containing the files
  #type - either national or state
  # CSV files list
  file_names <- list.files(path, pattern = " ", full.names = TRUE) # the patterns of files inserted inside " " which would be .cvs
  
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
    
    #rename the prediction column
    # names(selected_data)[6] <- "Prediction"
    # if (type == "rate"){
    #   names(selected_data)[6] <- "Prediction_Rate"
    # } else if (type == "count"){
    #   names(selected_data)[6] <- "Prediction_Count"
    # } else if (type == "state"){
    #   names(selected_data)[6] <- "Prediction_state"
    # } else if (type == "national"){
    #   names(selected_data)[6] <- "Prediction_National"
    # }
    
    data_list[[length(data_list) + 1]] <- selected_data
  }

# Combine all data frames into one
  data <- bind_rows(data_list)
  return (data)
}

# Directory containing the files

#pytorch national rate
path_pytorch_national_rate <- " " # insert the file between " " 
pytorch_rate_results <- read_predictions(path_pytorch_national_rate, "rate")
pytorch_rate_results <- pytorch_rate_results[pytorch_rate_results$Model == "neural",]
pytorch_rate_results$Prediction[pytorch_rate_results$Prediction < 0] <- 0
names(pytorch_rate_results)[which(names(pytorch_rate_results) == "Prediction")] <- "Prediction_Rate_PyTorch"

#pytorch national count
path_pytorch_national_count <- " " # insert the file between " " 
pytorch_count_results <- read_predictions(path_pytorch_national_count, "count")
pytorch_count_results <- pytorch_count_results[pytorch_count_results$Model == "neural",]
pytorch_count_results$Prediction[pytorch_count_results$Prediction < 0] <- 0
names(pytorch_count_results)[which(names(pytorch_count_results) == "Prediction")] <- "Prediction_Count_PyTorch"

#tensorflow state
path_state <- " "
state_results <- read_predictions(path_state, "state")

#tensorflow national
path_national <- " " # insert the file between " " 
national_rate_results <- read_predictions(path_national, "national")
national_rate_results <- national_rate_results[national_rate_results$Model == "neural",]
national_rate_results$Prediction[national_rate_results$Prediction < 0] <- 0
names(national_rate_results)[which(names(national_rate_results) == "Prediction")] <- "Prediction_Rate_Tensorflow"

path_national <- " " # insert the file between " " 
national_count_results <- read_predictions(path_national, "national")
national_count_results <- national_count_results[national_count_results$Model == "neural",]
national_count_results$Prediction[national_count_results$Prediction < 0] <- 0
names(national_count_results)[which(names(national_count_results) == "Prediction")] <- "Prediction_Count_Tensorflow"

#merge the four dataframes into one 
# merged_models_results <- merge(pytorch_rate_results,pytorch_count_results,state_results,national_results, by = c("Year","Lag","Window","FIPS"), all = TRUE)
merged_models_results <- merge(pytorch_rate_results, pytorch_count_results, by = c("Year","Model","Lag","Window","FIPS"))
merged_models_results <- merge(merged_models_results, national_rate_results, by = c("Year","Model","Lag","Window","FIPS"))
merged_models_results <- merge(merged_models_results, national_count_results, by = c("Year","Model","Lag","Window","FIPS"))
# merged_models_results <- rbind(pytorch_rate_results, pytorch_count_results)
# merged_models_results <- rbind(merged_models_results, national_rate_results)
# merged_models_results <- rbind(merged_models_results, national_count_results)

#read the original data 
original_data_path <- " " # insert the file between " " 
original_data <- read_csv(original_data_path, show_col_types = FALSE)
original_data <- original_data[,c("FIPS", "Year","Population","overdose_death_rate","Deaths")]


full_results <- merge(original_data, merged_models_results, by = c("FIPS", "Year"), all = TRUE)


full_results <- full_results %>%
  mutate(
    Absolute_Pytorch_Error_Rate = abs(overdose_death_rate - Prediction_Rate_PyTorch),
    Absolute_Pytorch_Error_Count = abs(Deaths - Prediction_Count_PyTorch),
    Absolute_Tensorflow_Error_Rate = abs(overdose_death_rate - Prediction_Rate_Tensorflow),
    Absolute_Tensorflow_Error_Count = abs(Deaths - Prediction_Count_Tensorflow),
  )

# Calculate difference in Absolute Error, Count - Rate
full_results <- full_results %>%
  mutate(
    Pytorch_Minus_Tensorflow_AbsoluteError_Rate = Absolute_Pytorch_Error_Rate - Absolute_Tensorflow_Error_Rate,
    Pytorch_Minus_Tensorflow_AbsoluteError_Count = Absolute_Pytorch_Error_Count - Absolute_Tensorflow_Error_Count,
  )

write.csv(full_results, " ") # the file name you want to create will be inserted inside " "
