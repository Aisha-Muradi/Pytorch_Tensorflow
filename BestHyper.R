
library(dplyr)
library(purrr)
library(readr)

#directory for the file
data_directory_count <- " " # The count resulted generated by Neural Network would be inserted here
data_directory_rate <- " " ## The rate resulted generated by Neural Network would be inserted here

#list of all csv files
file_list_count <- list.files(data_directory_count, pattern = "*.csv", full.names = TRUE)
file_list_rate <- list.files(data_directory_rate, pattern = "*.csv", full.names = TRUE)

#combining all the files into one frame
combined_data_count <- file_list_count %>%
  map_dfr(~ read_csv(.))

combined_data_rate <- file_list_rate %>%
  map_dfr(~ read_csv(.))

#sorted_df <- combined_data_count %>%
#  arrange(desc(Prediction))

sorted_df <- combined_data_count[order(combined_data_count$BestHyper), ]
top_10 <- head(sorted_df, 10)


#writing a cvs 
write_cvs <- (combined_data_count ,  "combined_data_count")
write_cvs <- (combined_data_rate ,  "combined_data_rate")

frequency(combined_data_count$BestHyper)
most_frequent <- names(which.max(frequency))

print(top_10)
