library(tidyverse)

initial_data <- read_csv("1-data/1-sample_data.csv")
additional_data <- read.csv("1-data/2-additional_data.csv")
additional_features <- read_csv("1-data/3-additional_features.csv")

head(initial_data)
haed(additional_data)
head(additional_features)

concatenated_data <- bind_rows(initial_data, additional_data)
full_data <- inner_join(concatenated_data, additional_features, by = "id")
write_csv(concatenated_data, "1-data/concatenated_data.csv")
write_csv(full_data, "1-data/full_data.csv")
