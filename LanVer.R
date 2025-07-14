# Load required packages
library(tidyverse)
library(ggplot2)
library(ggplot)
# Load dataset
dataset <- read.csv("C:/Users/lanvu/Downloads/vaers_final.csv")

# --- Summary of numeric variables ---
cat("\n📈 Summary of numeric variables:\n")
print(summary(dataset %>% select(where(is.numeric))))

# --- Age Distribution ---
if ("AGE_YRS" %in% colnames(dataset)) {
  ggplot(dataset, aes(x = AGE_YRS)) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
    labs(title = "Age Distribution", x = "Age (Years)", y = "Count")
}

# --- Sex breakdown ---
if ("SEX" %in% colnames(dataset)) {
  ggplot(dataset, aes(x = SEX)) +
    geom_bar(fill = "tomato", color = "black") +
    labs(title = "Gender Breakdown", x = "Sex", y = "Count")
}

# ---  Top 10 vaccine types ---
if ("VAX_TYPE" %in% names(dataset)) {
  dataset %>%
    count(VAX_TYPE = tolower(VAX_TYPE), sort = TRUE) %>%
    slice_head(n = 10) %>%
    ggplot(aes(x = reorder(VAX_TYPE, n), y = n)) +
    geom_col(fill = "seagreen") +
    coord_flip() +
    labs(title = "Top 10 Vaccine Types", x = "Vaccine Type", y = "Count")
}

# ---  Top 10 most common symptoms ---
if ("CLEANED_SYMPTOMS" %in% names(dataset)) {
  top_symptoms <- dataset %>%
    filter(CLEANED_SYMPTOMS != "") %>%
    pull(CLEANED_SYMPTOMS) %>%
    str_split(",") %>%
    unlist() %>%
    str_trim() %>%
    tolower() %>%
    table() %>%
    sort(decreasing = TRUE)
  
  barplot(head(top_symptoms, 10),
          col = "steelblue", las = 2,
          main = "Top 10 Reported Symptoms",
          ylab = "Frequency")
}
