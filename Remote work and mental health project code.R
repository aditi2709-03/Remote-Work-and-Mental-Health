#installing required packages 
library(tidyverse)
install.packages("psych") 
library(psych)

#uploading dataset
df <- read.csv("D:/Data Sc/R/Impact_of_Remote_Work_on_Mental_Health.csv")
describe(df)
#reading data 
head(df)
summary(df)
str(df)

#finding and cleaning null values 
is.na(df)
data_clean <- na.omit(df)


# visualisation 

# checking for outliers
boxplot(data_clean$Hours_Worked_Per_Week, main="Hours Worked Per Week")
boxplot(data_clean$Social_Isolation_Rating, main="Social Isolation Rating")


# Distribution of Age
ggplot(data_clean, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title="Age Distribution", x="Age", y="Frequency")

# Gender distribution
ggplot(data_clean, aes(x = Gender)) +
  geom_bar(fill = "lightgreen") +
  labs(title="Gender Distribution", x="Gender", y="Count")

# Work Location distribution
ggplot(data_clean, aes(x = Work_Location)) +
  geom_bar(fill = "lightcoral") +
  labs(title="Work Location Distribution", x="Work Location", y="Count")

# Work-Life Balance Rating
ggplot(data_clean, aes(x = as.factor(Work_Life_Balance_Rating))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Work-Life Balance Ratings",
       x = "Work-Life Balance Rating", 
       y = "Count") +
  theme_minimal()


# Count occurrences of each category in Access_to_Mental_Health_Resources
data_summary <- data_clean %>%
  count(Access_to_Mental_Health_Resources) %>%
  mutate(percentage = n / sum(n) * 100) # Calculate percentage

# Pie chart
ggplot(data_summary, aes(x = "", y = percentage, fill = Access_to_Mental_Health_Resources)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert bar chart to pie chart
  labs(title = "Access to Mental Health Resources", fill = "Access") +
  theme_void() +  # Removes axes and labels for a clean pie chart look
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))  # Display percentages




# Distribution of Job Roles
ggplot(data_clean, aes(x = Job_Role)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Job Roles", x = "Job Role", y = "Count") +
  theme_minimal()

# Distribution of Industries
ggplot(data_clean, aes(x = Industry)) +
  geom_bar(fill = "darkcyan") +
  labs(title = "Distribution of Industries", x = "Industry", y = "Count") +
  theme_minimal()


# Work Location and Stress Level
ggplot(data_clean, aes(x = Work_Location, fill = Stress_Level)) +
  geom_bar(position = "dodge") +
  labs(title="Work Location vs Stress Level", x="Work Location", y="Count")

# Work Life Balance and Mental Health Condition
ggplot(data_clean, aes(x = as.factor(Work_Life_Balance_Rating), fill = Mental_Health_Condition)) +
  geom_bar(position = "dodge") +
  labs(title="Work Life Balance vs Mental Health Condition", x="Work Life Balance Rating", y="Count")

# Hours Worked Per Week vs Productivity Change
ggplot(data_clean, aes(x = Productivity_Change, y = Hours_Worked_Per_Week, fill = Productivity_Change)) +
  geom_boxplot() +
  labs(title="Hours Worked vs Productivity Change", x="Productivity Change", y="Hours Worked Per Week")


# Stress Level by Job Role
ggplot(data_clean, aes(x = Job_Role, fill = Stress_Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Stress Level by Job Role", x = "Job Role", y = "Count", fill = "Stress Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mental Health Condition by Job Role
ggplot(data_clean, aes(x = Job_Role, fill = Mental_Health_Condition)) +
  geom_bar(position = "dodge") +
  labs(title = "Mental Health Condition by Job Role", x = "Job Role", y = "Count", fill = "Mental Health Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Productivity Change by Work Location
ggplot(data_clean, aes(x = Work_Location, fill = Productivity_Change)) +
  geom_bar(position = "dodge") +
  labs(title = "Productivity Change by Work Location", x = "Work Location", y = "Count", fill = "Productivity Change") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Years of Experience vs Satisfaction with Remote Work
ggplot(data_clean, aes(x = Years_of_Experience, y = Satisfaction_with_Remote_Work)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Years of Experience vs. Satisfaction with Remote Work", 
       x = "Years of Experience", 
       y = "Satisfaction with Remote Work") +
  theme_minimal()


#multivariate analysis
ggplot(data_clean, aes(x = Age, fill=Mental_Health_Condition)) +
  geom_bar() +
  facet_wrap(~Work_Location) +
  labs(title="Work Location Distribution", x="Work Location", y="Count")


#performing chi-squared test 1
# Set the variables for analysis
work_location <- data_clean$Work_Location
mental_health_condition <- data_clean$Mental_Health_Condition

# Create a contingency table
contingency_table <- table(work_location, mental_health_condition)

# Perform the Chi-squared test
chi_squared_test <- chisq.test(contingency_table)

# Display the results
print(chi_squared_test)




#performing chi-squared test 2
# Set the variables for analysis
work_location <- data_clean$Work_Location
stress_level <- data_clean$Stress_Level

# Create a contingency table
contingency_table <- table(work_location, stress_level)

# Perform the Chi-squared test
chi_squared_test <- chisq.test(contingency_table)

# Display the results
print(chi_squared_test)



#performing chi-squared test 3
# Set the variables for analysis
mental_health_condition <- data_clean$Mental_Health_Condition
stress_level <- data_clean$Stress_Level

# Create a contingency table
contingency_table <- table(mental_health_condition, stress_level)

# Perform the Chi-squared test
chi_squared_test <- chisq.test(contingency_table)

# Display the results
print(chi_squared_test)


#performing chi-squared test 4
# Set the variables for analysis
mental_health_condition <- data_clean$Mental_Health_Condition
Access_to_Mental_Health_Resources <- data_clean$Access_to_Mental_Health_Resources

# Create a contingency table
contingency_table <- table(work_location, stress_level)

# Perform the Chi-squared test
chi_squared_test <- chisq.test(contingency_table)

# Display the results
print(chi_squared_test)





# applying machine learning models for prediction

#installing packages 
install.packages("caret") # For model training and evaluation
install.packages("randomForest") # For Random Forest model


# Load required libraries
library(randomForest)
library(caret)

# Data preprocessing
data_clean$Mental_Health_Condition_Binary <- ifelse(data_clean$Mental_Health_Condition != "None", 1, 0)
data_clean$Work_Location_Remote <- ifelse(data_clean$Work_Location == "Remote", 1, 0)

# Select relevant features
features <- c("Work_Location_Remote", "Hours_Worked_Per_Week", "Number_of_Virtual_Meetings",
              "Work_Life_Balance_Rating", "Stress_Level", "Social_Isolation_Rating",
              "Satisfaction_with_Remote_Work", "Company_Support_for_Remote_Work",
              "Physical_Activity", "Sleep_Quality")

# Prepare the dataset
model_data <- data_clean %>%
  select(Mental_Health_Condition_Binary, all_of(features)) %>%
  na.omit()

# Convert categorical variables to factors
model_data$Mental_Health_Condition_Binary <- as.factor(model_data$Mental_Health_Condition_Binary)
model_data$Stress_Level <- as.factor(model_data$Stress_Level)
model_data$Physical_Activity <- as.factor(model_data$Physical_Activity)
model_data$Sleep_Quality <- as.factor(model_data$Sleep_Quality)

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(model_data$Mental_Health_Condition_Binary, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Train the Random Forest model
rf_model <- randomForest(Mental_Health_Condition_Binary ~ ., data = train_data, ntree = 500, importance = TRUE)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)

# Calculate accuracy
accuracy <- sum(predictions == test_data$Mental_Health_Condition_Binary) / nrow(test_data)
print(paste("Accuracy:", round(accuracy, 4)))

# Generate confusion matrix
conf_matrix <- confusionMatrix(predictions, test_data$Mental_Health_Condition_Binary)
print(conf_matrix)

# Display variable importance
importance_df <- importance(rf_model) %>% 
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  arrange(desc(MeanDecreaseGini))

print("Variable Importance:")
print(importance_df)

# Plot variable importance
ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Variables", y = "Mean Decrease in Gini", title = "Variable Importance in Random Forest Model") +
  theme_minimal()

