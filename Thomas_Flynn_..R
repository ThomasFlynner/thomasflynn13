install.packages("readr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("DescTools")
install.packages("ggcorrplot")
install.packages("corrplot")
library(corrplot)
install.packages("ggplot2")
library("ggplot2")
library("ggcorrplot")
library("readxl")      # For reading Excel files
library("readr")       # For reading CSV files
library("dplyr")       # For data manipulation
library("tidyverse")   # A collection of packages for data science
library("DescTools")   # For working with descriptive statistics and handling missing values
library("tools")

# Set the working directory
setwd("C:/Users/Tomfl/Downloads/DATA8802/Data Science & Analytics")
# Read the CSV file into a data frame
data <- read.csv("Credit_Risk_32_final.csv")
########################################################### part a)
# Display an overview of the data
View(data)# view the dataset
summary(data)# summary stats for the data
str(data) # see the structure of the dataset
head(data)
# summary stats for numerical variables
num_summary <- summary(data[, sapply(data, is.numeric)])
print(num_summary)    # Print these
# Making sure residence time in current district is non-negative by converting to absolute values
data$Residence.Time.In.current.district <- abs(data$Residence.Time.In.current.district)


# capitalizing the first letter of all the character columns
# selecting all character columns
character_cols <- names(data)[sapply(data, is.character)]
data[character_cols] <- lapply(data[character_cols], function(col) {
  return(toTitleCase(col))
})

# Replace empty cells with NA
data <- data %>% mutate_all(~ ifelse(. == "", NA, .))
# getting a count of missing values in each column
missing_count <- colSums(is.na(data))
print(missing_count)

# Replacing the missing values in Personal_Status with the mode of the column
data$Personal_Status[is.na(data$Personal_Status)] <- Mode(data$Personal_Status, na.rm = TRUE)

# Replacing the missing values in employment column with the mode
data$Employment[is.na(data$Employment)] <- Mode(data$Employment, na.rm = TRUE)

# Replacing the missing values in credit standing with the mode
data$Credit.Standing[is.na(data$Credit.Standing)] <- Mode(data$Credit.Standing, na.rm = TRUE)

# Replacing the missing values in the column Housing with the mode
data$Housing[is.na(data$Housing)] <- Mode(data$Housing, na.rm = TRUE)

# Checking for duplicates
duplicates <- duplicated(data)

# Check if all rows are FALSE (no duplicates)
no_duplicates <- all(!duplicates)
#if else state to print if there are duplicates or not
if (no_duplicates) {
  cat("No duplicates found in the dataset.\n")
} else {
  cat("Duplicates found in the dataset.\n")
}


#boxplots of all the numerical variables to check for outliers etc.
boxplot(data$Age, main="Boxplot of Age")
data$Age[data$Age == 151] <- NA  # replace 151 with the median as 151 and still working is clearly an entry error.
data$Age[is.na(data$Age)] <- median(data$Age, na.rm = TRUE)

boxplot(data$Months.since.Checking.Acct.opened, main="Boxplot of Months since Checking Acct opened")

boxplot(data$Credibility_score, main="Boxplot of Credibility Score")

boxplot(data$Residence.Time.In.current.district, main="Boxplot of Residence Time In current district")

boxplot(data$check, main="Boxplot of check")



#For all categorical variables vs credit standing
#Create contingency table
# Perform chi-squared test
# Barplot to visualize the relationship

# Checking Account vs Credit Standing
CheckingAcct_CreditTable <- table(data$Checking.Acct, data$Credit.Standing) # Create contingency table
CheckingAcct_Test <- chisq.test(CheckingAcct_CreditTable) # Perform chi-squared test
print(CheckingAcct_Test) # Print test result
print(CheckingAcct_CreditTable)
ggplot(data, aes(x = Checking.Acct, fill = Credit.Standing)) +  ### Barplot to visualize the relationship
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Checking Account vs Credit Standing", x = "Checking Account", y = "Proportion")

# Credit History vs Credit Standing
#Create contingency table
# Perform chi-squared test
# Barplot to visualize the relationship
CreditHistory_CreditTable <- table(data$Credit.History, data$Credit.Standing)
CreditHistory_Test <- chisq.test(CreditHistory_CreditTable)
print(CreditHistory_Test)
print(CreditHistory_CreditTable)
ggplot(data, aes(x = Credit.History, fill = Credit.Standing)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Credit History vs Credit Standing", x = "Credit History", y = "Proportion")

# Loan Reason vs Credit Standing
#Create contingency table
# Perform chi-squared test
# Barplot to visualize the relationship
LoanReason_CreditTable <- table(data$Loan.Reason, data$Credit.Standing)
LoanReason_Test <- chisq.test(LoanReason_CreditTable)
print(LoanReason_Test)
print(LoanReason_CreditTable)
ggplot(data, aes(x = Loan.Reason, fill = Credit.Standing)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Loan Reason vs Credit Standing", x = "Loan Reason", y = "Proportion")

#Savings Account vs Credit Standing
#Create contingency table
# Perform chi-squared test
# Barplot to visualize the relationship
SavingsAcct_CreditTable <- table(data$Savings_Acct, data$Credit.Standing)
SavingsAcct_Test <- chisq.test(SavingsAcct_CreditTable)
print(SavingsAcct_Test)
print(SavingsAcct_CreditTable)
ggplot(data, aes(x = Savings_Acct, fill = Credit.Standing)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Savings Account vs Credit Standing", x = "Savings Account", y = "Proportion")

# Employment vs Credit Standing
#Create contingency table
# Perform chi-squared test
# Barplot to visualize the relationship
Employment_CreditTable <- table(data$Employment, data$Credit.Standing)
Employment_Test <- chisq.test(Employment_CreditTable)
print(Employment_Test)
print(Employment_CreditTable)
ggplot(data, aes(x = Employment, fill = Credit.Standing)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Employment vs Credit Standing", x = "Employment", y = "Proportion")

# Personal Status vs Credit Standing
#Create contingency table
# Perform chi-squared test
# Barplot to visualize the relationship
PersonalStatus_CreditTable <- table(data$Personal_Status, data$Credit.Standing)
PersonalStatus_Test <- chisq.test(PersonalStatus_CreditTable)
print(PersonalStatus_Test)
print(PersonalStatus_CreditTable)
ggplot(data, aes(x = Personal_Status, fill = Credit.Standing)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Personal Status vs Credit Standing", x = "Personal Status", y = "Proportion")

# Housing vs Credit Standing
#Create contingency table
# Perform chi-squared test
# Barplot to visualize the relationship
Housing_CreditTable <- table(data$Housing, data$Credit.Standing)
Housing_Test <- chisq.test(Housing_CreditTable)
print(Housing_Test)
print(Housing_CreditTable)
ggplot(data, aes(x = Housing, fill = Credit.Standing)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Housing vs Credit Standing", x = "Housing", y = "Proportion")

# Job Type vs Credit Standing
#Create contingency table
# Perform chi-squared test
# Barplot to visualize the relationship
JobType_CreditTable <- table(data$Job.Type, data$Credit.Standing)
JobType_Test <- chisq.test(JobType_CreditTable)
print(JobType_Test)
print(JobType_CreditTable)
ggplot(data, aes(x = Job.Type, fill = Credit.Standing)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Job Type vs Credit Standing", x = "Job Type", y = "Proportion")

# Foreign National vs Credit Standing
#Create contingency table
# Perform chi-squared test
# Barplot to visualize the relationship
ForeignNational_CreditTable <- table(data$Foreign.National, data$Credit.Standing)
ForeignNational_Test <- chisq.test(ForeignNational_CreditTable)
print(ForeignNational_Test)
print(ForeignNational_CreditTable)
ggplot(data, aes(x = Foreign.National, fill = Credit.Standing)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Foreign National vs Credit Standing", x = "Foreign National", y = "Proportion")

# Registered State vs Credit Standing
#Create contingency table
# Perform chi-squared test
# Barplot to visualize the relationship
RegState_CreditTable <- table(data$Reg.State, data$Credit.Standing)
RegState_Test <- chisq.test(RegState_CreditTable)
print(RegState_Test)
print(RegState_CreditTable)
ggplot(data, aes(x = Reg.State, fill = Credit.Standing)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Registered State vs Credit Standing", x = "Registered State", y = "Proportion")

# Convert all character columns to factors
data[] <- lapply(data, function(x) if(is.character(x)) factor(x) else x)
# Convert the Credit.Standing column to an ordered factor 
data$Credit.Standing <- factor(data$Credit.Standing, levels = c("Bad", "Good"), ordered = TRUE)
# Change from a factor to numeric values where Bad = 1 and Good = 2
data$Credit.Standing <- as.numeric(data$Credit.Standing)
#identify all numerical columns into the variable called num_cols
num_cols <- names(data)[sapply(data, is.numeric)]
# Calculate the Spearman correlation matrix
spearman_corr_matrix <- cor(data[, sapply(data, is.numeric)], method = "spearman")

# Create a correlation plot using Spearman correlation 
corrplot(spearman_corr_matrix, method = "square", type = "upper", order = "hclust",
         tl.cex = 0.7, tl.col = "black", tl.srt = 35, 
         addCoef.col = "black", number.cex = 0.9)

# Install + load the GGally package
install.packages("GGally")
library(GGally)

# identify all numerical columns into the variable selected_data
selected_data <- data[, sapply(data, is.numeric)]

# Create a pairs plot using Spearman's correlation
correlation_matrix <- ggpairs(selected_data,
                              upper = list(continuous = wrap("cor", method = "spearman", size = 4)),
                              lower = list(continuous = wrap("points", size = 1)),
                              diag = list(continuous = wrap("densityDiag"))
)

# Print the correlation matrix
print(correlation_matrix)

# Scatterplot of Credit Standing against age with regression line
ggplot(data, aes(x = Age, y = as.numeric(Credit.Standing))) + 
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(x = "Age", y = "Credit Standing", title = "Scatterplot of Credit Standing against Age") +
  theme_minimal()

# Scatterplot of Credit Standing against Credibility Score with regression line
ggplot(data, aes(x = Credibility_score, y = as.numeric(Credit.Standing))) + 
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(x = "Credibility Score", y = "Credit Standing", title = "Scatterplot of Credit Standing against Credibility Score") +
  theme_minimal()

# Scatterplot of Credit Standing against Check with regression line
ggplot(data, aes(x = check, y = as.numeric(Credit.Standing))) + 
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(x = "Check", y = "Credit Standing", title = "Scatterplot of Credit Standing against Check") +
  theme_minimal()

# improved scatterplot of age against credibility score with regression line
ggplot(data, aes(x = Age, y = Credibility_score)) + 
  geom_point(size = 2) +  # size of points
  geom_smooth(method = "lm", col = "red", size = 0.8) +  # making regression line thinner
  labs(x = "Age", y = "Credibility Score", title = "Scatterplot of Age against Credibility Score") +
  theme_minimal() 





#testing for normalility on the numerical columns individually
shapiro.test(data$Age)
shapiro.test(data$Months.since.Checking.Acct.opened)
shapiro.test(data$Residence.Time.In.current.district)
shapiro.test(data$Credibility_score)
shapiro.test(data$check)


######Commented out all these t-tests as data was not normal in the end


#t_test_result <- t.test(Age ~ Credit.Standing, data = data)
#print(t_test_result)
# T-test for 'Variable1' against 'Credit.Standing'
#t_test_Variable1 <- t.test(Months.since.Checking.Acct.opened ~ Credit.Standing, data = data)
#print(t_test_Variable1)

# T-test for 'Variable2' against 'Credit.Standing'
#t_test_Variable2 <- t.test(Residence.Time.In.current.district ~ Credit.Standing, data = data)
#print(t_test_Variable2)

# T-test for 'Variable3' against 'Credit.Standing'
#t_test_Variable3 <- t.test(Credibility_score ~ Credit.Standing, data = data)
#print(t_test_Variable3)
# T-test for 'Variable1' against 'Credit.Standing'
#t_test_Variable4 <- t.test(check ~ Credit.Standing, data = data)
#print(t_test_Variable4)


####Used instead of t-test as data is not normal
# Mann-Whitney U test for 'Age' against 'Credit.Standing'
mw_test_Age <- wilcox.test(Age ~ Credit.Standing, data = data)
print(mw_test_Age)

# Mann-Whitney U test for Months since Checking Act opened  vs Credit.Standing
mw_test_MonthsSinceCheckingAcctOpened <- wilcox.test(Months.since.Checking.Acct.opened ~ Credit.Standing, data = data)
print(mw_test_MonthsSinceCheckingAcctOpened)

# Mann-Whitney U test for Residence Time In current district vs credit Standing
mw_test_ResidenceTimeInCurrentDistrict <- wilcox.test(Residence.Time.In.current.district ~ Credit.Standing, data = data)
print(mw_test_ResidenceTimeInCurrentDistrict)

# Mann-Whitney U test for credibility score vs credit Standing
mw_test_CredibilityScore <- wilcox.test(Credibility_score ~ Credit.Standing, data = data)
print(mw_test_CredibilityScore)

# Mann-Whitney U test for check'vs credit Standing
mw_test_Check <- wilcox.test(check ~ Credit.Standing, data = data)
print(mw_test_Check)



#Converting credit standing back to a factor
data$Credit.Standing <- factor(data$Credit.Standing, levels = c(1, 2), labels = c("Bad", "Good"))







# identifying the character columns to convert to factors
cat_cols <- names(data)[sapply(data, is.character)]

# then converting these character columns to factors
data[cat_cols] <- lapply(data[cat_cols], factor)
data$Employment <- factor(data$Employment, levels = c("Unemployed", "Very Short", "Short", "Medium", "Long", "Retired"))#, ordered = TRUE)
data$Checking.Acct <- factor(data$Checking.Acct)
data$Savings_Acct <- factor(data$Savings_Acct, levels = c("No Acct", "Low", "MedLow", "MedHigh", "High"))#, ordered = TRUE)
data$Job.Type <- factor(data$Job.Type, levels = c("Unemployed", "Unskilled", "Skilled", "Management"))#, ordered = TRUE)
data$Credit.Standing <- factor(data$Credit.Standing, levels = c("Bad", "Good"))#, ordered = TRUE)
data$Credit.History <- factor(data$Credit.History, 
                              levels = c("Delay", "Critical", "Current", "Bank Paid", "All Paid"))#, 
#ordered = TRUE)


install.packages("caret")
library(caret)
################################################################################### part b)
# Set the seed to my last 3 digigts of my student number
set.seed(398)
# Calculate the number of rows for training (75% of total rows)
train_size <- round(0.75 * nrow(data))

# Sample indices for training data
train_indices <- sample(1:nrow(data), train_size)

# Split the data into training and test sets
trainData <- data[train_indices, ]
testData <- data[-train_indices, ]

# Return the number of rows in each set
c(nrow(trainData), nrow(testData))


##################################################################################part c
# To exclude these columns converted to character
trainData$ID <- as.character(trainData$ID)
trainData$check =as.character(trainData$check) 
cat_cols <- names(trainData)[sapply(trainData, is.factor) & names(trainData) != "Credit.Standing"]


# Calculate the overall uncertainty-entropy in the Credit Standing column of the training data
target_table <- prop.table(table(trainData$Credit.Standing))
target_entropy <- sum(-target_table * log2(target_table))

# Defining a  function to calculate uncertainty for each predictor variable
calculate_entropy <- function(data, predictor, target) {
  # create a table comparing predictor with target adding a small number to avoid errors
  contingency_table <- table(data[, predictor], data[, target]) + 1e-6
  
  # calculate  percentage of each target class for each value of the predictor
  prop_table <- prop.table(contingency_table, margin = 1)
  
  # calculate uncertainty for each predictor value and addd them up.
  weighted_entropy <- sum(prop.table(table(data[, predictor])) * rowSums(-prop_table * log2(prop_table)))
  
  return(weighted_entropy)
}

# calculate the info gain for each categorical predictor.
# sapply applies the function to each predictor and stores the results.
entropy_values <- sapply(cat_cols, function(col) calculate_entropy(trainData, col, "Credit.Standing"))

# Calculate how much each predictor reduces uncertainty compared to the target.
info_gain <- target_entropy - entropy_values

# Find the predictor that reduces uncertainty the most.
best_predictor_index <- which.max(info_gain)
best_predictor <- cat_cols[best_predictor_index]
best_info_gain <- info_gain[best_predictor_index]

# Print the best predictor and how much it reduces uncertainty.
cat("Best predictor for the root node split is:", best_predictor, "\n",
    "with an information gain of:", best_info_gain, "\n")














################################################################################ part d)
# a function binary entropy for a specific level of a predictor variable
calculate_binary_entropy <- function(data, predictor, target, level) {
  # Split data into two groups based on thelevel of the predictor
  binary_split <- ifelse(data[, predictor] == level, "Level", "Not Level")
  
  # Create a contingency table with a small consstant Laplace smoothing to avoid errors
  contingency_table <- table(binary_split, data[, target]) + 1e-6
  
  # Calculate the proportions of each target class within the split groups
  prop_table <- prop.table(contingency_table, margin = 1)
  
  # Calculate the weighted entropy for the split
  weighted_entropy <- sum(prop.table(contingency_table) * rowSums(-prop_table * log2(prop_table)))
  
  return(weighted_entropy)
}

# to store information gain for each binary split of categorical variables
info_gain_df <- data.frame(Variable = character(), Level = character(), InformationGain = numeric())

# Looping over all categorical columns to calculate information gain for each level
for (predictor in cat_cols) {
  # Iterating through levels of the current categorical variable
  levels <- levels(trainData[, predictor])
  for (level in levels) {
    # Calculating the weighted entropy for the current binary split
    weighted_entropy <- calculate_binary_entropy(trainData, predictor, "Credit.Standing", level)
    
    # Information gainwhich is the difference between entropy of the target and the weighted entropy of the split
    info_gain <- target_entropy - weighted_entropy
    
    # Storing the variable level and information gain
    info_gain_df <- rbind(info_gain_df, data.frame(Variable = predictor, Level = level, InformationGain = info_gain))
  }
}

# finding the binary split with the max information gain
best_split <- info_gain_df[which.max(info_gain_df$InformationGain), ]

# the results the best variable and level for the initial split in a decision tree
cat("Best binary split for the root node split is in variable:", best_split$Variable, "\n",
    "on level:", best_split$Level, "\n",
    "with an information gain of:", best_split$InformationGain, "\n")


######### doing this part again using grouping of levels
# a function to calculate the binary entropy for grouped levels of a predictor variable
calculate_grouped_binary_entropy <- function(data, predictor, target, grouped_levels) {
  # Creating a binary split on whether the predictor variables level is in the grouped levels
  binary_split <- ifelse(data[, predictor] %in% grouped_levels, "GroupedLevel", "OtherLevels")
  
  # create a contingency table with Laplace smoothing to avoid errors
  contingency_table <- table(binary_split, data[, target]) + 1e-6
  
  #Calculate the proportions in each group for each class of the target variable
  prop_table <- prop.table(contingency_table, margin = 1)
  
  #Calaculated the weighted entropy of the split
  weighted_entropy <- sum(prop.table(contingency_table) * rowSums(-prop_table * log2(prop_table)))
  
  return(weighted_entropy)
}

# to store information gain for various grouped level binary splits
info_gain_grouped_df <- data.frame(Variable = character(), GroupedLevels = character(), InformationGain = numeric())

# Loop over each categorical predictor variable
for (predictor in cat_cols) {
  # Retrieve all levels of the current predictor variable
  levels <- levels(trainData[, predictor])
  
  # Generate all possible combinations of levels for grouping
  level_combinations <- lapply(1:(length(levels)-1), function(n) combn(levels, n, simplify = FALSE))
  level_combinations <- unlist(level_combinations, recursive = FALSE)
  
  # Iterate through each combination of grouped levels
  for (grouped_levels in level_combinations) {
    # Calculate the weighted entropy for the current group of levels
    weighted_entropy <- calculate_grouped_binary_entropy(trainData, predictor, "Credit.Standing", grouped_levels)
    
    # Calculated information gain by comparing to the overall target entropy
    info_gain <- target_entropy - weighted_entropy
    
    # storing the reuslt
    grouped_level_str <- paste(grouped_levels, collapse = ", ")
    info_gain_grouped_df <- rbind(info_gain_grouped_df, data.frame(Variable = predictor, GroupedLevels = grouped_level_str, InformationGain = info_gain))
  }
}

#finding the best group of levels for a binary split based on maximum information gain
best_grouped_split <- info_gain_grouped_df[which.max(info_gain_grouped_df$InformationGain), ]

#the results of the best variable and group of levels for the initial split
cat("Best grouped level binary split for the root node split is in variable:", best_grouped_split$Variable, "\n",
    "with grouped levels:", best_grouped_split$GroupedLevels, "\n",
    "and an information gain of:", best_grouped_split$InformationGain, "\n")

########################################################################################################## part e)
# Function to calculate entropy given the target variable
calculate_entropy <- function(target_col) {
  # Calculate proportions of each class in the target variable
  proportions <- table(target_col) / length(target_col)
  # Calculate and return entropy using the formula for entropy
  return(-sum(proportions * log2(proportions), na.rm = TRUE))
}

#calculate the entropy for the target variable CreditStanding
target_entropy <- calculate_entropy(trainData$Credit.Standing)

#to store the best split for continuous variables
best_continuous_split <- list(variable = NA, split_point = NA, information_gain = -Inf)

# Iterate over each continuous variable
for (predictor in names(trainData)[sapply(trainData, is.numeric)]) {
  #sort the values of the predictor and calculate midpoints between consecutive values
  sorted_values <- sort(unique(trainData[[predictor]]))
  midpoints <- head(sorted_values, -1) + diff(sorted_values) / 2
  
  # Check each midpoint as a potential split point
  for (midpoint in midpoints) {
    # Split the dataset into two groups based on the midpoint
    data_below <- subset(trainData, trainData[[predictor]] <= midpoint)
    data_above <- subset(trainData, trainData[[predictor]] > midpoint)
    
    # Calculate entropy for each split group
    entropy_below <- calculate_entropy(data_below$Credit.Standing)
    entropy_above <- calculate_entropy(data_above$Credit.Standing)
    # Calculate the weighted average of the two entropies
    weighted_entropy <- (nrow(data_below) * entropy_below + nrow(data_above) * entropy_above) / nrow(trainData)
    
    # Calculate the information gain from this potential split
    information_gain <- target_entropy - weighted_entropy
    
    # Check if this split is the best one so far
    if (information_gain > best_continuous_split$information_gain) {
      # Update the best split if current split is better
      best_continuous_split <- list(variable = predictor, split_point = midpoint, information_gain = information_gain)
    }
  }
}

# Print the best split found for continuous variables
cat("The best continuous variable split is:\n")
cat("Variable:", best_continuous_split$variable, "\n")
cat("Split point:", best_continuous_split$split_point, "\n")
cat("Information gain:", best_continuous_split$information_gain, "\n")


# Print the best split found for categorical variables
cat("\nThe best categorical variable split is:\n")
cat("Variable:", best_split$Variable, "\n")
cat("Level:", best_split$Level, "\n")
cat("Information gain:", best_split$InformationGain, "\n")



########################################################################################### part f)
# Function to calculate entropy for target variable credit standing
calculate_entropy <- function(data, target) {
  # Calculate proportions of each class in the target variable
  proportions <- table(data[[target]]) / length(data[[target]])
  # Calcualte and return entropy 
  return(-sum(proportions * log2(proportions), na.rm = TRUE))
}

#function to calculate entropy for a binary split of cat variables
calculate_grouped_binary_entropy <- function(data, predictor, target, grouped_levels) {
  # Split the data based on specified levels
  binary_split <- ifelse(data[[predictor]] %in% grouped_levels, "GroupedLevels", "OtherLevels")
  # Create a table counting occurrences in each split
  contingency_table <- table(binary_split, data[[target]]) + 1e-6
  #calculate and return the entropy for this binary split
  prop_table <- prop.table(contingency_table, margin = 1)
  return(sum(prop.table(contingency_table) * rowSums(-prop_table * log2(prop_table))))
}

#function to find the best split in data for max info gain
calculate_info_gain_for_binary_splits <- function(data_subset, target_variable_name, excluded_predictor, min_info_gain = 0.05) {
  # Calculate the base entropy for the subset
  target_entropy_subset <- calculate_entropy(data_subset, target_variable_name)
  
  #to store the best split found
  best_info_gain <- -Inf
  best_split <- list(variable = NA, grouped_levels = NA, split_point = NA, info_gain = NA)
  
  #which predictors to consider for splitting
  predictors <- setdiff(names(data_subset), c(target_variable_name, excluded_predictor))
  
  # Check each predictor for a potential split
  for (predictor in predictors) {
    # Process categories (factors)
    if (is.factor(data_subset[[predictor]])) {
      # Generate all possible combinations of levels
      levels <- levels(data_subset[[predictor]])
      level_subsets <- lapply(1:length(levels), function(n) combn(levels, n, simplify = FALSE))
      level_subsets <- unlist(level_subsets, recursive = FALSE)
      
      # Evaluate each combination for splitting
      for (subset_levels in level_subsets) {
        entropy_subset <- calculate_grouped_binary_entropy(data_subset, predictor, target_variable_name, subset_levels)
        info_gain <- target_entropy_subset - entropy_subset
        
        # Keep the split with the highest information gain
        if (info_gain > best_info_gain) {
          best_info_gain <- info_gain
          best_split$variable <- predictor
          best_split$grouped_levels <- subset_levels
          best_split$info_gain <- info_gain
        }
      }
    }
    # Process numeric variables
    else if (is.numeric(data_subset[[predictor]]) && !any(is.na(data_subset[[predictor]]))) {
      # Sort values and find midpoints as potential split points
      sorted_values <- sort(unique(data_subset[[predictor]]))
      if (length(sorted_values) > 1) {
        midpoints <- (head(sorted_values, -1) + tail(sorted_values, -1)) / 2
        
        # Evaluate each midpoint as a potential split
        for (midpoint in midpoints) {
          # Split the data into two groups based on the midpoint
          data_below <- data_subset[data_subset[[predictor]] <= midpoint, ]
          data_above <- data_subset[data_subset[[predictor]] > midpoint, ]
          # Calculate the entropy for each group
          entropy_below <- calculate_entropy(data_below, target_variable_name)
          entropy_above <- calculate_entropy(data_above, target_variable_name)
          # Calculate the weighted average of the entropy
          weighted_entropy <- (nrow(data_below) * entropy_below + nrow(data_above) * entropy_above) / nrow(data_subset)
          info_gain <- target_entropy_subset - weighted_entropy
          
          # Update best split if this one is better
          if (info_gain > best_info_gain) {
            best_info_gain <- info_gain
            best_split$variable <- predictor
            best_split$split_point <- midpoint
            best_split$info_gain <- info_gain
          }
        }
      }
    }
  }
  
  # Return the best split if its information gain is good enough
  return(if (best_info_gain > min_info_gain) best_split else NULL)
}

# Setup for finding the best splits in subsets of data
target_variable <- "Credit.Standing"
initial_split_variable <- "Credit.History"

# Creating subsets of data based on the initial split
subset_critical <- trainData[trainData[[initial_split_variable]] == "Critical", ]
subset_not_critical <- trainData[trainData[[initial_split_variable]] != "Critical", ]

#find the best split for the criticalsubset
best_split_critical <- calculate_info_gain_for_binary_splits(subset_critical, target_variable, initial_split_variable)

# find the best split for the not critical subset
best_split_not_critical <- calculate_info_gain_for_binary_splits(subset_not_critical, target_variable, initial_split_variable)

#the results for the critical subset
if (is.null(best_split_critical)) {
  cat("No suitable split found for 'Critical' subset\n")
} else {
  split_detail_critical <- ifelse(is.na(best_split_critical$split_point), 
                                  paste("Grouped levels:", best_split_critical$grouped_levels), 
                                  paste("Split point:", best_split_critical$split_point))
  cat("Best binary split for the 'Critical' subset is in variable:", best_split_critical$variable, 
      ", at", split_detail_critical,
      ", with an information gain of:", best_split_critical$info_gain, "\n")
}

#the results for the not critical subset
if (is.null(best_split_not_critical)) {
  cat("No suitable split found for 'Not Critical' subset\n")
} else {
  split_detail_not_critical <- ifelse(is.na(best_split_not_critical$split_point), 
                                      paste("Grouped levels:", best_split_not_critical$grouped_levels), 
                                      paste("Split point:", best_split_not_critical$split_point))
  cat("Best binary split for the 'Not Critical' subset is in variable:", best_split_not_critical$variable, 
      ", at", split_detail_not_critical,
      ", with an information gain of:", best_split_not_critical$info_gain, "\n")
}

###descision tree####

install.packages("tree")
library(tree)

set.seed(398)
# Define the formula for the decision tree
decision_tree_formula <- Credit.Standing ~ Checking.Acct + Credit.History +
  Loan.Reason + Savings_Acct + Employment + Personal_Status +
  Housing + Job.Type + Foreign.National +
  Months.since.Checking.Acct.opened + Residence.Time.In.current.district +
  Age + Credibility_score + Reg.State

# Build the decision tree model on the training data
decision_tree_model <- tree(decision_tree_formula, data = trainData)

# Plot the tree
plot(decision_tree_model)
text(decision_tree_model, pretty = 0, cex = 1)
# making the confusion matrix on the training data
train_predictions <- predict(decision_tree_model, newdata = trainData, type = "class")
train_conf_matrix <- table(Predicted = train_predictions, Actual = trainData$Credit.Standing)

#calculate accuracy on the training data
train_accuracy <- sum(diag(train_conf_matrix)) / sum(train_conf_matrix)

cat("Accuracy of the decision tree on the training set:", train_accuracy, "\n")

#predict on the test set
test_predictions <- predict(decision_tree_model, newdata = testData, type = "class")

#Create the confusion matrix
conf_matrix <- table(Predicted = test_predictions, Actual = testData$Credit.Standing)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy of the decision tree on test set:", accuracy, "\n")

print(decision_tree_model)
summary(decision_tree_model)



#Cross validation to find the optimal size for pruning based on misclassification

# Perform cross validation
set.seed(398)
cv_decision_tree <- cv.tree(decision_tree_model, K = 10, FUN = prune.misclass)

# View the cv errors for different sizes
print(cv_decision_tree)

# Plot the cv errors to determine the optimal tree size
par(mfrow = c(1, 2))
plot(cv_decision_tree$size, cv_decision_tree$dev, type = "b", 
     main = "Tree Size vs CV Error", xlab = "Number of Terminal Nodes", ylab = "CV Error")
plot(cv_decision_tree$k, cv_decision_tree$dev, type = "b", 
     main = "Complexity Parameter vs CV Error", xlab = "Complexity Parameter (k)", ylab = "CV Error")

# Prune the decision tree with the optimal size which was 12
pruned_tree_misclass <- prune.misclass(decision_tree_model, best = 12)

# Plot the pruned tree
plot(pruned_tree_misclass)
text(pruned_tree_misclass, pretty = 0, cex = 1)

# P#predict using the pruned tree on test set
pruned_test_predictions_misclass <- predict(pruned_tree_misclass, newdata = testData, type = "class")

# the confusion matrix for the pruned tree
pruned_conf_matrix_misclass <- table(Predicted = pruned_test_predictions_misclass, Actual = testData$Credit.Standing)

# the accuracy for the pruned tree on test set
pruned_accuracy_misclass <- sum(diag(pruned_conf_matrix_misclass)) / sum(pruned_conf_matrix_misclass)
cat("Accuracy of the misclass-pruned decision tree:", pruned_accuracy_misclass, "\n")
# Predict on the training set using the pruned tree
pruned_train_predictions_misclass <- predict(pruned_tree_misclass, newdata = trainData, type = "class")

# the confusion matrix for the pruned tree on the training data
pruned_train_conf_matrix_misclass <- table(Predicted = pruned_train_predictions_misclass, Actual = trainData$Credit.Standing)

# the accuracy for the pruned tree on the training data
pruned_train_accuracy_misclass <- sum(diag(pruned_train_conf_matrix_misclass)) / sum(pruned_train_conf_matrix_misclass)
cat("Accuracy of the misclass-pruned decision tree on the training data:", pruned_train_accuracy_misclass, "\n")
print(pruned_tree_misclass)


#randomForest package
install.packages("randomForest")
library(randomForest)
library(caret)
str(trainData)
summary(trainData)

# Define the formula
set.seed(398)
rf_formula <- Credit.Standing ~ Checking.Acct +
  Credit.History +
  Loan.Reason +
  Savings_Acct +
  Employment +
  Personal_Status +
  Housing +
  Job.Type +
  Foreign.National +
  Months.since.Checking.Acct.opened +
  Residence.Time.In.current.district +
  Age +
  Credibility_score +
  Reg.State


# Build the Random Forest model on the training data
rf_model <- randomForest(rf_formula, data = trainData,  ntree = 500, mtry=4, importance=TRUE)
rf_model
plot(rf_model)
importance(rf_model)
varImpPlot(rf_model)
# Predict on the test data
train_pred <- predict(rf_model, trainData)

# True values for the training data
true_values_train <- trainData$Credit.Standing

#Calculate accuracy on the training data
train_accuracy <- mean(train_pred == true_values_train)
cat("Train accuracy:", train_accuracy, "\n")
test_pred <- predict(rf_model, testData)

# True values for the test data
true_values_test <- testData$Credit.Standing

# calculate accuracy on the test data
test_accuracy <- mean(test_pred == true_values_test)
cat("Test accuracy:", test_accuracy, "\n")

#the confusion matrix on the test data
test_confusion_matrix <- table(true_values_test, test_pred)
cat("Confusion matrix for test data:\n")
print(test_confusion_matrix)

print(rf_model)





set.seed(398)
# Define the formula
rf_formula <- Credit.Standing ~  #Checking.Acct +
  Credit.History +
  Loan.Reason +
  #Savings_Acct +
  Employment +
  #Personal_Status +
  Housing +
  #Job.Type +
  #Foreign.National +
  Months.since.Checking.Acct.opened +
  #Residence.Time.In.current.district +
  Age +
  Credibility_score #+
  #Reg.State

# Define control parameters for cv
control_params <- trainControl(method = "cv", number = 10) # 10-fold cross-validation

# Define the tuning grid for Random Forest
tune_grid <- expand.grid(
  mtry = seq(2, 4, 1) # Test mtry values from 2 to 4
)

# Perform cv training of the Random Forest model
cv_rf_model <- train(
  rf_formula, 
  data = trainData, 
  method = "rf",
  trControl = control_params, 
  tuneGrid = tune_grid,
  ntree = 500 # Number of trees
)

# Print the results of the cv
print(cv_rf_model)


# Predict on the training data using the  model
train_pred_cv <- predict(cv_rf_model, trainData)
test_pred_cv <- predict(cv_rf_model, testData)

# Calculate accuracy on the training data
train_accuracy_cv <- mean(train_pred_cv == trainData$Credit.Standing)
cat("Train accuracy (CV model):", train_accuracy_cv, "\n")

# Calculate accuracy on the test data
test_accuracy_cv <- mean(test_pred_cv == testData$Credit.Standing)
cat("Test accuracy (CV model):", test_accuracy_cv, "\n")

# the confusion matrix on the test data
test_confusion_matrix_cv <- table(Actual = testData$Credit.Standing, Predicted = test_pred_cv)
cat("Confusion matrix for test data (CV model):\n")
print(test_confusion_matrix_cv)


X <- trainData[, c("Checking.Acct", 
  "Credit.History",
  "Loan.Reason",
  "Savings_Acct",
  "Employment",
  "Personal_Status",
  "Housing",
  "Job.Type",
  "Foreign.National",
  "Months.since.Checking.Acct.opened",
  "Residence.Time.In.current.district",
  "Age",
  "Credibility_score",
  "Reg.State")]
y <- trainData$Credit.Standing


# Tune the mtry parameter

tuned_mtry <- tuneRF(X, y, 
                     mtryStart = 1,
                     stepFactor = 2, 
                     ntreeTry = 500)

# The function will output the best mtry value
print(tuned_mtry)



#####################################################################################################################

set.seed(398)
# the new formula ##with GDPR
new_rf_formula <- Credit.Standing ~ Checking.Acct +
  Credit.History +
  Loan.Reason +
  Savings_Acct +
  Employment +
  Housing +
  Job.Type +
  Months.since.Checking.Acct.opened +
  Residence.Time.In.current.district +
  Credibility_score

# the new Random Forest model on the training data
new_rf_model <- randomForest(new_rf_formula, data = trainData, ntree = 500)
importance(new_rf_model)

#importance of features in the new model
new_importance <- importance(new_rf_model)

# Predict on the training data using the new model
new_train_pred <- predict(new_rf_model, trainData)

# True values for the training data
true_values_train <- trainData$Credit.Standing

# Calculate accuracy on the training data f
new_train_accuracy <- mean(new_train_pred == true_values_train)
cat("Train accuracy for the new model:", new_train_accuracy, "\n")

# Predict on the test data using the new model
new_test_pred <- predict(new_rf_model, testData)

# True values for the test data
true_values_test <- testData$Credit.Standing

# Calculate accuracy on the test data 
new_test_accuracy <- mean(new_test_pred == true_values_test)
cat("Test accuracy for the new model:", new_test_accuracy, "\n")

# the confusion matrix on the training data for the new model
new_train_confusion_matrix <- table(true_values_train, new_train_pred)
cat("Confusion matrix for training data with the new model:\n")
print(new_train_confusion_matrix)

#the confusion matrix on the test data for the new model
new_test_confusion_matrix <- table(true_values_test, new_test_pred)
cat("Confusion matrix for test data with the new model:\n")
print(new_test_confusion_matrix)


######################################################################################
set.seed(398)
# the modified new formula
new_rf_formula <- Credit.Standing ~ #checking.Acct +
  Credit.History +
  Loan.Reason +
  Savings_Acct +
  Employment +
  #Housing +
  #Job.Type +
  Months.since.Checking.Acct.opened +
  #Residence.Time.In.current.district +
  Credibility_score

# Define control parameters for cv
control_params <- trainControl(method = "cv", number = 10) # 10-fold cross-validation

# Define the tuning grid for Random Forest
tune_grid <- expand.grid(
  mtry = seq(2, 4, 1) # Test mtry values from 2 to 4
)

# Perform cv training of the Random Forest model
cv_new_rf_model <- train(
  new_rf_formula, 
  data = trainData, 
  method = "rf", 
  trControl = control_params, 
  tuneGrid = tune_grid,
  ntree = 500 # Number of trees
)

#the results of the cross-validation
print(cv_new_rf_model)

# Predict on the training and test data using the best-tuned model
train_pred_cv_new <- predict(cv_new_rf_model, trainData)
test_pred_cv_new <- predict(cv_new_rf_model, testData)

# Calculate and print accuracy on the training and test data
train_accuracy_cv_new <- mean(train_pred_cv_new == trainData$Credit.Standing)
cat("Train accuracy (CV new model):", train_accuracy_cv_new, "\n")

test_accuracy_cv_new <- mean(test_pred_cv_new == testData$Credit.Standing)
cat("Test accuracy (CV new model):", test_accuracy_cv_new, "\n")

#the confusion matrix on the test data
test_confusion_matrix_cv_new <- table(Actual = testData$Credit.Standing, Predicted = test_pred_cv_new)
cat("Confusion matrix for test data (CV new model):\n")
print(test_confusion_matrix_cv_new)


####################
#ordering ID column
data <- data[order(data$ID),]
# Calculate the overall good and bad rates
overall_good_rate <- sum(data$Credit.Standing == 'Good') / nrow(data)
overall_bad_rate <- sum(data$Credit.Standing == 'Bad') / nrow(data)

#the  rates
print(paste("Overall good rate:", overall_good_rate))
print(paste("Overall bad rate:", overall_bad_rate))

# create column to group by periods of 50 IDs:
data$Period <- cut(data$ID, breaks = seq(from = min(data$ID), to = max(data$ID), by = 50), include.lowest = TRUE)

# calculate the rates for each group
performance_by_period <- data %>%
  group_by(Period) %>%
  summarise(
    Good = sum(Credit.Standing == 'Good'),
    Bad = sum(Credit.Standing == 'Bad'),
    Total = n(),
    Good_Rate = Good / Total,
    Bad_Rate = Bad / Total
  )

# compare the rates for each period to the overall rates
performance_by_period <- performance_by_period %>%
  mutate(
    Good_Difference = Good_Rate - overall_good_rate,
    Bad_Difference = Bad_Rate - overall_bad_rate
  )

# Flag periods that are significantly different
performance_by_period <- performance_by_period %>%
  mutate(
    Flag_Good = ifelse(abs(Good_Difference) > 0.1, TRUE, FALSE),
    Flag_Bad = ifelse(abs(Bad_Difference) > 0.1, TRUE, FALSE)
  )

# view the flagged periods
flagged_periods <- performance_by_period %>%
  filter(Flag_Good | Flag_Bad)

print(flagged_periods)

# Convert to a numeric variable for plotting
performance_by_period$Period_Num <- as.numeric(as.factor(performance_by_period$Period))

# Plot with ggplot2
ggplot(data = performance_by_period, aes(x = Period_Num)) +
  #lines for Good and Bad Rates
  geom_line(aes(y = Good_Rate, color = "Good Rate"), size = 1) +
  geom_line(aes(y = Bad_Rate, color = "Bad Rate"), size = 1) +
  # Horizontal lines for overall good and bad rates
  geom_hline(yintercept = overall_good_rate, linetype = "dashed", color = "blue", size = 1) +
  geom_hline(yintercept = overall_bad_rate, linetype = "dashed", color = "red", size = 1) +
  #points for significant deviations in Good and Bad Rates
  geom_point(data = subset(performance_by_period, Flag_Good), aes(y = Good_Rate), color = "blue", size = 3, shape = 8) +
  geom_point(data = subset(performance_by_period, Flag_Bad), aes(y = Bad_Rate), color = "red", size = 3, shape = 8) +
  scale_color_manual(name = "Rate Type", values = c("Good Rate" = "green", "Bad Rate" = "orange")) +
  labs(title = "Good and Bad Credit Standing Rates by Period",
       x = "Period",
       y = "Rate") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  # Annotations for overall rates
  annotate("text", x = Inf, y = overall_good_rate, label = "Overall Good Rate", hjust = 1, vjust = -1, color = "blue", size = 3.5) +
  annotate("text", x = Inf, y = overall_bad_rate, label = "Overall Bad Rate", hjust = 1, vjust = 1, color = "red", size = 3.5) +
  scale_x_continuous(breaks = seq(min(performance_by_period$Period_Num, na.rm = TRUE), 
                                  max(performance_by_period$Period_Num, na.rm = TRUE), by = 1))


