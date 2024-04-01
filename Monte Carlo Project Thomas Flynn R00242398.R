

## Question 1 part a
set.seed(250)  # seed for reproducibility

# information from question
n_samples <- 250
n_intervals <- 50
mean_interior_1 <- 0.5
sd_interior_1 <- 0.2
min_interior_2 <- 0
max_interior_2 <- 1
clog_limit <- 130
n_simulations <- 10000

# Function to simulate one interval of internal buildup
simulate_internal_buildup <- function(mean_interior, sd_interior=NULL, uniform=FALSE) {
  if (uniform) {
    return(sum(runif(n_samples, min_interior_2, max_interior_2)))  # Uniform distribution
  } else {
    return(sum(rnorm(n_samples, mean_interior, sd_interior)))  # Normal distribution
  }
}

# Function to simulate clogs for n_intervals
simulate_clogs <- function(mean_interior, sd_interior=NULL, uniform=FALSE) {
  clogs <- sum(replicate(n_intervals, simulate_internal_buildup(mean_interior, sd_interior, uniform) > clog_limit))
  return(clogs)
}

# simulating for both engineer estimations
clogs_1_results <- replicate(n_simulations, simulate_clogs(mean_interior_1, sd_interior_1))
clogs_2_results <- replicate(n_simulations, simulate_clogs(mean_interior_1, uniform=TRUE))

# caluclting the average number of clogs across all simulations
mean_clogs_1 <- mean(clogs_1_results)
mean_clogs_2 <- mean(clogs_2_results)

# the results of both engineers
cat("Engineer 1 Estimation - Average number of clogs across", n_simulations, "simulations:", mean_clogs_1, "\n")
cat("Engineer 2 Estimation - Average number of clogs across", n_simulations, "simulations:", mean_clogs_2, "\n")





############### Question 1 part b
#install.packages("ggplot2")
library(ggplot2)
#  parameters for exterior buildup
mean_external <- 0.7  
sd_external <- 0.4  
# function to simulate one interval of external buildup
simulate_external_buildup <- function(mean_external, sd_external) {
  return(sum(rnorm(n_samples, mean_external, sd_external)))  # Normal distribution
}
# simulating buildup for 1 interval across 10,000 simulations
interior_buildup_simulations_1 <- replicate(n_simulations, simulate_internal_buildup(mean_interior_1, sd_interior_1))

#Simulating buildup for 1 interval across 10,000 simulations
interior_buildup_simulations_2 <- replicate(n_simulations, simulate_internal_buildup(n_samples, min_interior_2, max_interior_2))

#simulating exterior buildup across 10,000 simulations
exterior_buildup_simulations <- replicate(n_simulations, simulate_external_buildup(mean_external, sd_external))

# Interior buildup distribution plot for Engineer 1 model
ggplot() +  ##this is for engineer 1 (not needed just curious)
  geom_histogram(aes(interior_buildup_simulations_1), bins = 30, fill="blue", alpha=0.5) +
  ggtitle("Interior Buildup Distribution for One Interval (Engineer 1's Model)") +
  xlab("Buildup(mg)") + 
  ylab("Frequency")

# Interior buildup distribution plot for Engineer 2 model
ggplot() + 
  geom_histogram(aes(interior_buildup_simulations_2), bins = 30, fill="green", alpha=0.5) +
  ggtitle("Interior Buildup Distribution for One Interval (Engineer 2's Model)") +
  xlab("Buildup(mg)") + 
  ylab("Frequency")

# Exterior  distribution plot
ggplot() + 
  geom_histogram(aes(exterior_buildup_simulations), bins = 30, fill="red", alpha=0.5) +
  ggtitle("Exterior Buildup Distribution for One Interval") +
  xlab("Buildup(mg)") + 
  ylab("Frequency")






############### Question 1 part C
contamination_limit <- 180  # the threshold for external contamination

# Function to simulate contamination
simulate_external_simulation <- function() {
  # Simulate external buildup for 1 simulation
  external_buildup <- rnorm(n_samples, mean_external, sd_external)
  total_external <- sum(external_buildup)
  
  # seeing if the contamination limit is exceeded
  return(total_external > contamination_limit)
}

# Simulating contamination events
contamination_events <- sum(replicate(n_simulations, simulate_external_simulation()))

#the likelihood of contamination
likelihood_contamination <- contamination_events / n_simulations

#the result of the likelihood
cat("Estimated Likelihood of Needle Contamination in a Simulation:", likelihood_contamination, "\n")


# for Engineer 1
# the function to simulate one simulation for Engineer 1 and check for clogging
simulate_clogging_1 <- function() {
  #simulate internal buildup for 1 simulation
  interior_buildup <- rnorm(n_samples, mean_interior_1, sd_interior_1)
  total_interior <- sum(interior_buildup)
  
  #seeif the clogging limit is exceeded
  return(total_interior > clog_limit)
}

# multiple simulations and count clogging events for Engineer 1
clogging_events_1 <- sum(replicate(n_simulations, simulate_clogging_1()))

#the likelihood of clogging for Engineer 1
likelihood_clogging_1 <- clogging_events_1 / n_simulations

# the result for Engineer 1
cat("Estimated Likelihood of Needle Clogging (Engineer 1) in a Simulation:", likelihood_clogging_1, "\n") ##this is for engineer 1 (not needed just curious)


# Parameters for Engineer 2

# the function to simulate one simulation for Engineer 2 and check for clogging
simulate_clogging_2 <- function() {
  #simulate internal buildup for 1 simulation
  interior_buildup <- runif(n_samples, min_interior_2, max_interior_2)
  total_interior <- sum(interior_buildup)
  
  #see if the clogging limit is exceeded
  return(total_interior > clog_limit)
}

#multiple simulations and count clogging events for Engineer 2
clogging_events_2 <- sum(replicate(n_simulations, simulate_clogging_2()))

# the likelihood of clogging for Engineer 2
likelihood_clogging_2 <- clogging_events_2 / n_simulations

# the result for Engineer 2
cat("Estimated Likelihood of Needle Clogging (Engineer 2) in a Simulation:", likelihood_clogging_2, "\n")

##### Question 1 part D

#function to simulate tests before an event
simulate_tests_before_event <- function(n, limit, mean = NULL, sd = NULL, min = NULL, max = NULL, is_uniform = FALSE) {
  total_buildup <- 0
  num_tests <- 0
  while (total_buildup <= limit) {
    #add buildup depending on the distribution type
    if (is_uniform) {
      # if uniform distribution is specified
      buildup <- runif(1, min, max)
    } else {
      # use normal distribution if not uniform
      buildup <- rnorm(1, mean, sd)
    }
    total_buildup <- total_buildup + buildup
    num_tests <- num_tests + 1
  }
  return(num_tests)
}

#simulate the number of tests before clogging for Engineer 1
tests_before_clogging_1 <- replicate(n_simulations, simulate_tests_before_event(n_samples, clog_limit, mean = mean_interior_1, sd = sd_interior_1))

#simulate the number of tests before clogging for Engineer 2
tests_before_clogging_2 <- replicate(n_simulations, simulate_tests_before_event(n_samples, clog_limit, min = min_interior_2, max = max_interior_2, is_uniform = TRUE))

#simulate the number of tests before contamination
tests_before_contamination <- replicate(n_simulations, simulate_tests_before_event(n_samples, contamination_limit, mean = mean_external, sd = sd_external))

# the mean number of tests before clogging for Engineer 1
mean_tests_clogging_1 <- mean(tests_before_clogging_1)

#results for Engineer 1
cat("Engineer 1 - Mean number of tests before clogging:", mean_tests_clogging_1, "\n") #this is for engineer 1 (not needed just curious)

# Calculate the mean number of tests before clogging for Engineer 2
mean_tests_clogging_2 <- mean(tests_before_clogging_2)

#results for Engineer 2
cat("Engineer 2 - Mean number of tests before clogging:", mean_tests_clogging_2, "\n")

# the mean number of tests before contamination
mean_tests_contamination <- mean(tests_before_contamination)

#results for contamination
cat("Contamination - Mean number of tests before event:", mean_tests_contamination, "\n")



 ### data frames for each event
data_clogging_1 <- data.frame(Tests = tests_before_clogging_1, Event = "Clogging (Engineer 1)") #this is for engineer 1 (not needed just curious)
data_clogging_2 <- data.frame(Tests = tests_before_clogging_2, Event = "Clogging (Engineer 2)")
data_contamination <- data.frame(Tests = tests_before_contamination, Event = "Contamination")

#combining data frames
data <- rbind(data_clogging_1, data_clogging_2, data_contamination)


#plots for each event with mean line
p1 <- ggplot(data_clogging_1, aes(x = Tests)) + #this is for engineer 1 (not needed just curious)
  geom_histogram(binwidth = 1, bins=50, position = "identity", alpha = 0.6, fill = "blue") +
  geom_vline(aes(xintercept = mean(Tests)), color = "black", linetype = "dashed", linewidth = 1) +
  labs(title = "Engineer 1 - Distribution of Tests Before Clogging", x = "Number of Tests", y = "Frequency") +
  theme_minimal()

p2 <- ggplot(data_clogging_2, aes(x = Tests)) +
  geom_histogram(binwidth = 1, bins=50, position = "identity", alpha = 0.6, fill = "green") +
  geom_vline(aes(xintercept = mean(Tests)), color = "black", linetype = "dashed", linewidth = 1) +
  labs(title = "Engineer 2 - Distribution of Tests Before Clogging", x = "Number of Tests", y = "Frequency") +
  theme_minimal()

p3 <- ggplot(data_contamination, aes(x = Tests)) +
  geom_histogram(binwidth = 1, bins=50, position = "identity", alpha = 0.6, fill = "red") +
  geom_vline(aes(xintercept = mean(Tests)), color = "black", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Tests Before Contamination", x = "Number of Tests", y = "Frequency") +
  theme_minimal()

#show plots
p1
p2
p3



################################# Question 2

# Parameters
cost_per_contaminated_sample <- 500
cost_per_clogged_needle <- 10000
cost_per_non_tested_sample <- 300
revenue_per_test <- 100



# Function to simulate debris buildup
simulate_debris <- function(tests, mean, sd, distribution = "normal") {
  if (distribution == "normal") {
    return(cumsum(rnorm(tests, mean, sd)))
  } else if (distribution == "uniform") {
    return(cumsum(runif(tests, min_interior_2, max_interior_2)))
  } else {
    stop("Invalid distribution")
  }
}

# Function to calculate profit
calculate_profit <- function(tests, distribution = "normal") {
  exterior_debris <- simulate_debris(tests, mean_external, sd_external)
  interior_debris <- simulate_debris(tests, mean_interior_1, sd_interior_1, distribution)
  revenue <- 0
  cost_contamination <- 0
  cost_clogging <- 0
  cost_non_tested <- 0
  contamination_started <- FALSE
  
  for(i in 1:tests) {
    if(contamination_started) {
      cost_contamination <- cost_contamination + cost_per_contaminated_sample
      #no revenue for contaminated tests
    } else {
      if(exterior_debris[i] > 180) {
        cost_contamination <- cost_contamination + cost_per_contaminated_sample
        contamination_started <- TRUE
        #take away revenue if this test was initially counted as successful
        if (i > 1) {
          revenue <- revenue - revenue_per_test
        }
      } else {
        revenue <- revenue + revenue_per_test
      }
    }
    if(interior_debris[i] > clog_limit) {
      cost_clogging <- cost_per_clogged_needle
      cost_non_tested <- (tests - i) * cost_per_non_tested_sample
      #take away revenue for the clogged test and break
      if (i > 1) {
        revenue <- revenue - revenue_per_test
      }
      break
    }
  }
  total_profit <- revenue - cost_contamination - cost_clogging - cost_non_tested
  return(total_profit)
}




simulate_profits <- function(tests, distribution = "normal") {
  # Simulate profits across multiple simulations and calculate average profit
  profits <- replicate(n_simulations, calculate_profit(tests, distribution))
  return(mean(profits))
}

# Define the range of tests to simulate
test_range <- 1:280  # this range as could see from grapphs wouldnt be after 280

# getting average profits for each number of tests in the range
#average_profits_normal <- sapply(test_range, simulate_profits, distribution = "normal") this is for engineer 1 (no needed just curious)
average_profits_uniform <- sapply(test_range, simulate_profits, distribution = "uniform")

# Finding optimal number of tests and corresponding profits
#optimal_tests_normal <- test_range[which.max(average_profits_normal)]   #this is for engineer 1 (not needed just curious)
#optimal_profit_normal <- max(average_profits_normal)   ##this is for engineer 1 (not needed just curious)
optimal_tests_uniform <- test_range[which.max(average_profits_uniform)]
optimal_profit_uniform <- max(average_profits_uniform)

# Oshowing the the optimal results
optimal_results <- list(
  #optimal_tests_normal = optimal_tests_normal,  ##this is for engineer 1 (no needed just curious)
  #optimal_profit_normal = optimal_profit_normal, ###this is for engineer 1 (no needed just curious)
  optimal_tests_uniform = optimal_tests_uniform, 
  optimal_profit_uniform = optimal_profit_uniform
)

optimal_results

