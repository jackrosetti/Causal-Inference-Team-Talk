library(ggplot2)

# Set the random seed for reproducibility
set.seed(123)

# Function to simulate PSAT and SAT scores with more noise for regression discontinuity design
simulate_psat_sat_scores <- function(n_obs, max_value, cutoff) {
  psat_scores <- runif(n_obs, min = 800, max = max_value)
  treatment <- ifelse(psat_scores >= cutoff, 1, 0)
  noise_sd <- 100  # Increase the standard deviation of noise for more variation
  sat_scores <- psat_scores + 100 * treatment + rnorm(n_obs, mean = 0, sd = noise_sd)  # Increase effect size to 50
  data.frame(PSAT_Score = psat_scores, SAT_Score = sat_scores, Treatment = treatment)
}

# Simulate PSAT and SAT scores with more noise and a bigger positive effect for the treated group
n_obs <- 1000
max_value <- 1600
cutoff <- 1200
psat_sat_data <- simulate_psat_sat_scores(n_obs, max_value, cutoff)

# Separate data points for treated and control groups
data_treated <- subset(psat_sat_data, Treatment == 1)
data_control <- subset(psat_sat_data, Treatment == 0)

# Calculate the cutoff effect
cutoff_tolerance <- 20
cutoff_effect <- mean(data_treated$SAT_Score[data_treated$PSAT_Score >= cutoff - cutoff_tolerance & data_treated$PSAT_Score < cutoff + cutoff_tolerance]) -
  mean(data_control$SAT_Score[data_control$PSAT_Score >= cutoff - cutoff_tolerance & data_control$PSAT_Score < cutoff + cutoff_tolerance])

# Fit quadratic regression lines for treated and control groups
lm_treated <- lm(SAT_Score ~ PSAT_Score, data = data_treated)
lm_control <- lm(SAT_Score ~ PSAT_Score, data = data_control)

# Visualize the RDD plot with quadratic regression lines and cutoff effect label
ggplot(psat_sat_data, aes(x = PSAT_Score, y = SAT_Score)) +
  geom_point() +
  geom_vline(xintercept = cutoff, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", formula = y ~ x , data = data_treated, 
              se = FALSE, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ x , data = data_control, 
              se = FALSE, color = "green") +
  labs(title = "Effect of SAT Prep Class on SAT Score",
       x = "PSAT Score",
       y = "SAT Score",
       subtitle = paste("Cutoff Effect:", round(cutoff_effect, 2))) +  # Display the cutoff effect label
  theme_minimal()
