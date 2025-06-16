# Inflation-Unemployment-India

######## DISSERTATION R SCRIPT #################################################

################################################################################
# BASELINE RESULT R CODE #
################################################################################


#------------- FIRST LAG UR, Combined INF, e-Transcations Per 100 Pop -------------------------------------------------------
library(dplyr)

Final_Monthly_Dataset <- Final_Monthly_Dataset %>%
  mutate(
    UR_lag = lag(UR, 1),
    Combined_INF_lag = lag(`Combined INF`, 1),
    eTransaction_lag = lag(`e-Transaction Per 100 Population`, 1)
  )

# View the first few rows to check the new lagged columns
head(Final_Monthly_Dataset)



#--------------- SECOND LAG UR, Combined INF, e-Transactions Per 100 Pop ----------------------------------------------------

library(dplyr)

Final_Monthly_Dataset <- Final_Monthly_Dataset %>%
  mutate(
    UR_lag2 = lag(UR, 2),
    Combined_INF_lag2 = lag(`Combined INF`, 2),
    eTransaction_lag2 = lag(`e-Transaction Per 100 Population`, 2)
  )

# View the first few rows to verify the new lagged columns
head(Final_Monthly_Dataset)

#------------- CREATE Squared Independent Term ----------------------------------

Final_Monthly_Dataset$eTrans_sq <- Final_Monthly_Dataset$`log e-Transactions`^2



#---------------------- MIXED EFFECTS MODEL ------------------------------------

# Install and load required packages if not already installed
# Install and load required packages if not already installed
if (!require(lme4)) install.packages("lme4")
if (!require(lmerTest)) install.packages("lmerTest")
if (!require(dplyr)) install.packages("dplyr")
library(lme4)
library(lmerTest)
library(dplyr)

colnames(Final_Monthly_Dataset)




####################################
# MODEL FOR UR with bootMer approach
####################################

# Fit the mixed effects model for UR with a random intercept and random slope for eTransactions by State.
model_UR <- lmer(UR ~ `log e-Transactions` + UR_lag   + SDPpercap + `Total LFPR` + `Combined INF` + `LFPR - Total Male` +
                   + `LFPR - Rural Person` + `F&B Inflation` + UR_lag2  +  (1 + `log e-Transactions` | State), data = Final_Monthly_Dataset)
summary(model_UR)

# Define a function that returns the state-specific coefficients for eTransactions
state_coef_fun <- function(fit) {
  ranef_state <- ranef(fit)$State
  fixed_effect <- fixef(fit)["`log e-Transactions`"]
  coefs <- fixed_effect + ranef_state[,"`log e-Transactions`"]
  return(coefs)
}

# Compute the state-specific coefficients from the fitted model.
state_coef_UR <- state_coef_fun(model_UR)

# Use bootMer to obtain bootstrapped estimates.
set.seed(123)  # For reproducibility
boot_results_UR <- bootMer(model_UR, FUN = state_coef_fun, nsim = 1000, use.u = TRUE, type = "parametric")

# boot_results_UR$t is a matrix with each row a bootstrap replicate and each column a state.
# Compute the bootstrap standard error for each state's coefficient.
boot_se_UR <- apply(boot_results_UR$t, 2, sd)

# Compute z-statistics and approximate p-values.
z_values_UR <- state_coef_UR / boot_se_UR
p_values_UR <- 2 * pnorm(-abs(z_values_UR))

# Instead of names(state_coef_UR), extract state names from the random effects.
states_UR <- rownames(ranef(model_UR)$State)

# Combine results into a data frame.
results_UR_boot <- data.frame(
  State = states_UR,
  State_Coefficient = state_coef_UR,
  boot_SE = boot_se_UR,
  z_value = z_values_UR,
  p_value = p_values_UR
)

cat("Bootstrapped Results for UR:\n")
print(results_UR_boot)

####################################
# MODEL FOR Combined INF with bootMer approach
####################################
colnames(Final_Monthly_Dataset)
# Fit the mixed effects model for Combined INF.
model_INF <- lmer(`Combined INF` ~ `log e-Transactions`  + Combined_INF_lag  + SDPpercap  + `Total LFPR` + `UR` +
                    `Rural INF` + `Urban INF`+ `F&B Inflation`   + (1 + `log e-Transactions` | State), data = Final_Monthly_Dataset)
summary(model_INF)

# Define a function for the state-specific coefficients for Combined INF.
state_coef_fun_INF <- function(fit) {
  ranef_state <- ranef(fit)$State
  fixed_effect <- fixef(fit)["`log e-Transactions`"]
  coefs <- fixed_effect + ranef_state[,"`log e-Transactions`"]
  return(coefs)
}

# Compute state-specific coefficients.
state_coef_INF <- state_coef_fun_INF(model_INF)

# Use bootMer to obtain bootstrapped estimates.
set.seed(123)
boot_results_INF <- bootMer(model_INF, FUN = state_coef_fun_INF, nsim = 1000, use.u = TRUE, type = "parametric")

# Calculate bootstrap standard errors.
boot_se_INF <- apply(boot_results_INF$t, 2, sd)

# Compute z-statistics and p-values.
z_values_INF <- state_coef_INF / boot_se_INF
p_values_INF <- 2 * pnorm(-abs(z_values_INF))

# Get the state names from the random effects.
states_INF <- rownames(ranef(model_INF)$State)

# Combine results into a data frame.
results_INF_boot <- data.frame(
  State = states_INF,
  State_Coefficient = state_coef_INF,
  boot_SE = boot_se_INF,
  z_value = z_values_INF,
  p_value = p_values_INF
)

cat("Bootstrapped Results for Combined INF:\n")
print(results_INF_boot)

#----------- TWO WAY FIXED EFFECTS ---------------------------------------------

# Install and load the plm package (if not already installed)
install.packages("plm")
library(plm)

# Convert your dataset into a panel data frame.
# Replace "firm" and "time" with your actual individual and time identifiers.
pdata <- pdata.frame(Final_Monthly_Dataset, index = c("Date", "State"))

colnames(pdata)

# Two-way fixed effects model for UR
model_ur <- plm(UR ~ log.e.Transactions + Combined.INF + SDPpercap  + Urban.INF 
                + LFPR...Rural.Person + LFPR...Urban.Person  + LFPR...Total.Female + Total.LFPR   +
                  Rural.F.B.Inflation  + UR_lag + UR_lag2  + Combined_INF_lag2
                + Urban.F.B.Inflation + Rural.F.B.Inflation, 
                data = pdata, 
                model = "within", 
                effect = "twoways")

summary(model_ur)

# Two-way fixed effects model for Combined INF
# If the variable name "Combined INF" contains a space, use backticks.
model_combined_inf <- plm(Combined.INF ~   log.e.Transactions + UR  + Urban.INF + Rural.INF  + LFPR...Total.Male + LFPR...Total.Female + Total.LFPR    + SDPpercap
                          + Rural.F.B.Inflation + Urban.F.B.Inflation   + Combined_INF_lag2 + Combined_INF_lag + Urban.F.B.Inflation + Rural.F.B.Inflation, 
                          data = pdata, 
                          model = "within", 
                          effect = "twoways")
summary(model_combined_inf)

# Model without eTrans
model_combined_ninf <- plm(Combined.INF ~   UR + Urban.INF + Rural.INF   + LFPR...Total.Female + Total.LFPR    + SDPpercap
                           + Rural.F.B.Inflation + Urban.F.B.Inflation   + Combined_INF_lag2 + Combined_INF_lag + Urban.F.B.Inflation + Rural.F.B.Inflation, 
                           data = pdata, 
                           model = "within", 
                           effect = "twoways")
summary(model_combined_ninf)




model_combined_Rinf <- plm(Rural.INF ~ log.e.Transactions + UR + Urban.INF + Combined.INF  LFPR...Total.Male + SDPpercap
                           + Rural.F.B.Inflation  + Combined_INF_lag + Urban.F.B.Inflation + Rural.F.B.Inflation, 
                           data = pdata, 
                           model = "within", 
                           effect = "twoways")
summary(model_combined_Rinf)

model_combined_Uinf <- plm(Urban.INF ~ log.e.Transactions + UR + Rural.INF + Combined.INF + LFPR...Urban.Person + LFPR...Total.Male + LFPR...Total.Female + Total.LFPR  + CD.Ratio + Highway.Length + SDPpercap
                           + Rural.F.B.Inflation + Urban.F.B.Inflation + UR_lag + UR_lag2 + Combined_INF_lag2 + Combined_INF_lag + Urban.F.B.Inflation + Rural.F.B.Inflation + Highway.Length, 
                           data = pdata, 
                           model = "within", 
                           effect = "twoways")
summary(model_combined_Uinf)


######### Non linear effects ###################################################

model_SqrUR <- plm(UR ~ eTrans_sq + Urban.INF + Rural.INF   + LFPR...Total.Female + Total.LFPR    + SDPpercap
                   + Rural.F.B.Inflation + Urban.F.B.Inflation   + Combined_INF_lag2 + Combined_INF_lag + Urban.F.B.Inflation + Rural.F.B.Inflation, 
                   data = pdata, 
                   model = "within", 
                   effect = "twoways")

summary(model_SqrUR)

model_SqurINF <- plm(Combined.INF ~   eTrans_sq + UR  + Urban.INF + Rural.INF  + LFPR...Total.Male + LFPR...Total.Female + Total.LFPR    + SDPpercap
                     + Rural.F.B.Inflation + Urban.F.B.Inflation   + Combined_INF_lag2 + Combined_INF_lag + Urban.F.B.Inflation + Rural.F.B.Inflation, 
                     data = pdata, 
                     model = "within", 
                     effect = "twoways")
summary(model_SqurINF)



#------------------- df DATA VISUALISATION PLOTS -------------------------------


#E-Transactions

# Load required libraries
library(dplyr)
library(ggplot2)



# Group data by Date and calculate the average e-Transaction 
avg_df <- df %>%
  group_by(Date) %>%
  summarise(avg_value = mean(`log e-Transactions`, na.rm = TRUE)) %>%
  ungroup()

# Create the time series plot of the average values
ggplot(avg_df, aes(x = Date, y = avg_value)) +
  geom_line(color = "#40E0D0", size = 1.2) +
  labs(title = "Time Series Plot of Average log(e-Transactions) Across States",
       x = "Date",
       y = "Average log(e-Transactions)") +
  theme_minimal()


# UR

# Load required libraries
library(dplyr)
library(ggplot2)



# Group data by Date and calculate the average UR across states
avg_UR_df <- df %>%
  group_by(Date) %>%
  summarise(avg_UR = mean(UR, na.rm = TRUE)) %>%
  ungroup()

# Identify the date corresponding to the maximum average UR
max_date <- avg_UR_df$Date[which.max(avg_UR_df$avg_UR)]

# Create the time series plot with a dotted vertical line at the maximum point
ggplot(avg_UR_df, aes(x = Date, y = avg_UR)) +
  geom_line(color = "#FF5733", size = 1.2) +
  geom_vline(xintercept = as.numeric(max_date), linetype = "dotted", color = "blue") +
  labs(title = "Time Series Plot of Average UR Across States",
       x = "Date",
       y = "Average UR") +
  theme_minimal()

# Combined INF

# Load required libraries
library(dplyr)
library(ggplot2)



# Group data by Date and calculate the average Combined INF across states
avg_combined_inf_df <- df %>%
  group_by(Date) %>%
  summarise(avg_combined_INF = mean(`Combined INF`, na.rm = TRUE)) %>%
  ungroup()

# Create the time series plot of the average Combined INF
ggplot(avg_combined_inf_df, aes(x = Date, y = avg_combined_INF)) +
  geom_line(color = "orange", size = 1.2) +
  labs(title = "Time Series Plot of Average Combined INF Across States",
       x = "Date",
       y = "Average Combined INF") +
  theme_minimal()

#------------------ Latex Table ------------------------------------------------

#2FE UR Table 

library(stargazer)
stargazer(model_ur, type = "text", align = "TRUE")
stargazer(model_ur)

stargazer(model_ur, model_combined_inf, type = "text")
stargazer(model_combined_Rinf, model_combined_Uinf,  type = "text")
stargazer(model_combined_Rinf, model_combined_Uinf)

stargazer(model_ur, model_combined_inf)


stargazer(model_ur, model_combined_inf, model_combined_ninf, type = "text")
stargazer(model_ur, model_combined_inf, model_combined_ninf)



#DiD Table  
stargazer(model_unem, type = "text")
stargazer(model_inf, type = "text")


stargazer(model_unem, model_inf)


stargazer(model_inf, model_unem, type = "text")



####### Nonlinear table #########

library(stargazer)

stargazer(model_SqrUR, model_SqurINF, type = "text")

stargazer(model_SqrUR, model_SqurINF)

################################################################################
#DID MODEL# Relevant R Code
################################################################################



#------------- FIRST LAG -------------------------------------------------------
library(dplyr)

Final_Monthly_Dataset <- Final_Monthly_Dataset %>%
  mutate(
    UR_lag = lag(UR, 1),
    Combined_INF_lag = lag(`Combined INF`, 1),
    eTransaction_lag = lag(`log e-Transactions`, 1)
  )

# View the first few rows to check the new lagged columns
head(Final_Monthly_Dataset)



#--------------- SECOND LAG ----------------------------------------------------

library(dplyr)

Final_Monthly_Dataset <- Final_Monthly_Dataset %>%
  mutate(
    UR_lag2 = lag(UR, 2),
    Combined_INF_lag2 = lag(`Combined INF`, 2),
    eTransaction_lag2 = lag(`log e-Transactions`, 2)
  )

# View the first few rows to verify the new lagged columns
head(Final_Monthly_Dataset)

############### DiD Model #######################################################

# DiD Model using existing "log-Transactions" column

# Load required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(lmtest)
library(sandwich)



# Assume the dataset is already imported and is named Final_Monthly_Dataset
df <- Final_Monthly_Dataset

colnames(df)

# Assuming your dataset is stored in 'df'
summary(df$`log e-Transactions`)

# Convert Date: since the dates are in "YYYY/MM" format, append "/01" and convert to Date
df$Date <- as.Date(paste0(df$Date, "/01"), format = "%Y/%m/%d")

# Create the Post dummy: 1 if the observation is on or after the policy date, 0 otherwise
policy_date <- as.Date("2016-11-08")
df <- df %>%
  mutate(Post = ifelse(Date >= policy_date, 1, 0))

# Note: We are no longer computing the log transformation because "log-Transactions" is already available in the dataset.

# Calculate each state's average "log-Transactions" in the pre-treatment period
df_pre <- df %>% filter(Date < policy_date)
state_avg <- df_pre %>%
  group_by(State) %>%
  summarize(avg_logTrans = mean(`log e-Transactions`, na.rm = TRUE))

# Merge the state-level average back into the main dataset
df <- left_join(df, state_avg, by = "State")


# Define the treatment: States with an average "log-Transactions" above the threshold are treated
threshold <- 5.383
df <- df %>%
  mutate(Treat = ifelse(avg_logTrans > threshold, 1, 0))

# Check summary statistics for the average log-Transactions and treatment assignment
summary(df$avg_logTrans)
table(df$Treat)

# DiD Regression for UR
# Including state fixed effects (factor(State)) and year fixed effects (extracted from Date)
# Plus several control variables and lag terms
model_unem <- lm(UR ~ Treat * Post + factor(State) + factor(year(Date)) +
                   + `LFPR - Rural Male` + UR_lag +
                   `LFPR - Urban Male` + `LFPR - Urban Female`   
                 +
                   `SDPpercap` + `Gross Fiscal Deficit`  +
                   + UR_lag2 + `Combined_INF_lag2`,
                 data = df)

summary(model_unem)

# DiD Regression for Combined INF
model_inf <- lm(`Combined INF` ~ Treat * Post +
                  `Rural INF` + `Urban INF`  +
                  `LFPR - Rural Female` + `LFPR - Urban Male`  +
                  `CD Ratio`  +
                  `SDPpercap`   +
                  `Combined_INF_lag`   +
                  `F&B Inflation` + `Urban F&B Inflation` + `Combined_INF_lag2` +
                  factor(State) + factor(year(Date)),
                data = df)

summary(model_inf)




# Clustered Standard Errors by State for Unemployment Model
cluster_se_unem <- vcovCL(model_unem, cluster = ~State)

# Get coefficient table with clustered SE
coeftest(model_unem, vcov = cluster_se_unem)

# Clustered Standard Errors by State for Inflation Model
cluster_se_inf <- vcovCL(model_inf, cluster = ~State)

# Get coefficient table with clustered SE
coeftest(model_inf, vcov = cluster_se_inf)

library(stargazer)

# Extract coefficients and clustered SEs
coefs_unem <- coef(model_unem)
se_unem <- sqrt(diag(cluster_se_unem))

coefs_inf <- coef(model_inf)
se_inf <- sqrt(diag(cluster_se_inf))

# Generate table
stargazer(model_unem, model_inf,
          se = list(se_unem, se_inf),
          title = "DiD Results with Clustered Standard Errors",
          dep.var.labels = c("Unemployment Rate", "Combined Inflation"),
          covariate.labels = c("Treat", "Post", "Treat x Post", "..."),
          notes = "Standard errors clustered at the state level.",
          type = "text")

stargazer(model_unem, model_inf,
          se = list(se_unem, se_inf),
          title = "DiD Results with Clustered Standard Errors",
          dep.var.labels = c("Unemployment Rate", "Combined Inflation"),
          covariate.labels = c("Treat", "Post", "Treat x Post", "..."),
          notes = "Standard errors clustered at the state level")


stargazer(model_unem, model_inf)




#----------- LaTeX Table -------------------------------------------------------

library(stargazer)

stargazer(model_unem, model_inf, type = "text")
stargazer(model_unem, model_inf)



###############################################################################
#ROBUSTNES CHECKS
###############################################################################

# Stable Treatment Status ------------------------------------------------------

# Check if any state has more than one treatment status across time
state_treat_status <- df %>%
  group_by(State) %>%
  summarize(unique_treat_status = n_distinct(Treat)) %>%
  filter(unique_treat_status > 1)

print(state_treat_status)

# Count number of treated and control states in each month
group_counts <- df %>%
  group_by(Date, Treat) %>%
  summarize(num_states = n_distinct(State), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Treat, values_from = num_states, names_prefix = "Treat_")

# Plot to visually confirm stability
ggplot(group_counts, aes(x = Date)) +
  geom_line(aes(y = Treat_0, color = "Control")) +
  geom_line(aes(y = Treat_1, color = "Treated")) +
  labs(title = "Number of Treated vs Control States Over Time",
       y = "Number of States", x = "Date") +
  theme_minimal() +
  scale_color_manual(values = c("Control" = "blue", "Treated" = "red"))


library(dplyr)
library(tidyr)

# Create a frequency table: number of treated and control states for each month
group_composition <- df %>%
  group_by(Date, Treat) %>%
  summarize(num_states = n_distinct(State), .groups = "drop") %>%
  mutate(Group = ifelse(Treat == 1, "Treated", "Control")) %>%
  select(-Treat) %>%
  pivot_wider(names_from = Group, values_from = num_states)

# Replace NA with 0 (in case any group had no states in a given month)
group_composition[is.na(group_composition)] <- 0

# Print the frequency table
print(group_composition)


#---------- Parallel trends assumption -----------------------------------------

library(ggplot2)
pre_trend <- df %>% filter(Date < policy_date) %>%
  group_by(Date, Treat) %>%
  summarize(avg_outcome = mean(`Combined INF`, na.rm = TRUE), .groups = "drop")

ggplot(pre_trend, aes(x = Date, y = avg_outcome, color = factor(Treat))) +
  geom_line() +
  labs(title = "Pre-Treatment Trends", x = "Date", y = "Average Outcome") +
  theme_minimal()

############# Random treatment assignment test (Copy in the regression model from the original DiD code)#################


# Set seed for reproducibility
set.seed(1234)

# Number of iterations for random assignment
n_iter <- 10000
rand_coef <- numeric(n_iter)

###################### Original DiD Model for UR or Combined INF ###############################

model_unem <- lm(UR ~ Treat * Post + factor(State) + factor(year(Date)) +
                   + `LFPR - Rural Male` + UR_lag +
                   `LFPR - Urban Male` + `LFPR - Urban Female`   
                 +
                   `SDPpercap` + `Gross Fiscal Deficit`  +
                   + UR_lag2 + `Combined_INF_lag2`,
                 data = df)
original_coef <- coef(model_unem)["Treat:Post"]
cat("Original Treatment Interaction Coefficient (Treat:Post):", original_coef, "\n")

# Run the random treatment assignment test
for(i in 1:n_iter) {
  # Create a random shuffle of the treatment indicator
  df$randTreat <- sample(df$Treat)
  
  # Re-estimate the DiD model with the randomized treatment indicator
  model_rand <- lm(UR ~ Treat * Post + factor(State) + factor(year(Date)) +
                     + `LFPR - Rural Male` + UR_lag +
                     `LFPR - Urban Male` + `LFPR - Urban Female`   
                   +
                     `SDPpercap` + `Gross Fiscal Deficit`  +
                     + UR_lag2 + `Combined_INF_lag2`,
                   data = df)
  
  # Extract the coefficient on the interaction term from the random model
  rand_coef[i] <- coef(model_rand)["randTreat:Post"]
}

# Summarize the distribution of the random coefficients
summary(rand_coef)
hist(rand_coef, main = "Distribution of Interaction Coefficients (randTreat:Post)",
     xlab = "Coefficient", col = "skyblue", border = "white")

# Calculate the permutation p-value:
# Proportion of random coefficients with an absolute value equal to or larger than the original effect.
p_value <- mean(abs(rand_coef) >= abs(original_coef))
cat("Permutation p-value (based on", n_iter, "iterations):", p_value, "\n")




######## Parallel Trends UR ########################################################

library(ggplot2)
library(dplyr)

# Group and calculate monthly average UR for treatment and control groups
plot_data <- df %>%
  group_by(Date, Treat) %>%
  summarise(mean_UR = mean(UR, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Group = ifelse(Treat == 1, "Treated", "Control"))

# Create the plot
ggplot(plot_data, aes(x = Date, y = mean_UR, color = Group, linetype = Group)) +
  geom_smooth(size = 1.2) +
  geom_vline(xintercept = as.Date("2016-11-08"), linetype = "dashed", color = "black", size = 0.5) +
  annotate("text", x = as.Date("2016-11-08") + 90, y = max(plot_data$mean_UR, na.rm = TRUE),
           label = "Policy Implementation", hjust = 0, size = 4.2) +
  scale_color_manual(values = c("Treated" = "blue", "Control" = "red")) +
  scale_linetype_manual(values = c("Treated" = "solid", "Control" = "solid")) +
  labs(title = "Unemployment Trends: Treatment vs Control",
       
       x = "Date",
       y = "Mean Unemployment Rate",
       color = "",
       linetype = "") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    legend.position = "bottom"
  )

######################################## Parallel Trends Combined INF ################

library(dplyr)
library(ggplot2)

# Prepare data for plotting Combined INF
plot_inf <- df %>%
  group_by(Date, Treat) %>%
  summarise(mean_INF = mean(`Combined INF`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Group = ifelse(Treat == 1, "Treated", "Control"))

# Plot
ggplot(plot_inf, aes(x = Date, y = mean_INF, color = Group, linetype = Group, fill = Group)) +
  geom_smooth(method = "loess", se = TRUE, size = 1.2, span = 0.5) +
  geom_vline(xintercept = as.Date("2016-11-08"), linetype = "dashed", color = "black", size = ) +
  annotate("text", x = as.Date("2016-11-08") + 150, y = max(plot_inf$mean_INF, na.rm = TRUE),
           label = "Policy Implementation", hjust = 0, size = 4.2) +
  scale_color_manual(values = c("Treated" = "#1f78b4", "Control" = "#e31a1c")) +
  scale_fill_manual(values = c("Treated" = "#1f78b4", "Control" = "#e31a1c")) +
  labs(title = "Combined Inflation Trends: Treatment vs Control",
       subtitle = "Before and After Policy Implementation (Nov 2016)",
       x = "Date",
       y = "Mean Combined Inflation Rate",
       color = "",
       fill = "",
       linetype = "") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    legend.position = "bottom"
  )

######### DiD Graph ############################################################


####UR############

library(dplyr)
library(ggplot2)
library(lubridate)

ggplot(did_plot_data, aes(x = Date, y = avg_UR, color = Group)) +
  geom_line(size = 1.3) +
  geom_vline(xintercept = as.Date("2016-11-08"), linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = as.Date("2016-07-01"), 
           y = max(did_plot_data$avg_UR, na.rm = TRUE) * 0.95,  # Position text slightly lower
           label = "Demonetisation", 
           size = 4, fontface = "bold") +
  labs(title = "Impact of Digitalisation on Unemployment: DiD Analysis",
       subtitle = "Average Unemployment Rate in High vs. Low Digital Adoption States",
       x = "Year",
       y = "Average Unemployment Rate (%)",
       color = "Digitalisation Group") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +   # Cleaner x-axis
  scale_color_manual(values = c("High Digital Adoption" = "#E64B35", 
                                "Low Digital Adoption" = "#4DBBD5")) +  # Professional colors
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0),
    panel.grid.minor = element_blank()
  )
ggsave("DiD_Unemployment_Professional.png", width = 9, height = 6, dpi = 300)


####### Combined INF ######################

# Assuming you have similar structure for Combined INF data
did_plot_inf <- df %>%
  mutate(Group = ifelse(Treat == 1, "High Digital Adoption", "Low Digital Adoption")) %>%
  group_by(Date, Group) %>%
  summarise(avg_INF = mean(`Combined INF`, na.rm = TRUE), .groups = "drop")

# Create the professional Combined INF DiD plot
ggplot(did_plot_inf, aes(x = Date, y = avg_INF, color = Group)) +
  geom_line(size = 1.3) +
  geom_vline(xintercept = as.Date("2016-11-08"), linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = as.Date("2016-07-01"), 
           y = max(did_plot_inf$avg_INF, na.rm = TRUE) * 0.95,
           label = "Demonetisation", 
           size = 4, fontface = "bold") +
  labs(title = "Impact of Digitalisation on Inflation: DiD Analysis",
       subtitle = "Average Combined Inflation in High vs. Low Digital Adoption States",
       x = "Year",
       y = "Average Combined Inflation (%)",
       color = "Digitalisation Group") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_color_manual(values = c("High Digital Adoption" = "#E64B35", 
                                "Low Digital Adoption" = "#4DBBD5")) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0),
    panel.grid.minor = element_blank()
  )

######## EVENT STUDY PLOT (CLUSTERED SE) #######################################

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(lmtest)
library(sandwich)
library(broom)

# Assume the dataset is already imported and is named Final_Monthly_Dataset
df <- Final_Monthly_Dataset

# Inspect summary of the existing log-Transactions column
summary(df$`log e-Transactions`)

# Convert Date: Since the dates are in "YYYY/MM" format, append "/01" and convert to Date object
df$Date <- as.Date(paste0(df$Date, "/01"), format = "%Y/%m/%d")

# Define the policy date (actual intervention date)
policy_date <- as.Date("2016-11-08")

# Create the Post dummy: 1 if the observation is on or after the policy date, 0 otherwise
df <- df %>% mutate(Post = ifelse(Date >= policy_date, 1, 0))

# (You are not re-computing the log transformation because "log-Transactions" is already provided)

# Calculate each state's average "log-Transactions" in the pre-treatment period
df_pre <- df %>% filter(Date < policy_date)
state_avg <- df_pre %>%
  group_by(State) %>%
  summarize(avg_logTrans = mean(`log e-Transactions`, na.rm = TRUE))

# Merge the state-level average back into the main dataset
df <- left_join(df, state_avg, by = "State")

# Define the treatment: States with an average "log-Transactions" above the threshold are treated
threshold <- 5.383
df <- df %>% mutate(Treat = ifelse(avg_logTrans > threshold, 1, 0))

# Check summary statistics for avg_logTrans and treatment assignment
summary(df$avg_logTrans)
table(df$Treat)


# 1. Create the Event Time Variable
# We'll compute event_time in months relative to the policy date.
# That is, event_time = 0 means the month of the policy, -1 is one month before, +1 one month after, etc.
df <- df %>%
  mutate(event_time = (year(Date) - year(policy_date)) * 12 + (month(Date) - month(policy_date)))

# 2. Convert event_time to a factor and choose a base period.
# Typically, you would choose a period immediately before the policy (e.g., event_time = -1) as the base.
df$event_time <- as.factor(df$event_time)
# Re-level so that event_time "-1" is the reference category
df$event_time <- relevel(df$event_time, ref = "-1")

# 1. Run the Event Study Regression (Change outcome variable accordingly)
event_model <- lm(`Combined INF` ~ factor(event_time) + factor(State) + factor(year(Date)) +
                    `Rural INF` + `Urban INF` + `LFPR - Rural Female` + `LFPR - Urban Male`  +
                    `CD Ratio`  + SDPpercap + Combined_INF_lag +
                    `F&B Inflation` + Combined_INF_lag2,
                  data = df)


event_model <- lm(`UR` ~ factor(event_time) + factor(State) + factor(year(Date)) 
                  + `LFPR - Rural Male` + UR_lag +
                    `LFPR - Urban Male` + `LFPR - Urban Female`   
                  +
                    `SDPpercap` + `Gross Fiscal Deficit`  +
                    + UR_lag2 + `Combined_INF_lag2`,
                  data = df)



# 2. Compute Clustered Standard Errors by State
cluster_se_event <- vcovCL(event_model, cluster = ~State)

# 3. Extract Coefficients with Clustered SEs
event_results <- coeftest(event_model, vcov = cluster_se_event)

# 4. Convert to Tidy Format and Filter for 6 Leads and 24 Lags
event_estimates <- tidy(event_results) %>%
  filter(grepl("^factor\\(event_time\\)", term)) %>%
  mutate(event_time = as.numeric(gsub("factor\\(event_time\\)", "", term))) %>%
  filter(event_time >= -12 & event_time <= 60)

# 5. Plot Event Study with Clustered SEs and Shaded COVID Period
ggplot(event_estimates, aes(x = event_time, y = estimate)) +
  annotate("rect", xmin = 40, xmax = 60, ymin = -Inf, ymax = Inf, 
           fill = "black", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_point(color = "black") +
  geom_line(group = 1, color = "#0072B2", linewidth = 0.6) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2) +
  labs(title = "Event Study Plot: Impact on Inflation",
       x = "Event Time",
       y = "Coefficient Estimate") +
  theme_minimal()


library(dplyr)
library(lubridate)
library(ggplot2)
library(lmtest)
library(sandwich)
library(broom)

# Assume the dataset is already imported and named Final_Monthly_Dataset
df <- Final_Monthly_Dataset

# Convert Date from "YYYY/MM" to Date object
df$Date <- as.Date(paste0(df$Date, "/01"), format = "%Y/%m/%d")

# Set policy date (Demonetisation)
policy_date <- as.Date("2016-11-08")

# Post-treatment dummy
df <- df %>% mutate(Post = ifelse(Date >= policy_date, 1, 0))

# Pre-treatment state-level average digitalisation
df_pre <- df %>% filter(Date < policy_date)
state_avg <- df_pre %>%
  group_by(State) %>%
  summarize(avg_logTrans = mean(`log e-Transactions`, na.rm = TRUE))

df <- left_join(df, state_avg, by = "State")
threshold <- 5.383
df <- df %>% mutate(Treat = ifelse(avg_logTrans > threshold, 1, 0))

# Create event time variable (months from policy date)
df <- df %>%
  mutate(event_time = (year(Date) - year(policy_date)) * 12 + (month(Date) - month(policy_date)))

# Set -1 as the reference period
df$event_time <- factor(df$event_time)
df$event_time <- relevel(df$event_time, ref = "-1")

# ============================
# Run Event Study for UR
# ============================

event_model_ur <- lm(UR ~ factor(event_time) + factor(State) + factor(year(Date)) +
                       `LFPR - Rural Male` + UR_lag + UR_lag2 +
                       `LFPR - Urban Male` + `LFPR - Urban Female` +
                       SDPpercap + `Gross Fiscal Deficit` +
                       `Combined_INF_lag2`,
                     data = df)

cluster_se_ur <- vcovCL(event_model_ur, cluster = ~State)
event_results_ur <- coeftest(event_model_ur, vcov = cluster_se_ur)

event_estimates_ur <- data.frame(
  term = rownames(event_results_ur),
  estimate = event_results_ur[, 1],
  std.error = event_results_ur[, 2]
) %>%
  filter(grepl("^factor\\(event_time\\)", term)) %>%
  mutate(event_time = as.numeric(gsub("factor\\(event_time\\)", "", term))) %>%
  filter(event_time >= -30 & event_time <= 60)

# ============================
# Run Event Study for Inflation
# ============================

event_model_inf <- lm(`Combined INF` ~ factor(event_time) + factor(State) + factor(year(Date)) +
                        `Rural INF` + `Urban INF` +
                        `LFPR - Rural Female` + `LFPR - Urban Male` +
                        `CD Ratio` + SDPpercap + Combined_INF_lag + Combined_INF_lag2 +
                        `F&B Inflation` + `Urban F&B Inflation`,
                      data = df)

cluster_se_inf <- vcovCL(event_model_inf, cluster = ~State)
event_results_inf <- coeftest(event_model_inf, vcov = cluster_se_inf)

event_estimates_inf <- data.frame(
  term = rownames(event_results_inf),
  estimate = event_results_inf[, 1],
  std.error = event_results_inf[, 2]
) %>%
  filter(grepl("^factor\\(event_time\\)", term)) %>%
  mutate(event_time = as.numeric(gsub("factor\\(event_time\\)", "", term))) %>%
  filter(event_time >= -12 & event_time <= 60)

# ============================
# Plotting Function
# ============================

plot_event_study <- function(estimates, title) {
  ggplot(estimates, aes(x = event_time, y = estimate)) +
    annotate("rect", xmin = 40, xmax = 60, ymin = -Inf, ymax = Inf,
             fill = "black", alpha = 0.1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_point(color = "black") +
    geom_line(group = 1, color = "#0072B2", linewidth = 0.6) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error),
                  width = 0.2) +
    labs(title = title,
         x = "Event Time",
         y = "Coefficient Estimate") +
    theme_minimal()
}

# ============================
# Generate Plots
# ============================

plot_ur <- plot_event_study(event_estimates_ur, "Event Study Plot: Impact on Unemployment")
plot_inf <- plot_event_study(event_estimates_inf, "Event Study Plot: Impact on Inflation")

# Show plots
print(plot_ur)
print(plot_inf)






















