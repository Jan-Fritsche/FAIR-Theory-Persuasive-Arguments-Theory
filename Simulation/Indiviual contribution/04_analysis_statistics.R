# ------------------------------------------------------------------------------
# Statistical Analysis (Hypothesis Testing and Effectsize Calculation)
# ------------------------------------------------------------------------------

source("01_functions.R")
load("simulation_data.RData")
library(effectsize)
library(dplyr)

# Test 1: Original Study Baseline (C=0)
print("--- Result 1: Original Study ---")
print(t.test(simulation_results$T_post, simulation_results$T_pre, paired = TRUE))
print(cohens_d(simulation_results$T_post, simulation_results$T_pre, paired = TRUE))

# Test 2: Higher Sample Size (Robustness)
print("--- Result 2: Higher N (Robustness) ---")
print(t.test(simulation_higherN$T_post, simulation_higherN$T_pre, paired = TRUE))
print(cohens_d(simulation_higherN$T_post, simulation_higherN$T_pre, paired = TRUE))

# Test 3: Model with Uncertainty Factor (C=4)
print("--- Result 3: With Uncertainty (C=4) ---")
print(t.test(simulation_C$T_post, simulation_C$T_pre, paired = TRUE))
print(cohens_d(simulation_C$T_post, simulation_C$T_pre, paired = TRUE))

# Test 4: Selective Uptake - beta = 0
print("--- Result 4: Selective Uptake (beta = 0) ---")
print(t.test(simulation_beta0$T_post, simulation_beta0$T_pre, paired = TRUE))
print(cohens_d(simulation_beta0$T_post, simulation_beta0$T_pre, paired = TRUE))

# Test 5: Selective Uptake - beta = 1
print("--- Result 5: Selective Uptake (beta = 1) ---")
print(t.test(simulation_beta1$T_post, simulation_beta1$T_pre, paired = TRUE))
print(cohens_d(simulation_beta1$T_post, simulation_beta1$T_pre, paired = TRUE))

# Test 6: Selective Uptake - beta = 2
print("--- Result 6: Selective Uptake (beta = 2) ---")
print(t.test(simulation_beta2$T_post, simulation_beta2$T_pre, paired = TRUE))
print(cohens_d(simulation_beta2$T_post, simulation_beta2$T_pre, paired = TRUE))

# Group-level comparison across selective uptake conditions
group_shift_data <- bind_rows(
  simulation_beta0 %>% mutate(Condition = "beta0"),
  simulation_beta1 %>% mutate(Condition = "beta1"),
  simulation_beta2 %>% mutate(Condition = "beta2")
) %>%
  group_by(Condition, GroupID) %>%
  summarise(
    GroupShift = mean(T_post) - mean(T_pre),
    .groups = "drop"
  )

print("--- ANOVA: Group Polarization by Selective Uptake Condition ---")
print(summary(aov(GroupShift ~ Condition, data = group_shift_data)))

print("--- Descriptive Statistics by Condition ---")
print(
  group_shift_data %>%
    group_by(Condition) %>%
    summarise(
      Mean = mean(GroupShift),
      SD = sd(GroupShift),
      N = n()
    )
)

print("--- Tukey Post-hoc Comparisons ---")
print(TukeyHSD(aov(GroupShift ~ Condition, data = group_shift_data)))