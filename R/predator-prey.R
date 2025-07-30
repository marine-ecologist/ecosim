library(tidyverse)
library(deSolve)
library(ggplot2)
library(gganimate)
library(tidyr)
library(patchwork)

# Lotka-Volterra model with stochastic prey growth
lotka_volterra <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dPrey <- r * Prey - a * Prey * Predator + rnorm(1, mean = noise, sd = sqrt(var))
    dPredator <- b * Prey * Predator - m * Predator
    list(c(dPrey, dPredator))
  })
}

set.seed(101)

# Simulation function
simulate_lv <- function(state, parameters, times, label) {
  out <- as.data.frame(ode(y = state, times = times, func = lotka_volterra, parms = parameters))
  out$Scenario <- label
  out$Prey[out$Prey < 1] <- 0
  return(out)
}

# Time vector
times <- seq(0, 200, by = 0.5)

# Initial state
state <- c(Prey = 40, Predator = 9)

# Scenario A: default
params_A <- c(r = 0.6, a = 0.025, b = 0.01, m = 0.4, noise=0, var=0)

# Scenario B: more volatile predator (higher mortality, lower efficiency)
params_B <- c(r = 0.6, a = 0.025, b = 0.01, m = 0.35, noise=10, var=1)

# Simulate both
sim_A <- simulate_lv(c(Prey = 40, Predator = 9), params_A, times, "Stable oscillations") %>%
  mutate(row_id = row_number(),
         Predator_zero = Predator <= 1e-6,
         cutoff = ifelse(any(Predator_zero), min(row_id[Predator_zero]), Inf)) %>%
  mutate(
    Prey = if_else(row_id > cutoff, 0, Prey),
    Predator = if_else(row_id > cutoff, 0, Predator)
  ) %>%
  select(-row_id, -Predator_zero, -cutoff) %>%
  filter(time < 100)


sim_B <- simulate_lv(c(Prey = 40, Predator = 20), params_B, times, "Volatile prey dynamics") %>%
  mutate(row_id = row_number(),
         Predator_zero = Predator <= 1e-6,
         cutoff = ifelse(any(Predator_zero), min(row_id[Predator_zero]), Inf)) %>%
  mutate(
    Prey = if_else(row_id > cutoff, 0, Prey),
    Predator = if_else(row_id > cutoff, 0, Predator)
  ) %>%
  select(-row_id, -Predator_zero, -cutoff) %>%
  filter(time < 100)


# Combine and reshape
df_all <- bind_rows(sim_A, sim_B)
df_long <- pivot_longer(df_all, cols = c("Prey", "Predator"), names_to = "Species", values_to = "Population")

# Create animation plot
p <-ggplot(df_long, aes(x = time, y = Population, color = Species)) +
  geom_line(size = 0.7) +
  facet_wrap(~Scenario, ncol = 2, scales = "free_x") +
  scale_color_manual(values = c("Prey" = "darkgreen", "Predator" = "firebrick")) +
  labs(title = "Predator-Prey Dynamics Under Different Parameters",
       x = "Time", y = "Population Size") +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank())



# Save animation
anim_save("lotka_two_dynamics.gif", animation = p + transition_reveal(time),
          width = 1200, height = 500, res = 120, renderer = gifski_renderer())
