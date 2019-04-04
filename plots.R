# martingale

# libraries
library(tidyverse)
library(showtext)
library(magick)
library(png)
library(gridExtra)
library(parallel)
library(ggridges)

# theme setup
font_add_google("Montserrat", "montserrat")
showtext_auto()

my_theme <- function(scale, drop.y = FALSE){
  out <- theme_minimal() +
    theme(
      text = element_text(family = "montserrat", size = 32*scale),
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(size = 18*scale),
      axis.title = element_text(size = 24*scale),
      plot.caption = element_text(size = 16*scale),
      plot.subtitle = element_text(size = 20*scale, hjust = 0.5, margin = margin(t = 10))
    )
  
  if(drop.y){
    out <- theme_minimal() +
      theme(
        text = element_text(family = "montserrat", size = 32*scale),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 18*scale),
        axis.title = element_text(size = 24*scale),
        plot.caption = element_text(size = 16*scale),
        plot.subtitle = element_text(size = 20*scale, hjust = 0.5, margin = margin(t = 10)),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }
  return(out)
}

martingale(1, 100, 18/38, reverse = FALSE, plot = TRUE)
martingale(1, 100, 18/38, reverse = TRUE, plot = TRUE, stop_condn = 1)


# $1 bet; $100 cash
sim <- sapply(1:20000, function(x) martingale(bet = 1, cash = 100, p = 18/38, reverse = FALSE)$trials)
scale <- 1
data.frame(trials = sim) %>% 
  dplyr::filter(trials < quantile(sim, 0.975)) %>% 
  ggplot(aes(x = trials)) +
  geom_density(fill = "turquoise", col = "turquoise", alpha = 0.7) +
  labs(
    x = "Number of spins",
    y = "Density",
    title = "Martingale strategy",
    subtitle = "Distribution of number of trials from 20,000 simulations until bankruptcy ($1 bet; $100 cash)",
    caption = "@danoehm | gradientdescending.com"
  ) +
  my_theme(scale, drop.y = TRUE)
quantile(sim, c(0.025, 0.1, 0.5, 0.9, 0.975))

# reverse martingale
sim_reverse <- sapply(1:20000, function(x) martingale(bet = 1, cash = 100, p = 18/38, reverse = TRUE)$trials)
scale <- 1
data.frame(trials = sim_reverse) %>% 
  dplyr::filter(trials < quantile(sim_reverse, 0.999)) %>% 
  ggplot(aes(x = trials)) +
  geom_density(fill = "turquoise", col = "turquoise", alpha = 0.7) +
  labs(
    x = "Number of spins",
    y = "Density",
    title = "Reverse Martingale strategy",
    subtitle = "Distribution of number of trials from 20,000 simulations until bankruptcy ($1 bet; $100 cash)",
    caption = "@danoehm | gradientdescending.com"
  ) +
  my_theme(scale, drop.y = TRUE)
quantile(sim_reverse, c(0.025, 0.1, 0.5, 0.9, 0.975))

# dual plot
sim_max_cash <- sapply(1:20000, function(x) martingale(bet = 1, cash = 100, p = 18/38, reverse = FALSE)$cash %>% max)
sim_max_cash_reverse <- sapply(1:20000, function(x) martingale(bet = 1, cash = 100, p = 18/38, reverse = TRUE)$cash %>% max)
save(sim_max_cash, sim_max_cash_reverse, file = "./martingale/sim-max-cash.Rdata")
scale <- 1
data.frame(
  final_cash_amount = c(sim_max_cash, sim_max_cash_reverse), 
  strategy = c(rep("classic", 20000), rep("reverse", 20000))
) %>% 
  dplyr::filter(final_cash_amount < quantile(final_cash_amount, 0.975)) %>%
  ggplot(aes(x = final_cash_amount, fill = strategy, col = strategy)) +
  geom_density(alpha = 0.7) +
  labs(
    x = "Number of spins",
    y = "Density",
    title = "Classic vs. Reverse Martingale strategy",
    subtitle = "Distribution of maximum cash held at anytime from 20,000 simulations ($1 bet; $100 cash)"
    # caption = "Tail of Classic strategy heavily trimmed"
  ) +
  my_theme(scale, drop.y = TRUE) +
  scale_fill_manual(values = c("turquoise", "darkmagenta")) +
  scale_color_manual(values = c("turquoise", "darkmagenta"))





# vanilla
clust <- makeCluster(10)
clusterExport(clust, varlist = c("martingale", "%>%"))
stop_seq <- seq(0.1, 1, 0.1)
final_cash_amount_list_v <- lapply(
  stop_seq, 
  function(j) parSapply(clust, 1:20000, function(x) martingale(bet = 1, cash = 100, p = 18/38, stop_condn = j, reverse = FALSE)$total_cash %>% tail(1))
)

# reverse
final_cash_amount_list_r <- lapply(
  stop_seq, 
  function(j) parSapply(clust, 1:20000, function(x) martingale(bet = 1, cash = 100, p = 18/38, stop_condn = j, reverse = TRUE)$total_cash %>% tail(1))
)

# disconnect
stopCluster(clust)

# fix names
names(final_cash_amount_list_v) <- paste0("s", stop_seq)
names(final_cash_amount_list_r) <- paste0("s", stop_seq)

# plot ridges 
ridges_plot <- rbind(
  as_data_frame(final_cash_amount_list_v) %>% 
    gather(key = "stopping", value = "final_cash") %>% 
    mutate(
      stopping = factor(stopping),
      Strategy = "Classic"
    ),
  as_data_frame(final_cash_amount_list_r) %>% 
    gather(key = "stopping", value = "final_cash") %>% 
    mutate(
      stopping = factor(stopping),
      Strategy = "Reverse"
    )
) %>% 
  group_by(stopping) %>% 
  dplyr::filter(final_cash < quantile(final_cash, 0.9)) %>% 
  ggplot(aes(x = final_cash, y = stopping, fill = ..x..)) +
  geom_density_ridges_gradient() +
  scale_fill_gradientn(colours = colorRampPalette(c("darkmagenta", "turquoise"))(32)) +
  my_theme(1) +
  labs(
    title = "Comparison of strategies",
    subtitle = "Distribution of the final cash amount with 10 different stopping conditions - higher the number, the higher the accepted risk",
    x = "Final cash pool ($)",
    y = "Stopping condition",
    caption = "@danoehm | gradientdescending.com"
  ) +
  theme(legend.position = "none") +
  facet_grid(. ~ Strategy)

ridges_plot


sim <- parSapply(clust, 1:20000, function(x) martingale(bet = 1, cash = 100, p = 18/38)$trials)
save(sim, file = "./martingale/sim_trials.Rdata")

sim_reverse <- parSapply(clust, 1:20000, function(x) martingale(bet = 1, cash = 100, p = 18/38, reverse = TRUE)$trials)
save(sim_reverse, file = "./martingale/sim_trials_reverse.Rdata")

# double money
N <- 20000
clust <- makeCluster(10)
clusterExport(clust, varlist = c("martingale", "%>%"))
sim_double_money <- parSapply(clust, 1:N, function(x) martingale(bet = 1, cash = 100, p = 18/38, reverse = FALSE, stop_factor = 2)$total_cash %>% tail(1))
sim_double_money_reverse <- parSapply(clust, 1:N, function(x) martingale(bet = 1, cash = 100, p = 18/38, reverse = TRUE, stop_factor = 2)$total_cash %>% tail(1))
stopCluster(clust)

sum(sim_double_money >= 200)/length(sim_double_money)
sum(sim_double_money_reverse >= 200)/length(sim_double_money_reverse)
mean(sim_double_money)
mean(sim_double_money_reverse)

save(sim_double_money, sim_double_money_reverse, file = "./martingale/sim_double_money.Rdata")

g_double_money <- data.frame(
  final_cash_amount = c(sim_double_money, sim_double_money_reverse), 
  strategy = c(rep("vanilla", N), rep("reverse", N))
) %>% 
  ggplot(aes(x = final_cash_amount, fill = strategy)) +
  geom_histogram(position = "identity", alpha = 0.7) +
  labs(
    x = "Final cash amount ($)",
    title = "Distribution of final cash amounts",
    subtitle = "Comparison of Vanilla vs Reverse Martingale strategies stopping when cash is doubled or bust ($1 bet; $100 cash)",
    caption = "@danoehm | gradientdescending.com"
  ) +
  my_theme(1, drop.y = TRUE) +
  scale_fill_manual(values = c("turquoise", "darkmagenta"))
g_double_money
png("./martingale/double-money.png", width = 1920, height = 1080)
g_double_money
dev.off()
