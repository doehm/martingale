
# martingale function
martingale <- function(bet, cash, p, stop_condn = Inf, stop_factor = Inf, reverse = FALSE, plot = FALSE){
  
  bet_vec <- vector(mode = "numeric")
  cash_vec <- vector(mode = "numeric")
  outcome <- vector(mode = "numeric")
  winnings <- vector(mode = "numeric")
  total_cash <- vector(mode = "numeric")
  trial <- 0
  total_cash[1] <- cash
  
  # while(bet_vec[trial]/cash_vec[trial] < stop_condn & total_cash[trial] > 0){
  while(total_cash[max(trial, 1)] > 0){
    
    # iterate through trials
    trial <- trial + 1
    
    # update cash pool
    if(trial == 1){
      cash_vec[trial] <- cash
    }else{
      cash_vec[trial] <- total_cash[trial-1]
    }
    
    # set bet
    if(!reverse){
      if(outcome[trial - 1] == 1 || trial == 1){
        bet_vec[trial] <- bet
      }else{
        bet_vec[trial] <- min(2*bet_vec[trial-1], cash_vec[trial]) # if there isn't enough to double the bet just bet what is left
      }      
    }else{
      if(outcome[trial - 1] == 0 || trial == 1){
        bet_vec[trial] <- bet
      }else{
        bet_vec[trial] <- min(2*bet_vec[trial-1], cash_vec[trial]) # if there isn't enough to double the bet just bet what is left
      }
    }
    
    # stop condition
    if(bet_vec[trial]/cash_vec[trial] > stop_condn){
      outcome[trial] <- NA
      winnings[trial] <- NA
      total_cash[trial] <- cash_vec[trial]
      break
    }
    
    # win or lose
    outcome[trial] <- sample(c(0,1), 1, prob = c(1-p, p))
    
    # winings - profit / loss
    winnings[trial] <- bet_vec[trial]*outcome[trial] - bet_vec[trial]*(1-outcome[trial])
    
    # end of round cash total
    total_cash[trial] <- cash_vec[trial] + winnings[trial]
    
    # stop conditions
    if(total_cash[trial] >= stop_factor*cash) break
  }
  
  # make the plot
  g1 <- NULL
  if(plot){
    df <- data.frame(trials = 1:trial, cash = total_cash)
    g1 <- ggplot() +
      geom_line(data = df, mapping = aes(x = trials, y = cash), col = "darkmagenta", lty = 1, size = 1) +
      # geom_line(data = rbind(tail(df, 1), data.frame(trials = trial+1, cash = 0)), mapping = aes(x = trials, y = cash), col = "darkmagenta", lty = 2, size = 1) +
      geom_hline(yintercept = cash_vec[1], col = "grey", lty = 2) +
      theme_minimal() +
      my_theme(1) +
      labs(
        x = "Number of spins",
        y = "Total cash in hand",
        title = ifelse(reverse, "Reverse Martingale strategy", "Martingale strategy"),
        subtitle = "The growth and decline of the gamblers cash pool - it always ends the same way"
      ) +
      ylim(0, NA)
    print(g1)
  }
  
  return(list(
    bet = bet_vec, 
    cash = cash_vec, 
    outcome = outcome, 
    total_cash = total_cash, 
    trials = trial, 
    plot = g1))
}
