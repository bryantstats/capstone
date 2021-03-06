
library(ggplot2)
library(purrr)

# one time playing

# Plot money vs times of playing
bett1 = function(original_amount=1000, times=50, bet=10, payout=35, odd=37, m =2)
{
  amount_series = c()
  amount_series[1] = original_amount
  new_bet = bet
  for (i in 1:times)
  {
    win = rbernoulli(1, 1/(odd+1))
    
    if (win){
      amount_series[i+1] = amount_series[i] + (payout)*new_bet
      new_bet = bet
    } 
    else {
      
      amount_series[i+1] = amount_series[i] - new_bet
      new_bet = m*new_bet
    }
    
  }
  
  return(amount_series) 
}

bett1n = function(original_amount=1000, times=50, bet=10, payout=35, odd=37, m =2)
{
  amount_series = c()
  amount_series[1] = original_amount
  new_bet = bet
  win = rbernoulli(times, 1/(odd+1))
  for (i in 1:times)
  {
    if (win[i]){
      amount_series[i+1] = amount_series[i] + (payout)*new_bet
      new_bet = bet
    } 
    else {
      amount_series[i+1] = amount_series[i] - new_bet
      new_bet = m*new_bet
    }
    
  }
  
  return(amount_series) 
}

# When running out of money?

bett2 = function(original_amount=1000, times=50, bet=10, payout=35, odd=37, m =2)
{
  amount_series = c()
  amount_series[1] = original_amount
  new_bet = bet
  for (i in 1:times)
  {
    win = rbernoulli(1, 1/(odd+1))
    
    if (win){
      amount_series[i+1] = amount_series[i] + (payout)*new_bet
      new_bet = bet
    } 
    else {
      
      amount_series[i+1] = amount_series[i] - new_bet
      new_bet = m*new_bet
    }
    
    if (amount_series[i+1]<0) break
      
  }
  
  return(amount_series) 
}

# Long term Outcome

# Expected Amount

# Plot money vs times of playing
bett_lt = function(no.simu =  1000, original_amount=1000, times=50, bet=10, payout=35, odd=37, m =2)
{
  amount_series_lt = matrix(0, ncol = no.simu, nrow = (times +1))
  
  for (i in 1:no.simu)
  {
    amount_series_lt[,i] = bett1n(original_amount=original_amount, times=times, bet=bet, payout=payout, odd=odd, m =m)
  }
  
  amount_series_lt_expected = apply(amount_series_lt,1, mean)
  return(amount_series_lt_expected)
}


# Running some simulations
for (i in 21: 40){
  print(i)
  x = bett_lt(no.simu =  1000000, original_amount=100, times=100, bet=10, payout=1, odd=10/9, m =1)
  ggsave(paste0(i,'.png'), qplot(c(1:length(x)),x, xlab = 'Number of Plays'))
}

x = bett_lt(no.simu =  1000000, original_amount=1000, times=1000, bet=10, payout=35, odd=37, m=2)
qplot(c(1:length(x)),x, xlab = 'Number of Plays')


