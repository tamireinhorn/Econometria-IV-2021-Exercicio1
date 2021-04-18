###This should be the R script where we save our functions, because this makes it all much tidier.

RollingWindow <- function(key_stock = 'SPY', window_size = 1000, week = 5, month = 22, df, model_type = 'level'){
  ##The key_stock argument defines the stock over which we want to model. It defaults to SPY since it is the ETF.
  
  ##The window_size is the size of our rolling window, which is defaulted to be 1000 as defined by the assignment.
  
  ##The week and month arguments are just for specifying the RV_m, RV_w part of the calculation, should we want to change them as well.
  
  ##model_type will give us if the model is to be a log or level model and therefore which variable to choose later:
  chosen_variable <- ifelse(model_type == 'level', 'RV', 'log_RV')
  
  other_stock_df <- df %>% filter(Stock != key_stock) ##This will create a dataframe with all other stocks other than our dependent variable!
  
 
  
  wide_other_stock_df <- other_stock_df %>% select(Stock, Date, RV) %>% dcast(Date ~ Stock, value.var = chosen_variable)  ##This other_stock df needs to be wide!
  
  
  key_stock_df <- df %>% filter(Stock == key_stock) ##This, in turn, creates a df with ONLY the stock we want.
  
  days <- dim(key_stock_df)[[1]] ##This will give us the length of our dataset
  day_list <- sort(unique(key_stock_df$Date))
  #Now for the rolling window loop in itself:
  #Since month is the biggest order of our model, I need to have 22 observations (or however many I set)
  ##Therefore, I need to start in observation 22, so that I have the past 21 observations! 
  ##So I always start in 
  ##Say my window is of size 3. Then I would get observations 22, 23, 24.
  ##The end of my interval will be at window_Size -1  + i + month -1 => 2 + 22 = 24
 
  
  for(i in 1:days){
    beg <- i + (month - 1) ##Just to clear notation, I wanted to create this variable for the interval's beginning.
    key_window <- key_stock_df[(beg):(window_size - 1 + beg),]  ##This creates the data subset we wish to use ##TODO: should this use the COMPLETE df or the separate?
    other_window <- wide_other_stock_df [(beg):(window_size - 1 +beg ),]
    
    ##However, this does not seal the deal. Once inside this window, we need to construct the variables. This requires us to loop again?
    ##If our window_size is indeed 3, and we have observations 22, 23, 24, we need to build RV_m for all these
    ##RV_{m, 22} is 1/22 * sum(RV of 22 to 1), RV_{m,23} is 1/22 * sum(RV of 23 to 2), etc. 
    ##So RV_{m,i} needs to go from i+month-1 to i, then from i+month-1+1 to i+1, etc. 
    ##For week, we start at i+beg (22) and we need to go until 18, which means (i+beg) - (week - 1) = 
    RV_m <- vector(mode = 'list', length = window_size)
    RV_w <- vector(mode = 'list', length = window_size)
    RV_t <- vector(mode = 'list', length = window_size)
      for(k in 0:(window_size-1)){
        RV_m[[(k+1)]] <- (1/month) * sum(( key_stock_df$RV[(beg+k):(i+k)]))
        RV_w[[(k+1)]]<- (1/week) * sum(( key_stock_df$RV[(beg+k):(beg+k - (week-1))]))
        RV_t[[(k+1)]] <- key_stock_df$RV[(beg+k-1)]
      }
  }
  
  
  
  
  
  
  
  
  
  
  
  
  return()
}
