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
 
  
  for(i in 1:days){ ##This cannot go on until days, this needs to stop at the last possible window. ##days - window_size +1 maybe
    
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
    current_date <- vector(mode = 'list', length = window_size)
    past_date <- vector(mode = 'list', length = window_size)
    date_list <- key_stock_df$Date
      for(k in 0:(window_size-1)){
        RV_m[[(k+1)]] <- (1/month) * sum(( key_stock_df$RV[(beg+k):(i+k)]))
        RV_w[[(k+1)]]<- (1/week) * sum(( key_stock_df$RV[(beg+k):(beg+k - (week-1))]))
        RV_t[[(k+1)]] <- key_stock_df$RV[(beg+k-1)]
        current_date[[(k+1)]] <- key_stock_df$Date[(beg+k)]
        past_date[[(k+1)]] <- key_stock_df$Date[(beg+k-1)]
      }
    ##Now that we've built all of our variables as lists, we wish to assemble them into a dataframe,
    ##We do so by using I(list) to consider it as a column, and then unlist so that the datatype is not a list itself
    independent_variables <-  
      data.frame('month_RV' = unlist(I(RV_m)), 'week_RV' = unlist(I(RV_w)), 'previous_RV' = unlist(I(RV_t)), 'current_date' = unlist(I(current_date)), 'past_date' = unlist(I(past_date)))
    
    
    independent_variables$current_date <- as.Date(independent_variables$current_date, origin = '1970-01-01') ##R likes to convert dates to int (nb of days since 1970-01-01), this converts it back
    
    
    independent_variables$past_date <- as.Date(independent_variables$past_date, origin = '1970-01-01')                
    independent_variables <- inner_join(independent_variables, wide_other_stock_df, by = c('past_date' = 'Date' )) ## Now, we want to join this with the RV_{t-1} of all other stocks
    ##Finally, I want to join this with the dependent variable which we separated in key_window.
    full_data <- key_window %>% select(Date, VOL, chosen_variable) %>% inner_join(independent_variables, by = c('Date' = 'current_date'))
    y_var <- data.matrix(scale(full_data$RV)) ##GLMNET only works with data matrix, so we need to convert it and separate the variables. Ridge Regression also imposes that the response is centered.
    x_var <- data.matrix(scale(full_data[,!names(full_data) %in% c('Date', 'VOL', 'RV', 'past_date')])) ##Here, I HAD to remove V, because it's NAN before 2008.
    
    ##If I remove it as planned from my dataset, it won't be a problem anymore.
    # Setting the range of lambda values
    
    # Using glmnet function to build the ridge regression in r
    fit <- tidy(glmnet(x_var, y_var, alpha = 0, lambda  = lambda_seq)) ##This creates a data frame for the current model!
    lambda_sequence <- 10^seq(4, -4, by = -.01) ##Create a lambda sequence to test
    ##Now, we can use HD Econometrics to do the optimizations for ridge
    ridge <- ic.glmnet(x_var, y_var, crit = 'bic', alpha = 0, lambda = lambda_sequence) ##This estimates the best lambda via BIC and spits out the model.
    
    
    
    # Plot information criteria against tried values of lambdas
    ggplot(log(lambda_sequence), aic, col = "orange", type = "l",
         ylim = c(190, 260), ylab = "Information Criterion") + 
    geom_line(aes(log(lambda_sequence)), bic, col = "skyblue3")
    legend("bottomright", lwd = 1, col = c("orange", "skyblue3"), legend = c("AIC", "BIC"))
    # Checking the model
    break
 return(i) }
  
  
  
  
  
  
  
  
  
  
  
  
  return()
}
