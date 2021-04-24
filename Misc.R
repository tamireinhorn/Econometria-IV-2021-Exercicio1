###This should be the R script where we save our functions, because this makes it all much tidier.

VariableCreation <- function(key_stock = 'SPY', week = 5, month = 22, df){
  ##The key_stock argument defines the stock over which we want to model. It defaults to SPY since it is the ETF.
  
  ##The window_size is the size of our rolling window, which is defaulted to be 1000 as defined by the assignment.
  
  ##The week and month arguments are just for specifying the RV_m, RV_w part of the calculation, should we want to change them as well.
  

  
  
  other_stock_df <- df %>% filter(Stock != key_stock) ##This will create a dataframe with all other stocks other than our dependent variable!
  
  
  
  wide_other_stock_df <- other_stock_df %>% select(Stock, Date, RV) %>% dcast(Date ~ Stock, value.var = 'RV')  ##This other_stock df needs to be wide!
  
  
  key_stock_df <- df %>% filter(Stock == key_stock) ##This, in turn, creates a df with ONLY the stock we want.
  
  days <- dim(key_stock_df)[[1]] ##This will give us the length of our dataset
  day_list <- sort(unique(key_stock_df$Date))
  length_rw <- days - (month - 1) - 1 
  #would contain regressors, but there would be no future date for these regressors to forecast (that is, there would be no dependent variable), so we have to stop in the observation right before the last one (as it will allow for regressors and a dependent variable, which will be the last observation)
  
  #We construct regressors below. The dependent variable will simply be the RV column in key_stock_df, and it will later be combined correctly with the regressors through an inner_join, taking into account the fact that regressors are one period before the dependent variable they forecast 
  
  RV_m <- vector(mode = 'list', length = length_rw)
  RV_w <- vector(mode = 'list', length = length_rw)
  RV_t <- vector(mode = 'list', length = length_rw)
  current_date <- vector(mode = 'list', length = length_rw)
  future_date <- vector(mode = 'list', length = length_rw) #This will be used for an upcomming inner_join that will relate independent and dependent variables, such that into a row of regressors we should add a dependent variable, with the date of the dependent variable (which is one period ahead of the regressors) equals, thus, the future date of the regressors
  
  for(i in 1:length_rw){
    
    RV_m[[i]] <- (1/month) * sum(key_stock_df$RV[i:(i+month-1)]) #for i=1 (index of the first OBS in the original SPY data for which RV_t RV_m and RV_w are constructed), we sum over observations 1 to 22 of ORIGINAL spy data, yielding the first observation for our regressor
    RV_w[[i]] <- (1/week) * sum(key_stock_df$RV[(i+17):(i+month-1)]) #for i=1, we sum over observations 18 to 22
    RV_t[[i]] <- key_stock_df$RV[(i+month-1)] #for i=1, we select observation 22
    current_date[[i]] <- key_stock_df$Date[(i+month-1)]
    future_date[[i]] <- key_stock_df$Date[(i+month-1+1)] #if the code works, the last observation of future_date will be the last date for which SPY has an observation. Testei e ta ok
  }
  #Below we form our dataframe of independent variables, using regressors as regular numeric and date coluns
  
  independent_variables <-  
    data.frame('month_RV' = unlist(I(RV_m)), 'week_RV' = unlist(I(RV_w)), 'previous_RV' = unlist(I(RV_t)), 'current_date' = unlist(I(current_date)), 'future_date' = unlist(I(future_date)))  
  
  independent_variables$current_date <- as.Date(independent_variables$current_date, origin = '1970-01-01') #R likes to convert dates to int (nb of days since 1970-01-01), this converts it back
  independent_variables$future_date <- as.Date(independent_variables$future_date, origin = '1970-01-01')
  
  #Now, we combine our lagged endogenous variables (RV for "SPY" with Stocks from other companies, captured in wide_other_stock_df, and use the relation between dates)
  
  independent_variables <- inner_join(independent_variables, wide_other_stock_df, by = c('current_date' = 'Date' )) # Now, we want to join this with the RV_{t-1} of all other stocks
  
  #Now, we combine the dependent variable with the independent variables above. For a certain date, each dependent variable is paired with the independent variable of the previous date, as it will forecast the dependent
  
  full_data <- key_stock_df %>% select(Date, RV) %>% inner_join(independent_variables, by = c('Date' = 'future_date')) 
return(full_data)}

RidgeRegressionBIC <- function(x,y){
  ridge <- glmnet(x, y, alpha = 0) ##This makes a ridge model with the function variables. 
  
  #initializing the BIC vector
  BIC = rep(NA, length(ridge$lambda))
  ##The BIC criteria needs to calculate df(lambda), which is given by trace (or sum of the elements of diag)
  ##The matrix is given by X * Inverse((X'X + lambda * Identity)) * X'
  x_squared <- t(x) %*% x #transpose of x multiplied (in the matrix sense) by x
  
  identity_matrix <- diag(ncol(x)) ##This creates the identity matrix I.
  aux <-(log(window_size)/window_size)
  
  #calculating information criteria
  for (j in 1:length(ridge$lambda)) { 
    
    #degrees of freedom
    lambda_diag = ridge$lambda[j] * identity_matrix ##This is Lambda * Identity
    
    df = sum(diag(x %*% solve(x_squared + lambda_diag) %*% t(x))) + 1 #+1 due to the the intercept! 
    ##This completes the degrees of freedom calculation!
    #In slide 6 of lecture 2, part 3, we have that df = tr(x(x'*x+lambda*i)^(-1)*x')
    #the term (x'*x+lambda*i)^(-1) is obtained by inverting (x'*x+lambda*i), hence we use solve()
   
    
    #sigma^2(lambda)
    
    #for yhat, we take into account also the intercept, and for this, we add to X a column of constant 1, which when multiplied by coef(ridge[intercept]), will yield the correct value
    yhat = cbind(1, x_var[i:(window_size+i-1),]) %*% coef(ridge)[,j]
    
    #resid is a vector composed of 1000 elements, each one the residual for each observation in the window
    resid = (y- yhat)
    
    #for sig2, we sum the square of each element of resid. Thus, do resid^d and sum its elements
    sig2 = sum(resid^2) / (window_size - df)
    
    BIC[j] = log(sig2) + df * aux
    
  } #end of information criteria
  ##After completing the calculation of the BIC criteria for every possible lambda in glmnet, all we have to do is get the optimal:
  lambda_bic = ridge$lambda[which.min(BIC)]
  final_ridge <- glmnet(x,y, lambda = lambda_bic, alpha = 0)
  
return(final_ridge)}

RollingWindow <- function(df, window_size = 1000){
  ##The key_stock argument defines the stock over which we want to model. It defaults to SPY since it is the ETF.
  
  ##The window_size is the size of our rolling window, which is defaulted to be 1000 as defined by the assignment.
  
  ##The week and month arguments are just for specifying the RV_m, RV_w part of the calculation, should we want to change them as well.
  
 
  
  #A given date in dep (dependent variable) needs to match the "future_date" of the independent variables
  
  #OBS: wide_other_stock_df has RV columns for each stock named with the stock`s ticker, while "SPY", in key_stock, has the RV column named "RV"
  
  y_var <- data.matrix(scale(full_data$RV)) ##GLMNET only works with data matrix, so we need to convert it and separate the variables. Ridge Regression also imposes that the response is centered.
  
  x_var <- data.matrix(scale(full_data[,!names(full_data) %in% c('Date', 'RV', 'current_date')])) ##Here, I HAD to remove V, because it's NAN before 2008.
  
  #From full data, we retrieve all rows, and ! indicates the names of columns in full_data that we ignore
  
  
  #Ridge Estimation as in Barbara

  #test to see what each row of code does in one arbitrary window
  
  #close_test
  
  #For a given window of size 1000, we estimate the model for each lambda. Then, we compute AIC and BIC for each lambda, choose lambda that minimizes BIC, and select the 32 coefficients that correspond to the optimal model (29 stocks, 3 lagged RV regressors and an intercept), saving them in the object "ridge". These 32 coefficients for the current window are then stored in ridge_df, which reunites ridge coefficients for all windows, alongside optimal lambda for the window and the forecast. 
  
  #x_var and y_var have 4287 rows (which corresponds to original 4309 observations, minus 21 original observations, minus last observation).
  #window_size = 1000
  
  #This means we will have 4287-1000+1=3288 windows. The first one goes from 1 to 1000,the last one from 3288 to 4287
  
  #An external loop will define a window starting at i, and through it we will select, in x_var and y_var, observations [i:i+ window_size - 1], and used it as a glmnet() argument
  
  window_size <- 1000
  num_windows <- days - 21 - 1 - window_size +1
  
  #initializing the objects
  lambda_ridge <- rep(NA, num_windows)
  forecast_ridge <- rep(NA, num_windows)
  betas_ridge <- matrix(nrow = dim(x_var)[2], ncol = num_windows ) #we ignore the intercept, as we are interest in the model`s regressors (there are 31 = dim(x_var)[2]))) 
  i <- 1
  for (i in 1:num_windows){
    #i = 1 #test
    ridge = glmnet(x_var[i:(window_size+i-1),], y_var[i:(window_size+i-1)], alpha = 0)
    
    x_sc = x_var[i:(window_size+i-1),]
    
    #initializing the BIC vector
    BIC = rep(NA, length(ridge$lambda))
   
    
    #calculating information criteria
    for (j in 1:length(ridge$lambda)) { 
      
      #degrees of freedom
      lambda_diag = ridge$lambda[j] * diag(ncol(x_sc)) ##This is Lambda * Identity
      xx = t(x_sc) %*% x_sc  #transpose of x multiplied (in the matrix sense) by x
      df = sum(diag(x_sc %*% solve(xx + lambda_diag) %*% t(x_sc))) + 1 #+1 due to the the intercept! 
      
      #In slide 6 of lecture 2, part 3, we have that df = tr(x(x'*x+lambda*i)^(-1)*x')
      #the term (x'*x+lambda*i)^(-1) is obtained by inverting (x'*x+lambda*i), hence we use solve()
      #why +1?
      
      #sigma^2(lambda)
     
      #for yhat, we take into account also the intercept, and for this, we add to X a column of constant 1, which when multiplied by coef(ridge[intercept]), will yield the correct value
      yhat = cbind(1, x_var[i:(window_size+i-1),]) %*% coef(ridge)[,j]
      
      #resid is a vector composed of 1000 elements, each one the residual for each observation in the window
      resid = (y_var[(i):(window_size+i-1)] - yhat)
      
      #for sig2, we sum the square of each element of resid. Thus, do resid^d and sum its elements
      sig2 = sum(resid^2) / (window_size - df)
      
      BIC[j] = log(sig2) + df * (log(window_size)/window_size)
      
    } #end of information criteria
    
    #optimal lambda for each window
    lambda_ridge[i] = ridge$lambda[which.min(BIC)]
    betas_ridge[,i] = ridge$beta[,which.min(BIC)]
    
    #predicting
    #new_x = model_vars %>% slice(window + i) %>% select(-Date, -SPY_plus1) %>% data.matrix()
    
    #f_ridge[i] <- predict(ridge, newx = new_x, s = lambda_ridge[i])
    
  }
  
  
  
  
  
  
  
  
  
  #Now for the rolling window loop in itself:
  #Since month is the biggest order of our model, I need to have 22 observations (or however many I set)
  ##Therefore, I need to start in observation 22, so that I have the past 21 observations! 
  ##So I always start in 
  ##Say my window is of size 3. Then I would get observations 22, 23, 24.
  ##The end of my interval will be at window_Size -1  + i + month -1 => 2 + 22 = 24
  
  ridge_df <- data.frame(matrix(NA, nrow = days-window_size+1, ncol = 32))#THIS SHOULD BE NUMBER OF WINDOWS TODO
  lambda_list <- vector(mode = 'list', length = days-window_size+1)

  for(i in 1:(days-window_size+1)){ ##This cannot go on until days, this needs to stop at the last possible window. ##days - window_size +1 maybe
    
    beg <- i + (month - 1) ##Just to clear notation, I wanted to create this variable for the interval's beginning.
    key_window <- key_stock_df[(beg+1):(window_size + beg),]  ##This creates the data subset we wish to use ##TODO: should this use the COMPLETE df or the separate?
    
   
    ##However, this does not seal the deal. Once inside this window, we need to construct the variables. This requires us to loop again?
    ##If our window_size is indeed 3, and we have observations 22, 23, 24, we need to build RV_m for all these
    ##RV_{m, 22} is 1/22 * sum(RV of 22 to 1), RV_{m,23} is 1/22 * sum(RV of 23 to 2), etc. 
    ##So RV_{m,i} needs to go from i+month-1 to i, then from i+month-1+1 to i+1, etc. 
    ##For week, we start at i+beg (22) and we need to go until 18, which means (i+beg) - (week - 1) = 
    RV_m <- vector(mode = 'list', length = window_size)
    RV_w <- vector(mode = 'list', length = window_size)
    
    RV_t <- vector(mode = 'list', length = window_size)
    current_date <- vector(mode = 'list', length = window_size)
    future_date <- vector(mode = 'list', length = window_size)
    
    date_list <- key_stock_df$Date
      for(k in 0:(window_size-1)){
        RV_m[[(k+1)]] <- (1/month) * sum(( key_stock_df$RV[(beg+k):(i+k)]))
        RV_w[[(k+1)]]<- (1/week) * sum(( key_stock_df$RV[(beg+k):(beg+k - (week-1))]))
        RV_t[[(k+1)]] <- key_stock_df$RV[(beg+k)]
        current_date[[(k+1)]] <- key_stock_df$Date[(beg+k)]
        future_date[[(k+1)]] <- key_stock_df$Date[(beg+k+1)]
      }
    ##Now that we've built all of our variables as lists, we wish to assemble them into a dataframe,
    ##We do so by using I(list) to consider it as a column, and then unlist so that the datatype is not a list itself
    independent_variables <-  
      data.frame('month_RV' = unlist(I(RV_m)), 'week_RV' = unlist(I(RV_w)), 'previous_RV' = unlist(I(RV_t)), 'current_date' = unlist(I(current_date)), 'future_date' = unlist(I(future_date)))
    
    
    independent_variables$current_date <- as.Date(independent_variables$current_date, origin = '1970-01-01') ##R likes to convert dates to int (nb of days since 1970-01-01), this converts it back
    
    
    independent_variables$future_date <- as.Date(independent_variables$future_date, origin = '1970-01-01')                
    independent_variables <- inner_join(independent_variables, wide_other_stock_df, by = c('current_date' = 'Date' )) ## Now, we want to join this with the RV_{t} of all other stocks
    ##Finally, I want to join this with the dependent variable which we separated in key_window.
    full_data <- key_window %>% select(Date, VOL, chosen_variable) %>% inner_join(independent_variables, by = c('Date' = 'future_date'))
    
   
    
    y_var <- data.matrix(scale(full_data$RV)) ##GLMNET only works with data matrix, so we need to convert it and separate the variables. Ridge Regression also imposes that the response is centered.
    x_var <- data.matrix(scale(full_data[,!names(full_data) %in% c('Date', 'VOL', 'RV', 'current_date')])) ##Here, I HAD to remove V, because it's NAN before 2008.
    names(ridge_df) <- names(x_var)
    ##If I remove it as planned from my dataset, it won't be a problem anymore.
    # Setting the range of lambda values
    
    # Using glmnet function to build the ridge regression in r
   
    lambda_sequence <- 10^seq(4, -4, by = -.1) ##Create a lambda sequence to test
  
    aic <- c()
    bic <- c()
    for (lambda in 1:length(lambda_sequence)){
     
      model <- glmnet(x_var, y_var, alpha = 0, lambda = lambda_sequence[lambda])
      betas <- as.vector((as.matrix(coef(model))[-1, ]))
      resid <- y_var - (x_var %*% betas)
      # Compute hat-matrix and degrees of freedom
      ld <- lambda_sequence[lambda] * diag(ncol(x_var))
      H <- x_var %*% solve(t(x_var) %*% x_var + ld) %*% t(x_var)
      df <- tr(H)
      # Compute information criteria
      aic[lambda] <- nrow(x_var) * log(t(resid) %*% resid) + 2 * df
      bic[lambda] <- nrow(x_var) * log(t(resid) %*% resid) + 2 * df * log(nrow(x_var))
    }
    
    lambda_bic <- lambda_sequence[which.min(bic)] #Chooses the best lambda for BIC criteria.
    lambda_list[[i]] <- lambda_bic
    ridge <-  tidy(glmnet(x_var, y_var, alpha = 0, lambda = lambda_bic)) ##Creates DF for Ridge with optimal Lambda
    ridge_df[i:i,] = ridge$estimate
    names(ridge_df) <- ridge$term

    
 }
  
  
  
  
testeHd <- ic.glmnet(x_var, y_var, crit = 'bic', alpha = 0, lambda = lambda_sequence)  

  
  lambda_list[i]
  
  
  
  
  return()
}
