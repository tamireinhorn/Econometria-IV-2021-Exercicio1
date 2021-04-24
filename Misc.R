###This should be the R script where we save our functions, because this makes it all much tidier.


##This function will create our variables as desired, in order to call upon them the models with rolling windows.
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





##TODO Augment this function to make it work on ALL possible models. 
BICModels <- function(x,y){
  ridge <- glmnet(x, y, alpha = 0) ##This makes a ridge model with the function variables. 
  lasso <- glmnet(x,y,alpha = 1) ##This creates a LASSO model with the function variables.
  #initializing the BIC vector
  BIC_ridge = rep(NA, length(ridge$lambda))
 
  
  ##The BIC criteria needs to calculate df(lambda), which is given by trace (or sum of the elements of diag)
  ##The matrix is given by X * Inverse((X'X + lambda * Identity)) * X'
  x_squared <- t(x) %*% x #transpose of x multiplied (in the matrix sense) by x
  
  identity_matrix <- diag(ncol(x)) ##This creates the identity matrix I.
  aux <-(log(window_size)/window_size)
  
  #calculating information criteria for ridge
 
  for (j in 1:length(ridge$lambda)) { 
    
    #degrees of freedom
    lambda_diag = ridge$lambda[j] * identity_matrix ##This is Lambda * Identity
    
    
    
    df_ridge = sum(diag(x %*% solve(x_squared + lambda_diag) %*% t(x))) + 1 #+1 due to the the intercept! 
    ##This completes the degrees of freedom calculation!
    #In slide 6 of lecture 2, part 3, we have that df = tr(x(x'*x+lambda*i)^(-1)*x')
    #the term (x'*x+lambda*i)^(-1) is obtained by inverting (x'*x+lambda*i), hence we use solve()
    
    
    #sigma^2(lambda)
    
    #for yhat, we take into account also the intercept, and for this, we add to X a column of constant 1, which when multiplied by coef(ridge[intercept]), will yield the correct value
    yhat = cbind(1, x) %*% coef(ridge)[,j]
    
    #resid is a vector composed of 1000 elements, each one the residual for each observation in the window
    resid = (y- yhat)
    
    #for sig2, we sum the square of each element of resid. Thus, do resid^d and sum its elements
    sig2 = sum(resid^2) / (window_size - df_ridge)
    
    BIC_ridge[j] = log(sig2) + df_ridge * aux
    
  } #end of information criteria for lasso
  BIC_lasso = rep(NA, length(lasso$lambda))
  
  #calculating information criteria for LASSO
  for (j in 1:length(lasso$lambda)) { 
    
        yhat = cbind(1, x) %*% coef(lasso)[,j]
    
    #resid is a vector composed of 1000 elements, each one the residual for each observation in the window
        resid = (y- yhat)
        df_lasso = lasso$df[j]
    #for sig2, we sum the square of each element of resid. Thus, do resid^d and sum its elements
        sig2 = sum(resid^2) / (window_size - df_lasso)
        
    BIC_lasso[j] = log(sig2) + df_lasso * aux
    
  } #end of information criteria for lasso
  
  ##After completing the calculation of the BIC criteria for every possible lambda in glmnet, all we have to do is get the optimal:
  lambda_ridge = ridge$lambda[which.min(BIC_ridge)]
  lambda_lasso = lasso$lambda[which.min(BIC_lasso)]
  final_ridge <- glmnet(x,y, lambda = lambda_ridge, alpha = 0)
  final_lasso <- glmnet(x,y, lambda = lambda_lasso, alpha = 1)
  ##Now, the AdaLasso
  tau <- 1 
  first_step_coef <- coef(final_ridge)[-1] # Since p < T, we use Ridge as the first step model and exclude the intercept.
  penalty_factor <- abs(first_step_coef)**-tau
  adalasso <- glmnet(x,y, penalty.factor = penalty_factor)
  
  BIC_adalasso = rep(NA, length(adalasso$lambda))
  
  #calculating information criteria for adaLASSO
  for (j in 1:length(adalasso$lambda)) { 
    
    yhat = cbind(1, x) %*% coef(adalasso)[,j]
    
    #resid is a vector composed of 1000 elements, each one the residual for each observation in the window
    resid = (y- yhat)
    df_adalasso = adalasso$df[j]
    #for sig2, we sum the square of each element of resid. Thus, do resid^d and sum its elements
    sig2 = sum(resid^2) / (window_size - df_adalasso)
    
    BIC_adalasso[j] = log(sig2) + df_adalasso * aux
    
  } #end of information criteria for adalasso
  lambda_adalasso = adalasso$lambda[which.min(BIC_adalasso)]
  final_adalasso <- glmnet(x,y, lambda = lambda_adalasso, alpha = 1, penalty.factor = penalty_factor)
  return_list <- list('ridge' = final_ridge, 'lasso' = final_lasso, 'adalasso' = final_adalasso)
return(return_list)}

RollingWindow <- function(df, window_size = 1000, month = 22){

  
  
  y_var <- data.matrix(scale(df$RV)) ##GLMNET only works with data matrix, so we need to convert it and separate the variables. Ridge Regression also imposes that the response is centered.
  
  x_var <- data.matrix(scale(df[,!names(full_data) %in% c('Date', 'RV', 'current_date')])) ##Here, I HAD to remove V, because it's NAN before 2008.
  
  #From full data, we retrieve all rows, and ! indicates the names of columns in full_data that we ignore
  

  #For a given window of size 1000, we estimate the model for each lambda. Then, we compute AIC and BIC for each lambda, choose lambda that minimizes BIC, and select the 32 coefficients that correspond to the optimal model (29 stocks, 3 lagged RV regressors and an intercept), saving them in the object "ridge". These 32 coefficients for the current window are then stored in ridge_df, which reunites ridge coefficients for all windows, alongside optimal lambda for the window and the forecast. 
  
  #x_var and y_var have 4287 rows (which corresponds to original 4309 observations, minus 21 original observations, minus last observation).
  #window_size = 1000
  
  #This means we will have 4287-1000+1=3288 windows. The first one goes from 1 to 1000,the last one from 3288 to 4287
  
  #An external loop will define a window starting at i, and through it we will select, in x_var and y_var, observations [i:i+ window_size - 1], and used it as a glmnet() argument
  days <- dim(key_stock_df)[[1]] ##This will give us the length of our dataset
  num_windows <- days - (month - 1)  - window_size 
  
  #initializing the objects
  lambda_ridge <- rep(NA, num_windows)
  lambda_lasso <- rep(NA, num_windows)
  lambda_adalasso <- rep(NA, num_windows)
  forecast_ridge <- rep(NA, num_windows)
  forecast_lasso <- rep(NA, num_windows)
  forecast_adalasso <- rep(NA, num_windows)
  betas_ridge <- matrix(nrow = dim(x_var)[2], ncol = num_windows ) #we ignore the intercept, as we are interest in the model`s regressors (there are 31 = dim(x_var)[2]))) 
  betas_lasso <- matrix(nrow = dim(x_var)[2], ncol = num_windows ) #we ignore the intercept, as we are interest in the model`s regressors (there are 31 = dim(x_var)[2]))) 
  betas_adalasso <- matrix(nrow = dim(x_var)[2], ncol = num_windows ) #we ignore the intercept, as we are interest in the model`s regressors (there are 31 = dim(x_var)[2]))) 
  i <- 1
  for (i in 1:num_windows){
    #i = 1 #test
    models = BICModels(x_var[i:(window_size+i-1),], y_var[i:(window_size+i-1)])
    ridge = models$ridge
    lasso = models$lasso
    adalasso = models$adalasso
    #optimal lambda for each window
    lambda_ridge[i] = ridge$lambda
    z <- as.list(ridge$beta[,])
    betas_ridge[,i] = unlist(z)
    lambda_lasso[i] = lasso$lambda
    lambda_adalasso[i] = adalasso$lambda
    betas_lasso[,i] = unlist(as.list(lasso$beta[,]))
    betas_adalasso[,i] = unlist(as.list(adalasso$beta[,]))
    #predicting
    new_x = as.matrix(t(x_var[(window_size + i),])) ##Now, we get one step ahead values of x, make them into a matrix and use it for prediction!
    forecast_ridge[i] = predict(ridge, newx = new_x)
    forecast_lasso[i] = predict(lasso, newx =  new_x)
    forecast_lasso[i] = predict(adalasso, newx =  new_x)
    #f_ridge[i] <- predict(ridge, newx = as.matrix(new_x))
  }
 return(2) }
  
  
  
  
  
  
  
