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
BICModels <- function(x,y, window_size){
  #ridge <- glmnet(x, y, alpha = 0) ##This makes a ridge model with the function variables. 
  #lasso <- glmnet(x,y,alpha = 1) ##This creates a LASSO model with the function variables.
  #initializing the BIC vector
  #BIC_ridge = rep(NA, length(ridge$lambda))
 
  
  ##The BIC criteria needs to calculate df(lambda), which is given by trace (or sum of the elements of diag)
  ##The matrix is given by X * Inverse((X'X + lambda * Identity)) * X'
  x_squared <- t(x) %*% x #transpose of x multiplied (in the matrix sense) by x
  
  identity_matrix <- diag(ncol(x)) ##This creates the identity matrix I.
  aux <-(log(window_size)/window_size)
  
  #calculating information criteria for ridge DISABLE 
  # 
  #    foreach (j = 1:length(ridge$lambda)) %do% { 
  # #   
  # #   #degrees of freedom
  # #   lambda_diag = ridge$lambda[j] * identity_matrix ##This is Lambda * Identity
  # #   
  # #   
  # #   
  # #   df_ridge = sum(diag(x %*% solve(x_squared + lambda_diag) %*% t(x))) + 1 #+1 due to the the intercept! 
  # #   ##This completes the degrees of freedom calculation!
  # #   #In slide 6 of lecture 2, part 3, we have that df = tr(x(x'*x+lambda*i)^(-1)*x')
  # #   #the term (x'*x+lambda*i)^(-1) is obtained by inverting (x'*x+lambda*i), hence we use solve()
  # #   
  # #   
  # #   #sigma^2(lambda)
  # #   
  # #   #for yhat, we take into account also the intercept, and for this, we add to X a column of constant 1, which when multiplied by coef(ridge[intercept]), will yield the correct value
  # #   yhat = cbind(1, x) %*% coef(ridge)[,j]
  # #   
  # #   #resid is a vector composed of 1000 elements, each one the residual for each observation in the window
  # #   resid = (y- yhat)
  # #   
  # #   #for sig2, we sum the square of each element of resid. Thus, do resid^d and sum its elements
  # #   sig2 = sum(resid^2) / (window_size - df_ridge)
  # #   
  # #   BIC_ridge[j] = log(sig2) + df_ridge * aux
  # #   
  # #   log(sig2) + sum(diag(x %*% solve(x_squared + ridge$lambda[j] * identity_matrix) %*% t(x))) + 1 * aux
  # #   
  # # }
  # #end of information criteria for ridge
  # ##This loop was simply too computationally intensive, so we decided to use the degrees of freedom like the other models.
  # ##Running this loop on smaller intervals (less windows, for instance) yielded results that were not significantly different than the ones produced by this simplification.
  #      
  # #Simpler ridge:
  # yhat = cbind(1,x) %*% coef(ridge)
  # resid = y - yhat
  # resid2 = resid ^2
  # sig2 = colSums(resid2)/ (window_size - ridge$df)
  # BIC_ridge = log(sig2) + ridge$df * aux
  # #Now for the Lasso
  # 
  # BIC_lasso = rep(NA, length(lasso$lambda))
  # 
  # yhat = cbind(1,x) %*% coef(lasso)
  # resid = y - yhat
  # resid2 = resid ^2
  # sig2 = colSums(resid2)/ (window_size - lasso$df)
  # BIC_lasso = log(sig2) + lasso$df * aux
  # 
  # 
  # ##After completing the calculation of the BIC criteria for every possible lambda in glmnet, all we have to do is get the optimal:
  # lambda_ridge = ridge$lambda[which.min(BIC_ridge)]
  # lambda_lasso = lasso$lambda[which.min(BIC_lasso)]
  # 
  # final_ridge <- glmnet(x,y, lambda = lambda_ridge, alpha = 0)
  # final_lasso <- glmnet(x,y, lambda = lambda_lasso, alpha = 1)
  # ##Now, the AdaLasso
  # tau <- 1 
  # first_step_coef <- coef(final_lasso)[-1] # Since p < T, we use Ridge as the first step model and exclude the intercept.
  # penalty_factor <- abs(first_step_coef)**-tau
  # adalasso <- glmnet(x,y, penalty.factor = penalty_factor)
  # #calculating information criteria for adaLASSO
  # BIC_adalasso = rep(NA, length(adalasso$lambda))
  # yhat = cbind(1, x) %*% coef(adalasso)
  # resid = y - yhat
  # resid2 = resid ^2
  # sig2 = colSums(resid2)/ (window_size - adalasso$df)
  # BIC_adalasso = log(sig2) + adalasso$df * aux
  # 
  # 
  # lambda_adalasso = adalasso$lambda[which.min(BIC_adalasso)]
  # final_adalasso <- glmnet(x,y, lambda = lambda_adalasso, alpha = 1, penalty.factor = penalty_factor)
  # 
  # ##Elastic Net
  # elastic_net <- glmnet(x,y, alpha = 0.5)
  # ##initializing the BIC vector for elastic net
  # BIC_elastic_net = rep(NA, length(elastic_net$lambda))
  # yhat = cbind(1, x) %*% coef(elastic_net)
  # resid = y - yhat
  # resid2 = resid ^2
  # sig2 = colSums(resid2)/ (window_size - elastic_net$df)
  # BIC_elastic_net = log(sig2) + elastic_net$df * aux
  # #calculating information criteria for elastic net
  # lambda_elastic_net = elastic_net$lambda[which.min(BIC_elastic_net)]
  # final_elastic_net <- glmnet(x,y, lambda = lambda_elastic_net, alpha = 0.5)
  # 
  # 
  # #Adaptative Elastic Net
  # tau <- 1 
  # first_step_coef <- coef(final_elastic_net)[-1] # Since p < T, we use Elastic Net as the first step model and exclude the intercept.
  # penalty_factor <- abs(first_step_coef)**-tau
  # ada_elastic_net <- glmnet(x,y, penalty.factor = penalty_factor)
  # #calculating information criteria for adaLASSO
  # BIC_ada_elastic_net = rep(NA, length(ada_elastic_net$lambda))
  # yhat = cbind(1, x) %*% coef(ada_elastic_net)
  # resid = y - yhat
  # resid2 = resid ^2
  # sig2 = colSums(resid2)/ (window_size - ada_elastic_net$df)
  # BIC_ada_elastic_net = log(sig2) + ada_elastic_net$df * aux
  # lambda_ada_elastic_net = ada_elastic_net$lambda[which.min(BIC_ada_elastic_net)]
  # final_ada_elastic_net <- glmnet(x,y,lambda = lambda_ada_elastic_net, alpha = 0.5, penalty.factor = penalty_factor)
  
  
  ##Now for Bagging
  
  y_un <- unlist(y)
  y_un <- as.numeric(y_un)
  
  
  num_bootstrap <- 100
  final_bagging <- HDeconometrics::bagging(data.matrix(x), y_un,fn=NULL,R=num_bootstrap,l=3,sim="fixed", pre.testing = "joint")
  
  
  ##Complete Subset Regression
  
  final_csr <- HDeconometrics::csr(x, y, K = 27, k = 24, fixed.controls = NULL)
  
  #Neural Networks
  #Prior to applying the networks on the data, we need to make a choice on the number of neurons and the number of layers for each network. Determining the number of neurons and layers is driven by many factors such as the number of variables in the model, the number of data points, etc. In order to avoid reporting biased results, we run each network on a range of different combinations of layers and neurons. This allows us to monitor the performance of the network over different network architectures and to summarize the network performance. Although there is no clear rule on selecting the number of neurons and layers, we follow Demuth et al. (2014) who argue that the number of neurons should be lower than the number of variables used in the network. Furthermore, the number of hidden layers should not be more than two to three because most problems are tackled even with one hidden layer. Adding many hidden layers on small data sets (less than one million observations) does not result in a better performance. T#herefore, we decide to report the performance of the network on a combination of neurons that range from 1 to 25 and hidden layers that range from 1 to 3. 
  
  #linear regression could be seen as a particular case of the model explored here, in which there is a unique neuron, and g(x)=x.
  
  #Referências para number of neurons and layers: https://www.tandfonline.com/doi/full/10.1080/14697688.2019.1633014
  
  final_nnbr <- brnn(x,y,neurons=15) #31 regressors, therefore let's go for 31 neurons
  
  #Random Forest:
  
  z <- data.frame(x)
  z$y <- y
  final_random_forest <- rangerts::rangerts(y~ ., data = z,
                                                     num.trees = 100,
                                                     mtry = floor(sqrt(ncol(z))),
                                                     replace = T, # default = T too
                                                     seed = 1,
                                                     bootstrap.ts = "moving",
                                                     block.size = 365)
  
  #list('ridge' = final_ridge, 'lasso' = final_lasso, 'adalasso' = final_adalasso, 'elastic_net' = final_elastic_net, 'ada_elastic_net' = final_ada_elastic_net, 
  return_list <-   list('bagging'=final_bagging, 'csr'=final_csr, 'nn' = final_nnbr, 'rf' = final_random_forest)

  return(return_list)}

RollingWindow <- function(df, window_size = 1000, month = 22){

  
  
  y_var <- log(data.matrix(df$RV)) ##GLMNET only works with data matrix, so we need to convert it and separate the variables. Ridge Regression also imposes that the response is centered.
  
  x_var <- log(data.matrix(df[,!names(df) %in% c('Date', 'RV', 'current_date')])) ##Here, I HAD to remove V, because it's NAN before 2008.
  
  #From full data, we retrieve all rows, and ! indicates the names of columns in full_data that we ignore
  

  #For a given window of size 1000, we estimate the model for each lambda. Then, we compute AIC and BIC for each lambda, choose lambda that minimizes BIC, and select the 32 coefficients that correspond to the optimal model (29 stocks, 3 lagged RV regressors and an intercept), saving them in the object "ridge". These 32 coefficients for the current window are then stored in ridge_df, which reunites ridge coefficients for all windows, alongside optimal lambda for the window and the forecast. 
  
  #x_var and y_var have 4287 rows (which corresponds to original 4309 observations, minus 21 original observations, minus last observation).
  #window_size = 1000
  
  #This means we will have 4287-1000+1=3288 windows. The first one goes from 1 to 1000,the last one from 3288 to 4287
  
  #An external loop will define a window starting at i, and through it we will select, in x_var and y_var, observations [i:i+ window_size - 1], and used it as a glmnet() argument
  days <- dim(y_var)[[1]] ##This will give us the length of our dataset
  #num_windows <- days - (month - 1)  - window_size Old before adjustment 
  num_windows <- days  - window_size
  #initializing the objects
  # lambda_ridge <- rep(NA, num_windows)
  # lambda_lasso <- rep(NA, num_windows)
  # lambda_adalasso <- rep(NA, num_windows)
  # lambda_elastic_net <-  rep(NA, num_windows)
  # lambda_ada_elastic_net <- rep(NA, num_windows)
  # ##Forecasts:
  # forecast_ridge <- rep(NA, num_windows)
  # forecast_lasso <- rep(NA, num_windows)
  # forecast_adalasso <- rep(NA, num_windows)
  # forecast_elastic_net <-  rep(NA, num_windows)
  # forecast_ada_elastic_net <- rep(NA, num_windows)
  forecast_bagging <- rep(NA, num_windows)
  forecast_csr <- rep(NA, num_windows)
  forecast_nnbr <- rep(NA, num_windows)
  forecast_random_forest <- rep(NA, num_windows)
  
  
  ##MSE
  # MSE_ridge <-  rep(NA, num_windows)
  # MSE_lasso <-  rep(NA, num_windows)
  # MSE_adalasso <-  rep(NA, num_windows)
  # MSE_elastic_net <-  rep(NA, num_windows)
  # MSE_ada_elastic_net <-  rep(NA, num_windows)
  MSE_bagging <-  rep(NA, num_windows)
  MSE_csr <-  rep(NA, num_windows)
  MSE_nnbr <-  rep(NA, num_windows)
  MSE_random_forest <-  rep(NA, num_windows)
  ##Beta matrices
  # betas_ridge <- matrix(nrow = dim(x_var)[2], ncol = num_windows  ) #we ignore the intercept, as we are interest in the model`s regressors (there are 31 = dim(x_var)[2]))) 
  # betas_lasso <- matrix(nrow = dim(x_var)[2], ncol = num_windows ) #we ignore the intercept, as we are interest in the model`s regressors (there are 31 = dim(x_var)[2]))) 
  # betas_adalasso <- matrix(nrow = dim(x_var)[2], ncol = num_windows ) #we ignore the intercept, as we are interest in the model`s regressors (there are 31 = dim(x_var)[2]))) 
  # betas_elastic_net <- matrix(nrow = dim(x_var)[2], ncol = num_windows )
  # betas_ada_elastic_net <- matrix(nrow = dim(x_var)[2], ncol = num_windows )
  betas_bagging <- matrix(nrow = dim(x_var)[2], ncol = num_windows )
  betas_csr <- matrix(nrow = dim(x_var)[2], ncol = num_windows )
  nn_importance <- matrix(nrow = dim(x_var)[2], ncol = 33)
  rf_importance <- matrix(nrow = dim(x_var)[2], ncol = 33)
  bagging_importance <- matrix(nrow = dim(x_var)[2], ncol = 33)
  csr_importance <- matrix(nrow = dim(x_var)[2], ncol = 33)
  relevant_windows <- seq(1, 3201, 100) #These are the windows for my variable importance calculation. 
  foreach (i = 1:num_windows) %do% {
    #i = 1 #test
    x <- x_var[i:(window_size+i-1),]
    y <-  y_var[i:(window_size+i-1)]
    models = BICModels(x,y, window_size)
    
    # ridge = models$ridge
    # lasso = models$lasso
    # adalasso = models$adalasso
    # elastic_net = models$elastic_net
    # ada_elastic_net = models$ada_elastic_net
    bagging = models$bagging
    csr = models$csr
    nn = models$nn
    random_forest = models$rf
    #optimal lambda for each window
    # lambda_ridge[i] = ridge$lambda
    # lambda_lasso[i] = lasso$lambda
    # lambda_adalasso[i] = adalasso$lambda
    # lambda_elastic_net[i] = elastic_net$lambda
    # lambda_ada_elastic_net[i] = ada_elastic_net$lambda
    # z <- as.list(ridge$beta[,])
    # betas_ridge[,i] = unlist(z)
    # 
    # betas_lasso[,i] = unlist(as.list(lasso$beta[,]))
    # betas_adalasso[,i] = unlist(as.list(adalasso$beta[,]))
    # betas_elastic_net[,i] = unlist(as.list(elastic_net$beta[,]))
    # betas_ada_elastic_net[,i] = unlist(as.list(ada_elastic_net$beta[,]))
    
    bagging_coefficients <- bagging$coefficients[,-1] ##Get the coefficients of the bagging model but without intercept
    betas_bagging[,i] <- t(as.matrix(colMeans(bagging_coefficients))) 
    csr_coefficients <- csr$coefficients[,-1]
    betas_csr[,i] <- t(as.matrix(colMeans(csr_coefficients)))
    #predicting
    new_x = as.matrix(t(x_var[(window_size + i),])) ##Now, we get one step ahead values of x, make them into a matrix and use it for prediction!
    new_y = y_var[(window_size + i)]
    # forecast_ridge[i] = predict(ridge, newx = new_x)
    # forecast_lasso[i] = predict(lasso, newx =  new_x)
    # forecast_adalasso[i] = predict(adalasso, newx =  new_x)
    # forecast_elastic_net[i] = predict(elastic_net, newx =  new_x)
    # forecast_ada_elastic_net[i] = predict(ada_elastic_net, newx =  new_x)
    forecast_bagging[i] = predict(bagging, newdata =  new_x)
    
    forecast_csr[i] = predict(csr, newdata =  new_x)
    forecast_nnbr[i] = predict(nn, newdata =  new_x) 
    forecast_random_forest[i] = predict(random_forest, data =  new_x)$predictions
    #Broken
    # MSE_ridge[i] = (new_y - forecast_ridge[i])^2
    # MSE_lasso[i] = (new_y - forecast_lasso[i])^2
    # MSE_adalasso[i] = (new_y - forecast_adalasso[i])^2
    # MSE_elastic_net[i] = (new_y - forecast_elastic_net[i])^2
    # MSE_ada_elastic_net[i] = (new_y - forecast_ada_elastic_net[i])^2
    
    MSE_bagging[i] = (new_y - forecast_bagging[i])^2
    MSE_csr[i] = (new_y - forecast_csr[i])^2
    MSE_nnbr[i] = (new_y - forecast_nnbr[i])^2
    MSE_random_forest[i] = (new_y - forecast_random_forest[i])^2
    
    j <- 1
    if(i %in% relevant_windows){
      bagging_e <- DALEX::explain(bagging, data = x, y = y, model_info = list(package = 'HDeconometrics', ver = '0.1.0', type = 'regression'), verbose = FALSE)
      vi_bagging <- model_parts(bagging_e, loss_function = loss_root_mean_square,
                                B = 50,
                                N=NULL,
                                type = "ratio")
      median_loss_bagging <- vi_bagging %>% filter(variable != '_baseline_') %>% filter(variable != '_full_model_') %>% group_by(variable) %>% summarise(median = median(dropout_loss))
      bagging_importance[,j] <- median_loss_bagging$median
      
      csr_e <- DALEX::explain(csr, data = x, y = y, model_info = list(package = 'HDeconometrics', ver = '0.1.0', type = 'regression'), verbose = FALSE)
      vi_csr <- model_parts(csr_e, loss_function = loss_root_mean_square,
                            B = 50,
                            N=NULL,
                            type = "ratio")
      median_loss_csr <- vi_csr %>% filter(variable != '_baseline_') %>% filter(variable != '_full_model_') %>% group_by(variable) %>% summarise(median = median(dropout_loss))
      csr_importance[,j] <- median_loss_csr$median
      
      rf_predict <- function(X.model, newdata) {
        predict(X.model, data.frame(newdata))$predictions}
      rf_e <- DALEX::explain(random_forest, data = x, y = y, model_info = list(package = 'rangerts', ver = '0.0.3', type = 'regression'), predict_function = rf_predict, verbose = FALSE)
      vi_rf <- model_parts(rf_e,  loss_function = loss_root_mean_square,
                           B = 50,
                           N=NULL,
                           type = "ratio")
      
      median_loss_rf <- vi_rf %>% filter(variable != '_baseline_') %>% filter(variable != '_full_model_') %>% group_by(variable) %>% summarise(median = median(dropout_loss))
      
      rf_importance[,j] <- median_loss_rf$median
      
      nn_e<-DALEX::explain(brnn,data=x,y=y,model_info=list(package="brnn",ver="0.8",type="regression"), verbose = FALSE)
      
      vi_nn<-DALEX::model_parts(explainer = nn_e, 
                                loss_function = loss_root_mean_square,
                                B = 50,
                                N=NULL,
                                type = "ratio")
      median_loss_nn <- vi_nn %>% filter(variable != '_baseline_') %>% filter(variable != '_full_model_') %>% group_by(variable) %>% summarise(median = median(dropout_loss))
      nn_importance[,i] <- median_loss_nn$median
      j <- j + 1
      
    } #I only calculate the variable importance in these windows.
  }
  forecastHAR = HARForecast(RM = y_var, nRoll = num_windows, nAhead = 1)
  MSE_HAR = as.numeric(forecastRes(forecastHAR))^2  ##Forecast Res extracts residuals of the HAR forecast, and we get it as a vector.
  # mean(MSE_HAR)
  # mean(MSE_ridge)/mean(MSE_HAR)
  # mean(MSE_lasso)/mean(MSE_HAR)
  # mean(MSE_adalasso)/mean(MSE_HAR)
  # mean(MSE_elastic_net)/ mean(MSE_HAR)
  # mean(MSE_ada_elastic_net) / mean(MSE_HAR)
  # 
  # dm.test(MSE_HAR, MSE_ridge)
  # dm.test(MSE_HAR, MSE_lasso)
  # dm.test(MSE_HAR, MSE_elastic_net)
  # 
  # dm.test(MSE_HAR, MSE_adalasso)
  # 
  # dm.test(MSE_HAR, MSE_ada_elastic_net)
  # return_list <- list('betas_lasso' = betas_lasso, 'betas_ridge' = betas_ridge, 'betas_adalasso' = betas_adalasso, 
  #                     'betas_elastic_net' = betas_elastic_net, 'betas_ada_elastic_net' = betas_ada_elastic_net, 'betas_bagging' = betas_bagging, 
  #                     'betas_csr' = betas_csr, 'MSE_HAR' = MSE_HAR, 'MSE_ridge' = MSE_ridge, 'MSE_lasso' = MSE_lasso, 'MSE_adalasso' = MSE_adalasso,
  #                     'MSE_elastic_net' = MSE_elastic_net, 'MSE_ada_elastic_net' = MSE_ada_elastic_net, 'MSE_bagging' = MSE_bagging, 'MSE_csr' = MSE_csr, 'MSE_nnbr' = MSE_nnbr)
  # 
  # 
  
  return_list <- list('betas_bagging' = betas_bagging, 
                      'betas_csr' = betas_csr, 'MSE_HAR' = MSE_HAR, 'MSE_bagging' = MSE_bagging, 'MSE_csr' = MSE_csr, 'MSE_nnbr' = MSE_nnbr,'MSE_random_forest' = MSE_random_forest
                      , 'nn_importance' = nn_importance, 'rf_importance' = rf_importance, 'csr_importance' = csr_importance, 'bagging_importance' = bagging_importance)
  
  
 return(return_list) }
  
  
  
  
  
  
  
