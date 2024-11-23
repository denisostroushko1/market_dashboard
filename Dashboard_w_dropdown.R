

########################
# LIBRARY AND FUNCTIONS 


library(aws.s3)
library(knitr)
library(rlang)
library(tidyverse)
library(zoo)
library(fpp2)
library(numbers)
library(smooth)
library(Mcomp)
library(roll)
library(data.table)
library(alr4)
library(gridExtra)
library(shinythemes)
library(shiny)
library(shinydashboard)
library(cowplot)
library(plotly)
library(shinyjs)
library(kableExtra)
library(DT)
library(prophet)
library(rstan)
library(TTR)
library(quantmod)

## custom functions 

#*add dashed line to plotly 

hline <- function(y = 0, color = "red") {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color, dash="dash")
  )
}

vline <- function(color = "blue", x0, x1, y0, y1) {
  list(
    type = "line", 
    x0 = x0, 
    x1 = x1, 
    yref = "paper",
    y0 = y0, 
    y1 = y1, 
    line = list(color = color, dash="dash")
  )
}



## custom rounding function 

round_custom <- 
  function(x, magnitude){
    x <- round(x, 0)
    y <- nchar(trunc(x)) - magnitude
    x <- round(round(x,0)/(10^y), 0.1)*(10^y)
  }

#*## format to dollar 
format_dollar <- 
  function(x, round = 2){
    return(
      case_when(
        x>= 0 ~ paste0("$", prettyNum(round(x, round), big.mark = ",")),
        TRUE ~ paste0("-$", prettyNum(round(abs(x), round), big.mark = ","))
      )
    )
  }

##########################
# CONNECT TO S3
source('Connect to S3.R')

############################
# GLOBAL COLORS AND SETTING 

colors_btc <- c( "#E7B800", "#56B4E9", "#66FF00", "#FF0000", "#525b60", "#d900ff")

colors_alt <- c( "#E7B800", "#66FF00", "#FF0000", "#525b60", "#d900ff")

gradient_color_palet <- colorRamp(c("blue", "light green", "red"))

recent_price_plot_width <- 12
recent_risk_plot_width <-   9

## Ploting set up 

legend_global <- list(orientation = 'h',xanchor = "center", x = 0.5) # position of legens on the plot

y_margins = list(l = 50, r = 50) # margins of y axes 

## Opacity level 

opacity_level = 0.5

## Colors to use 

color_1 <- "blue" 
color_2 <- "orange"
color_3 <- "#36454F"
color_4 <- '#db2ea4'
  #"#ADD8E6"

##########################
# FORMATTING VECTORS

btc_value_assets <- c("ETH/BTC", "LINK/BTC", "ADA/BTC", "THETA/BTC", "ZIL/BTC")
usd_big_value_assets <- c("BTC/USD", "ETH/USD", "LINK/USD")
usd_lil_value_assets <- c("ADA/USD", "THETA/USD", "ZIL/USD", "XRP/USD", "VET/USD")

###################
# GLOBAL FUNCTIONS 

## define features from the pre-processed raw data set 

{
# estimate_slopes <- function(DATA){
#         
#   ## historical slopes of price for a 15 day scale. 
#   ## This metric shows average price change on a 15 day scale, as estimated using a simple linear regression models
#   ## we say model because we regress on the original scale to get average $ change
#   ##                                  and on the natural log scale to get average % change 
#   ##  average % change change is used as a detection tool 
#       
#   DAYS <- 15
#   results_obs <- nrow(DATA) 
#     
#   ## store results of looping over the data set in the 'results' data frame. Then merge to the data set 
#   results <- 
#     data.frame(
#       datetime       = as.Date(min(DATA$datetime)), 
#       p_val          = NA, 
#       coeff          = NA, 
#       coeff_p        = NA, 
#       trend_dir      = NA, 
#       trend_strength = NA
#     )
#   
#   for(i in DAYS:results_obs){
#   
#       first <- i - DAYS + 1
#       last  <- i
#       
#       df_local <- DATA[first:last, ]
#     
#       rownames(df_local) <- NULL
#       df_local$local_n <- as.numeric(rownames(df_local))
#       
#       local_price_lm <- lm(price ~ local_n, data= df_local)
#       local_price_lm_percent <- lm(log(price) ~ local_n, data= df_local)
#       
#       sum <- summary(local_price_lm)$coefficients
#       sum_percent <- summary(local_price_lm_percent)$coefficients
#       
#       p_val <- round(sum[,4][2], 2)
#       
#       coeff <- as.numeric(round(sum[,1][2], 1))
#       
#       coeff_p <- round(as.numeric(round(exp(sum_percent[,1][2]), 3)) - 1,3) * 100
#       
#      trend_dir <- 
#       case_when(
#         coeff_p <= -1 ~ "Bearish",
#         coeff_p >= -1 & coeff_p <= -0.5 ~ "Slow Bearish", 
#         coeff_p >= -0.5 & coeff_p <= 0.5 ~ "Sideways", 
#         coeff_p >= 0.5 & coeff_p <= 1 ~ "Slow Bllish", 
#         TRUE ~ "Bullish"
#       )
#       
#     trend_strength <- 
#        case_when(
#          p_val <= .05 ~ "Strong",
#          p_val >= 0.05 & p_val <= 0.3 ~ "Weak", 
#          TRUE ~ "No Trend"
#        )
#     
#     trend_dir <- 
#       case_when(
#         trend_strength == "No Trend" ~ "Sideways", 
#         TRUE ~ trend_dir
#       )
#       
#       ## store results 
#       
#       results[first, ]$datetime <- df_local$datetime[nrow(df_local)]
#       results$p_val[first] <- p_val
#       results$coeff[first] <- coeff
#       results$coeff_p[first] <- coeff_p
#       results$trend_dir[first] <- trend_dir
#       results$trend_strength[first] <- trend_strength
#   }
#   
# #  results <- na.omit(results)
#   
#   DATA <- merge(
#     results, 
#     DATA, 
#     by = "datetime", 
#     all = T
#   )
#   
#   return(DATA)
# }
} # old function to estimate slopes 

estimate_slope <- function(DATA, DAYS){
    
    DATA$coeff_p <- NA
    
    for(i in DAYS:nrow(DATA)){
    coeff_p <- round(summary(lm(log(price) ~ n, data = DATA[i:(i-DAYS), ]))$coefficients[2,1] * 100,2)
    
    DATA$coeff_p[i] <- coeff_p
    }
    return(DATA)
  }

modify_raw_data <- function(DATA, ASSET_NAME){
  
  n_obs <- nrow(DATA)
  
  DATA$n <- seq(from = 1, to = n_obs, by = 1)
 
  #calculate new ATH based on closing price. This is not an entirely accurate method becuase 
  # ATH != closing price, but 
  # a) most data viz. are done with closing price, so we need to be consistent there
  # b) ATH ~ closing price for days where ATH is established, so closing price approximates ATH 
  
  
  DATA$new_ath_flag <- 0
  
  for( i in 2:nrow(DATA)){
    DATA$new_ath_flag[i] <- ifelse(DATA$price[i] > max(DATA$price[1:(i-1)]), 1, 0)
  }
  
  # record most recent ATH based on closing price 
  DATA$recent_ath <- min(DATA$price)
  
  for(i in 2:nrow(DATA)){
    DATA$recent_ath[i] <- max(DATA$price[1:i])
  }
  
  #calculate % that the current price is down from the most recent ATH 
  DATA$p_down_from_ATH <- with(DATA, price/recent_ath - 1)
  
  #get 20 week MA
  DATA$week_ma_20<- frollmean(DATA$price, 20 * 7)
  # get extension fo price from the 20 week moving average, we use this as one of the detection tools 
  DATA$price_ma_ratio <- with(DATA, price / week_ma_20)
  #get rate of change of the 20 week moving average. Essentially, this is volatility of 20 week moving average 
  DATA$ma_roc <- with(DATA, week_ma_20 / lag(week_ma_20))
  
  # get ATH of a 20 week moving average 
  DATA$new_ath_ma <- 0
    
  for( i in 2:nrow(DATA)){
      DATA$new_ath_ma[i] <- ifelse(DATA$week_ma_20[i] > 
                                         max(DATA$week_ma_20[1:(i-1)], na.rm = T), 1, DATA$new_ath_ma[i])
    }
  
  #store asset name 
  DATA$asset <- ASSET_NAME
  
  #volatility
  DATA$volatility <- NA
  DATA$volatility <- with(DATA, price/lag(price))
  DATA$volatility[1] <- 1
  
  ## define transformed volatility: basically in terms of % change 
  DATA$transformed_volatility <- DATA$volatility - 1
  
  DATA$volatility_type <- 
    case_when(
      sign(DATA$transformed_volatility) == 1 ~ "Positive volatility", 
      sign(DATA$transformed_volatility) == 0 ~ "Zero volatility",
      TRUE ~ "Negative volatility"
    )
  #calculate consequtive datys of the same direction volatility 
   ### consequtive days: consequtive day number that volatity is the same direction: positive or negative
      # basically would like to see how many days in a row can volatility go up and down 
      
      DATA$consequtive_days <- 1
      
      #############
      # NOTE: 
      # "sign" is a bad function here because it doesn't work with 0, so it needs to be fixed. 
      
      for(i in 2:nrow(DATA)){
      
        # if volatility is positive or negative both today and yesterday add one day to consequtive streak, 
          # othewise set streak to 1 
        DATA$consequtive_days[i] <- 
          ifelse(
            DATA$volatility_type[i] ==  
              DATA$volatility_type[i-1] , DATA$consequtive_days[i-1] + 1, 1
          )
      }
      
      ## assign ID number to each volatility sequesnce. Easier to identify on the plot 
      DATA$seq_id <- 1
          
          for(i in 2:nrow(DATA)){
            
            ## if previous day volatility is the same sign, i.e. also positive, negative, or zero
            # then keep the same sequence id
            # otherwise add 1 
            
            DATA$seq_id[i] <- 
              ifelse(
                DATA$volatility_type[i] ==  
                    DATA$volatility_type[i-1] ,
                
                DATA$seq_id[i-1],
                DATA$seq_id[i-1] + 1
              )
          }

   DATA$rsi <- RSI(DATA$price, days = 14)

   DATA <- estimate_slope(DATA, DAYS = 15)
   
  ## RETURN FINAL DATA SET 
  return(DATA)
}

## risk metric for BITCOIN
{
# BTC_risk_metric <- 
#   function(DATA, DAY_MA, POWER, AVG_VOLATILITY_TIMEFRAME){
#     
#     DATA$week_ma_50 <- frollmean(DATA$price, DAY_MA * 7)
#     
#     #first row will have its own price as MA 
#     DATA[1,]$week_ma_50 <- DATA[1,]$price
#     
#     max_n <- max(DATA[is.na(DATA$week_ma_50), ]$n)
#     
#     for(i in 2:max_n){
#       DATA[i,]$week_ma_50 <- frollmean(DATA[1:(i),]$price, n = i)[i]
#     }
#     
#     ##STEP 1: ratio of price to Week MA 
#     DATA$price_to_50_week_MA <- with(DATA, price / week_ma_50)
#     
#     ##STEP 2: add time components 
#     DATA$price_to_50_week_MA_w_time <- with(DATA, price_to_50_week_MA * (n^(1/POWER)))
#     
#     ##STEP 3: roling volatility and squishing raw risk metric 
#         
#     DATA$volatility_roll <- frollmean(DATA$volatility, AVG_VOLATILITY_TIMEFRAME)
#     
#     max_n <- max(DATA[is.na(DATA$volatility_roll), ]$n)
#     DATA$volatility_roll[1] <- 1 
#     
#     for(i in 2:max_n){
#       DATA[i,]$volatility_roll <- frollmean(DATA[1:(i),]$volatility, n = i)[i]
#     }
#     DATA$volatility_roll[1] <- 1 
#         
#     DATA$price_to_50_week_MA_w_time_w_vol <- 
#       with(DATA, price_to_50_week_MA_w_time * volatility_roll)
#     
#     DATA$price_to_50_week_MA_w_time_w_vol <- 
#       sqrt(DATA$price_to_50_week_MA_w_time_w_vol)
#     
#     ## RAW RISK DIMINISHING SLOPE
#      slope <- (14.359433 - 15.297704) / (2491 - 1024)
# 
#     #### intercept = y - slope * x 
#     intercept <- 14.65 - slope * 1024
#     ####add slope to the entire data set 
#     a <- 0.1
#     DATA$raw_risk_slope_a <- DATA$n * (slope - (a/1000)) + (intercept - 0.25 + 3*a)
# 
#     DATA$price_to_50_week_MA_w_time_w_vol <- 
#       with(DATA, 
#            case_when(
#              price_to_50_week_MA_w_time_w_vol >= raw_risk_slope_a ~ raw_risk_slope_a,
#              TRUE ~ price_to_50_week_MA_w_time_w_vol
#            ))
#     
#     DATA$scaled_risk <- DATA$price_to_50_week_MA_w_time_w_vol/DATA$raw_risk_slope_a
#     ## finally, calculate scaled risk 
#     
#     
#     DATA$scaled_risk <- 
#       with(DATA,
#              (scaled_risk - min(scaled_risk, na.rm = T))/
#                (max(scaled_risk, na.rm = T) - min(scaled_risk, na.rm = T))
#            )
#     
#     # DATA <- DATA %>% select(-raw_risk_slope_a, -price_to_50_week_MA_w_time_w_vol, -n, 
#     #                           - volatility_roll, -week_ma, -price_to_50_week_MA, -price_to_50_week_MA_w_time)
#     ##FINISHED: Store data 
#     return(DATA)
#     
#   }
}

BTC_risk_metric <- 
  function(DATA, 
           DAY_MA,
            POWER,
            AVG_VOLATILITY_TIMEFRAME,
            Y2_f, 
            Y1_f, 
            X2_f, 
            X1_f,
            
            Y2_f_l,
            Y1_f_l,
            X2_f_l,
            X1_f_l,
        
            POWER_TR
           ){
    
    DATA$week_ma_50 <- frollmean(DATA$price, DAY_MA * 7)
    
    #first row will have its own price as MA 
    DATA[1,]$week_ma_50 <- DATA[1,]$price
    
    max_n <- max(DATA[is.na(DATA$week_ma_50), ]$n)
    
    for(i in 2:max_n){
      DATA[i,]$week_ma_50 <- frollmean(DATA[1:(i),]$price, n = i)[i]
    }
    
    ##STEP 1: ratio of price to Week MA 
    DATA$price_to_50_week_MA <- with(DATA, price / week_ma_50)
    
    ##STEP 2: add time components 
    DATA$price_to_50_week_MA_w_time <- with(DATA, price_to_50_week_MA * (n^(1/POWER)))
    
    ##STEP 3: roling volatility and squishing raw risk metric 
        
    DATA$volatility_roll <- frollmean(DATA$volatility, AVG_VOLATILITY_TIMEFRAME)
    
    max_n <- max(DATA[is.na(DATA$volatility_roll), ]$n)
    DATA$volatility_roll[1] <- 1 
    
    for(i in 2:max_n){
      DATA[i,]$volatility_roll <- frollmean(DATA[1:(i),]$volatility, n = i)[i]
    }
    DATA$volatility_roll[1] <- 1 
        
    DATA$price_to_50_week_MA_w_time_w_vol <- 
      with(DATA, price_to_50_week_MA_w_time * volatility_roll)
    
    DATA$price_to_50_week_MA_w_time_w_vol <- 
      sqrt(DATA$price_to_50_week_MA_w_time_w_vol) ^ POWER_TR
    
    ## RAW RISK DIMINISHING SLOPE
     # slope <- (14.359433 - 15.297704) / (2491 - 1024)
     
     slope <- (Y2_f - Y1_f) / (X2_f - X1_f)

    #### intercept = y - slope * x 
    # intercept <- 14.65 - slope * 1024
    
    intercept <- Y1_f - slope * X1_f
    
    ####add slope to the entire data set 
  #  a <- 0.1
    # DATA$raw_risk_slope_a <- DATA$n * (slope - (a/1000)) + (intercept - 0.25 + 3*a)
    
    DATA$raw_risk_slope_a <-  DATA$n * slope + intercept

    DATA$price_to_50_week_MA_w_time_w_vol <- 
      with(DATA, 
           case_when(
             price_to_50_week_MA_w_time_w_vol >= raw_risk_slope_a ~ raw_risk_slope_a,
             TRUE ~ price_to_50_week_MA_w_time_w_vol
           ))
    
    #DATA$scaled_risk <- DATA$price_to_50_week_MA_w_time_w_vol/DATA$raw_risk_slope_a
    ## finally, calculate scaled risk 
    
    low_data_x <- data.frame(
      n = c(X1_f_l,X2_f_l), 
      y = c(Y1_f_l, Y2_f_l)
    )
    
    model <- lm(y ~ sqrt(n), data = low_data_x)
    
    DATA$raw_lower_slope_a <- predict(model, DATA)
    
    high_data_x <- data.frame(
      n = c(X1_f,X2_f), 
      y = c(Y1_f, Y2_f)
    )
    
    model <- lm(y ~ sqrt(n), data = high_data_x)
    
    DATA$raw_risk_slope_a <- predict(model, DATA)

    DATA$scale_risk_1 <- 
      with(DATA, 
           (price_to_50_week_MA_w_time_w_vol-raw_lower_slope_a)/(raw_risk_slope_a-raw_lower_slope_a)
           )
    
    DATA$scaled_risk <- 
      with(DATA,
           (scale_risk_1 - min(scale_risk_1))/
             (max(scale_risk_1) - min(scale_risk_1))
           )
    
    DATA <- DATA %>% select(-scale_risk_1, -raw_lower_slope_a)
    
    # DATA <- DATA %>% select(-raw_risk_slope_a, -price_to_50_week_MA_w_time_w_vol, -n, 
    #                           - volatility_roll, -week_ma, -price_to_50_week_MA, -price_to_50_week_MA_w_time)
    ##FINISHED: Store data 
    return(DATA)
    
  }

## risk metric for ALTCOINS 
ALT_risk_metric <- 
  function(DATA, DAY_MA, TIME_POWER, RISK_POWER, AVG_VOLATILITY_TIMEFRAME){
    
    DATA$week_ma_50 <- frollmean(DATA$price, n = DAY_MA * 7)
    #first row will have its own price as MA 
    DATA[1,]$week_ma_50 <- DATA[1,]$price
    
    max_n <- max(DATA[is.na(DATA$week_ma_50), ]$n)
    
    for(i in 2:max_n){
      DATA[i,]$week_ma_50 <- frollmean(DATA[1:(i),]$price, n = i)[i]
    }
    
    ##STEP 1: ratio of price to Week MA 
    DATA$price_to_50_week_MA <- with(DATA, price / week_ma_50)
    
    ##STEP 2: add time components 
    DATA$price_to_50_week_MA_w_time <- with(DATA, price_to_50_week_MA * (n^(1/TIME_POWER)))
    
    ##STEP 3: roling volatility and squishing raw risk metric 
        
    DATA$volatility_roll <- frollmean(DATA$volatility, AVG_VOLATILITY_TIMEFRAME)
    
    max_n <- max(DATA[is.na(DATA$volatility_roll), ]$n)
    
    for(i in 2:max_n){
      DATA[i,]$volatility_roll <- frollmean(DATA[1:(i),]$volatility, n = i)[i]
    }
    
    DATA$volatility_roll[1] <- 1 
    
    DATA$price_to_50_week_MA_w_time_w_vol <- 
      with(DATA, price_to_50_week_MA_w_time * volatility_roll)
    
    max_risk <- max(DATA[!is.na(DATA$price_to_50_week_MA_w_time_w_vol),
                               ]$price_to_50_week_MA_w_time_w_vol)
    
    min_risk <- min(DATA[!is.na(DATA$price_to_50_week_MA_w_time_w_vol),
                                     ]$price_to_50_week_MA_w_time_w_vol)
    
    DATA$scaled_risk <- 
      ((DATA$price_to_50_week_MA_w_time_w_vol - min_risk)/
         (max_risk - min_risk)) ^ (1/RISK_POWER)
    
    DATA <- DATA %>% select(-week_ma_50)
    # DATA <- DATA %>% select(-n, 
    #                           - volatility_roll, -week_ma, -price_to_50_week_MA, -price_to_50_week_MA_w_time)
    ##FINISHED: Store data 
    return(DATA)
    
  }

####################################################################################################################
# FRONT PAGE SUMMARY FUNCTIONS
####################################################################################################################

# value box functions 

price_value_box <- 
  function(DATA, cur_asset){
    
    rounding <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ 4, 
        cur_asset %in% btc_value_assets ~ 6,
        TRUE ~ 2
      )
    
    data_set <- DATA %>% filter(asset == cur_asset )
    
    main_val <- DATA[DATA$datetime == max(DATA$datetime) & 
                                DATA$asset == cur_asset, ]$price
    
    main_val <- 
        case_when(
          cur_asset %in% usd_lil_value_assets ~ format_dollar(main_val, rounding), 
          cur_asset %in% btc_value_assets ~ 
            as.character(round(main_val, rounding)),
          TRUE ~ format_dollar(main_val, rounding)
      )
    
    second_val <- 100*round(DATA[DATA$datetime == max(DATA$datetime) & 
                                   DATA$asset == cur_asset,]$volatility - 1,4)
    
    color_val <- ifelse(DATA[DATA$datetime == max(DATA$datetime) & 
                               DATA$asset == cur_asset,]$volatility > 1,  "green", "red")
    
    cons_vol <- data_set[data_set$datetime == max(data_set$datetime), ]$consequtive_days
    
    valueBox(
      tags$p(paste("Current Price: ",main_val), style = "font-size: 50%;"),
      
      subtitle = paste0("Daily Volatility: ", 
                        second_val, "%", "; ", 
                        "Same Volatility Days: ", cons_vol
                        ), 
      
      color = color_val
    )
  }

ma_value_box <- 
  function(DATA, cur_asset){
    
    rounding <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ 4, 
        cur_asset %in% btc_value_assets ~ 6,
        TRUE ~ 2
      )
    
    main_val <- DATA[DATA$datetime == max(DATA$datetime) & 
                       DATA$asset == cur_asset,]$week_ma_20
    
    main_val <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ format_dollar(main_val, rounding), 
        cur_asset %in% btc_value_assets ~ 
          as.character(round(main_val, rounding)),
        TRUE ~ format_dollar(main_val, rounding)
      )
    
    second_val <- 100*round(DATA[DATA$datetime == max(DATA$datetime) & 
                                   DATA$asset == cur_asset,]$price 
                                          /
                              DATA[DATA$datetime == max(DATA$datetime) & 
                                     DATA$asset == cur_asset,]$week_ma_20 
                            - 1,4)
    
    color_val <- ifelse(DATA[DATA$datetime == max(DATA$datetime) & 
                               DATA$asset == cur_asset,]$price / 
                          DATA[DATA$datetime == max(DATA$datetime) & 
                                 DATA$asset == cur_asset,]$week_ma_20> 1,  "green", "red")
    
    valueBox(
      
      tags$p(paste("20 Week Moving Average: ",main_val), style = "font-size: 40%;"),
      
      subtitle = tags$div(paste0("Price to 20 week MA % difference: ", 
                        second_val, "%"), 
                        br(), 
                        " " ), 
      
      color = color_val 
    )
  }

risk_value_box <- 
  function(DATA, cur_asset){
    
    
    main_val <- round(DATA[DATA$datetime == max(DATA$datetime) & 
                                     DATA$asset == cur_asset,]$scaled_risk, 4)
  
    color_val <- 
      with(DATA[DATA$datetime == max(DATA$datetime) & 
                  DATA$asset == cur_asset,],
           
           case_when(
             scaled_risk <.2 ~ "aqua", 
             scaled_risk >.2 & scaled_risk < .4 ~ "green", 
             scaled_risk >.4 & scaled_risk < .6 ~ "yellow", 
             scaled_risk >.6 & scaled_risk < .8 ~ "orange", 
             scaled_risk >.8 & scaled_risk <= 1 ~ "red"
           )
      )
    
    valueBox(
      
      tags$p(paste("Current risk: ",main_val), style = "font-size: 50%;"),
             
      subtitle = "Risk is estimated for current displayed price", 
      
      color = color_val)
  }

ath_value_box <- 
  function(DATA, cur_asset){
    
    rounding <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ 4, 
        cur_asset %in% btc_value_assets ~ 6,
        TRUE ~ 2
      )
    
    high_vals <- c(
      max(DATA[DATA$asset == cur_asset,]$high), 
      max(DATA[DATA$asset == cur_asset,]$price), 
      max(DATA[DATA$asset == cur_asset,]$open), 
      max(DATA[DATA$asset == cur_asset,]$low)
    )
    
    main_val <- max(high_vals)
    
    main_val <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ format_dollar(main_val, rounding), 
        cur_asset %in% btc_value_assets ~ 
          as.character(round(main_val, rounding)),
        TRUE ~ format_dollar(main_val, rounding)
      )
    
    second_val <- 100*round((DATA[DATA$datetime == max(DATA$datetime) & 
                                         DATA$asset == cur_asset,]$price) / 
                              max(DATA[DATA$asset == cur_asset,]$high)
                              -1,4)
    
    ath_date <- DATA[DATA$asset == cur_asset, ][max(DATA[DATA$asset == cur_asset, ]$high) == DATA[DATA$asset == cur_asset, ]$high, ]$datetime
    
    color_val <- "aqua"
    
    valueBox(
      tags$p(paste("All Time High: ",main_val), style = "font-size: 40%;"), 
      subtitle = tags$div(
        paste0("Established on: ", ath_date), 
        br(), 
        paste0("Current Price % Down from ATH : ", 
                        second_val, "%")
                        ),  
      
      color = color_val
    )
  }

######summary of prices in selected days 

range_min_price_value <- 
  function(DATA, cur_asset, min_date, max_date){
    
    df_local <- DATA %>% filter(datetime >= min_date & datetime <= max_date & asset == cur_asset)
    
    rounding <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ 4, 
        cur_asset %in% btc_value_assets ~ 6,
        TRUE ~ 2
      )
    
    main_val <- min(df_local$low)
    
    main_val <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ format_dollar(main_val, rounding), 
        cur_asset %in% btc_value_assets ~ 
          as.character(round(main_val, rounding)),
        TRUE ~ format_dollar(main_val, rounding)
      )
    
    color_val <- "teal"
    
    valueBox(
      tags$p("Min Price: ",br() ,main_val, style = "font-size: 40%;"), 
      subtitle = tags$div(" "),  
      color = color_val
    )
  }

range_max_price_value <- 
  function(DATA, cur_asset, min_date, max_date){
    
    df_local <- DATA %>% filter(datetime >= min_date & datetime <= max_date & asset == cur_asset)
    
    rounding <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ 4, 
        cur_asset %in% btc_value_assets ~ 6,
        TRUE ~ 2
      )
    
    main_val <- max(df_local$high)
    
    main_val <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ format_dollar(main_val, rounding), 
        cur_asset %in% btc_value_assets ~ 
          as.character(round(main_val, rounding)),
        TRUE ~ format_dollar(main_val, rounding)
      )
    
    color_val <- "teal"
    
    valueBox(
      tags$p("Max Price: ",br(), main_val, style = "font-size: 40%;"), 
      subtitle = tags$div(" "),  
      color = color_val
    )
  }

range_open_price <- 
  function(DATA, cur_asset, min_date, max_date){
    
    df_local <- DATA %>% filter(datetime >= min_date & datetime <= max_date & asset == cur_asset)
    
    #if open > close than this box is red 
    color_val <- 
      ifelse(
        df_local[df_local$datetime == min(df_local$datetime ), ]$price < 
          df_local[df_local$datetime == max(df_local$datetime ), ]$price, 
        "green", 
        "red"
      )
    
    rounding <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ 4, 
        cur_asset %in% btc_value_assets ~ 6,
        TRUE ~ 2
      )
    
    main_val <-  df_local[df_local$datetime == min(df_local$datetime ), ]$price
    
    main_val <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ format_dollar(main_val, rounding), 
        cur_asset %in% btc_value_assets ~ 
          as.character(round(main_val, rounding)),
        TRUE ~ format_dollar(main_val, rounding)
      )
    
    valueBox(
      tags$p("Open Price: ",br(), main_val, style = "font-size: 40%;"), 
      subtitle = tags$div(" "),  
      color = color_val
    )
  }

range_price_close <- 
  function(DATA, cur_asset, min_date, max_date){
    
    df_local <- DATA %>% filter(datetime >= min_date & datetime <= max_date & asset == cur_asset)
    
    rounding <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ 4, 
        cur_asset %in% btc_value_assets ~ 6,
        TRUE ~ 2
      )
    
    main_val <- df_local[df_local$datetime == max(df_local$datetime ), ]$price 
    
    main_val <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ format_dollar(main_val, rounding), 
        cur_asset %in% btc_value_assets ~ 
          as.character(round(main_val, rounding)),
        TRUE ~ format_dollar(main_val, rounding)
      )
    
    #if open > close than this box is red 
    color_val <- 
      ifelse(
        df_local[df_local$datetime == min(df_local$datetime ), ]$price < 
          df_local[df_local$datetime == max(df_local$datetime ), ]$price, 
        "green", 
        "red"
      )
    
    valueBox(
      tags$p("Close Price: ", br(),main_val, style = "font-size: 40%;"), 
      subtitle = tags$div(" "),  
      color = color_val
    )
  }

range_max_price_change <- 
  function(DATA, cur_asset, min_date, max_date){
    
    df_local <- DATA %>% filter(datetime >= min_date & datetime <= max_date & asset == cur_asset)
    
    rounding <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ 4, 
        cur_asset %in% btc_value_assets ~ 6,
        TRUE ~ 2
      )
    
    main_val <- df_local[df_local$datetime == max(df_local$datetime ), ]$price - 
                df_local[df_local$datetime == min(df_local$datetime ), ]$price
    
    main_val <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ format_dollar(main_val, rounding), 
        cur_asset %in% btc_value_assets ~ 
          as.character(round(main_val, rounding)),
        TRUE ~ format_dollar(main_val, rounding)
      )
    
    supp <- 
      paste0(
      round(
         df_local[df_local$datetime == max(df_local$datetime ), ]$price /
          df_local[df_local$datetime == min(df_local$datetime ), ]$price - 1, 4) * 100, "%")
    
    main_val <- paste0(main_val, " (", supp, ")")
        
    #if open > close than this box is red 
    color_val <- 
      ifelse(
        df_local[df_local$datetime == min(df_local$datetime ), ]$price < 
          df_local[df_local$datetime == max(df_local$datetime ), ]$price, 
        "green", 
        "red"
      )
    valueBox(
      tags$p("Price Change: ",br(), main_val, style = "font-size: 40%;"), 
      subtitle = tags$div(" "),  
      color = color_val
    )
  }

range_max_price_range <- 
  function(DATA, cur_asset, min_date, max_date){
    
    df_local <- DATA %>% filter(datetime >= min_date & datetime <= max_date & asset == cur_asset)
  
    rounding <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ 4, 
        cur_asset %in% btc_value_assets ~ 6,
        TRUE ~ 2
      )
    
    #if open > close than this box is red 
    color_val <- 
      ifelse(
        df_local[df_local$datetime == min(df_local$datetime ), ]$price < 
          df_local[df_local$datetime == max(df_local$datetime ), ]$price, 
        "green", 
        "red"
      )
    
    # if open > close then we have negative range, and if close > open we have a positive range 
    
    main_val <- 
      ifelse(
        df_local[df_local$datetime == min(df_local$datetime ), ]$price < 
          df_local[df_local$datetime == max(df_local$datetime ), ]$price, 
        
        max(df_local$high) - min(df_local$low) , 
        min(df_local$low) - max(df_local$high) 
      )
    
    main_val <- 
      case_when(
        cur_asset %in% usd_lil_value_assets ~ format_dollar(main_val, rounding), 
        cur_asset %in% btc_value_assets ~ 
          as.character(round(main_val, rounding)),
        TRUE ~ format_dollar(main_val, rounding)
      )
    
    
    valueBox(
      tags$p("Price Range: ", br(),main_val, style = "font-size: 40%;"), 
      subtitle = tags$div(" "),  
      color = color_val
    )
  }

######summary of volatility in selected days 

range_count_pos_days <- 
  function(DATA, cur_asset, min_date, max_date){
    
    df_local <- DATA %>% filter(datetime >= min_date & datetime <= max_date & asset == cur_asset)
    
    main_val <- prettyNum(nrow(df_local[df_local$volatility > 1, ]), big.mark = ",")
    
    color_val <- "green"
    
    valueBox(
      tags$p("Price Growth", "Days: ", main_val, style = "font-size: 40%;"), 
      subtitle = tags$div(" "),  
      color = color_val
    )
  }

range_count_avg_pos_vol <- 
  function(DATA, cur_asset, min_date, max_date){
    
    df_local <- DATA %>% filter(datetime >= min_date & datetime <= max_date & asset == cur_asset)
    
    main_val <- paste0(round(mean(df_local[df_local$volatility > 1, ]$volatility-1),4)*100, "%")
    
    color_val <- "green"
    
    valueBox(
      tags$p("Avg. Pos. Growth: ", main_val, style = "font-size: 40%;"), 
      subtitle = tags$div(" "),  
      color = color_val
    )
  }

range_count_neg_days <- 
  function(DATA, cur_asset, min_date, max_date){
    
    df_local <- DATA %>% filter(datetime >= min_date & datetime <= max_date & asset == cur_asset)
    
    main_val <- prettyNum(nrow(df_local[df_local$volatility < 1, ]), big.mark = ",")
    
    color_val <- "red"
    
    valueBox(
      tags$p("Price Drop", "Days: ", main_val, style = "font-size: 40%;"), 
      subtitle = tags$div(" "),  
      color = color_val
    )
  }

range_count_avg_neg_vol <- 
  function(DATA, cur_asset, min_date, max_date){
    
    df_local <- DATA %>% filter(datetime >= min_date & datetime <= max_date & asset == cur_asset)
    
    main_val <- paste0(round(mean(df_local[df_local$volatility < 1, ]$volatility-1),4)*100, "%")
    
    color_val <- "red"
    
    valueBox(
      tags$p("Avg. Neg. Drop: ", main_val, style = "font-size: 40%;"), 
      subtitle = tags$div(" "),  
      color = color_val
    )
  }

######## price dynamics selected days 

range_price_dynamics_plot <- 
  function(DATA, cur_asset, min_date, max_date){
    
    df_local <- DATA %>% filter(datetime >= min_date & datetime <= max_date & asset == cur_asset)
    
    rownames(df_local) <- NULL
    df_local$local_n <- as.numeric(rownames(df_local))
    
    local_price_lm <- lm(price ~ local_n, data= df_local)
   
    predict(local_price_lm, df_local, interval = "confidence")
    
    df_local$fit <- predict(local_price_lm, df_local, interval = "confidence")[,1]
    df_local$fit_lb <- predict(local_price_lm, df_local, interval = "confidence")[,2]
    df_local$fit_ub <- predict(local_price_lm, df_local, interval = "confidence")[,3]
    
   rounding <- 
    case_when(
      cur_asset %in% usd_lil_value_assets ~ 4, 
      cur_asset %in% btc_value_assets ~ 6,
      TRUE ~ 2
    )
   
    with(df_local, 
         
    plot_ly() %>% 
      
      add_trace(
        x = ~datetime,
        y = ~price,

        mode = "lines",
        type = "scatter",

        line = list(color = 'black'),
        opacity = opacity_level,

        name = "Close Price", 
        
        text = paste(
          "Date: ", datetime, 
          "<br>Price: ", 
                case_when(
                  cur_asset %in% usd_lil_value_assets ~ format_dollar(price, rounding), 
                  cur_asset %in% btc_value_assets ~ 
                    as.character(round(price, rounding)),
                  TRUE ~ format_dollar(price, rounding)
                ), 
          "<br>Risk: ", round(scaled_risk, 3)
        ), 
        
        hoverinfo = 'text'
      )  %>% 
      
      add_trace(
        x = ~datetime, 
        y = ~fit, 
        
        mode = "lines",
        type = "scatter",

        line = list(color = 'red'),
        opacity = opacity_level,

        name = "Linear Fit", 
        
        text = paste(
          "Date: ", datetime, 
          "<br>Linear Fit: ", 
                case_when(
                  cur_asset %in% usd_lil_value_assets ~ format_dollar(fit, rounding), 
                  cur_asset %in% btc_value_assets ~ 
                    as.character(round(fit, rounding)),
                  TRUE ~ format_dollar(fit, rounding)
                ) 
        ), 
        
        hoverinfo = 'text'
        
      ) %>% 
      
      add_trace(
        x = ~datetime, 
        y = ~fit_lb, 
        
        mode = "lines",
        type = "scatter",

        line = list(color = 'blue'),
        opacity = opacity_level,

        name = "Linear Fit LB", 
        
        text = paste(
          "Date: ", datetime, 
          "<br>Lower Fit Boundary: ", 
                case_when(
                  cur_asset %in% usd_lil_value_assets ~ format_dollar(fit_lb, rounding), 
                  cur_asset %in% btc_value_assets ~ 
                    as.character(round(fit_lb, rounding)),
                  TRUE ~ format_dollar(fit_lb, rounding)
                ) 
        ), 
        
        hoverinfo = 'text'
        
      ) %>%
      
      add_trace(
        x = ~datetime, 
        y = ~fit_ub, 
        
        mode = "lines",
        type = "scatter",

        line = list(color = 'blue'),
        opacity = opacity_level,

        name = "Linear Fit UB", 
        
        text = paste(
          "Date: ", datetime, 
          "<br>Upper Fit Boundary: ", 
                case_when(
                  cur_asset %in% usd_lil_value_assets ~ format_dollar(fit_ub, rounding), 
                  cur_asset %in% btc_value_assets ~ 
                    as.character(round(fit_ub, rounding)),
                  TRUE ~ format_dollar(fit_ub, rounding)
                ) 
        ), 
        
        hoverinfo = 'text'
        
      ) %>% 
      
      layout(
        yaxis = list(title = "Price and Fit"),
        xaxis = list(title = ""), 
        legend = legend_global, 
        margin = y_margins
      )
    # close 'with data' clause
    )
  }

price_dynamics_summary <- 
  function(DATA, cur_asset, min_date, max_date){
    
    df_local <- DATA %>% filter(datetime >= min_date & datetime <= max_date & asset == cur_asset)
    
    rownames(df_local) <- NULL
    df_local$local_n <- as.numeric(rownames(df_local))
    
    local_price_lm <- lm(price ~ local_n, data= df_local)
    local_price_lm_percent <- lm(log(price) ~ local_n, data= df_local)
    
    sum <- summary(local_price_lm)$coefficients
    sum_percent <- summary(local_price_lm_percent)$coefficients
    
    p_val <- round(sum[,4][2], 2)
    
    coeff <- as.numeric(round(sum[,1][2], 1))
    
    coeff_display <- paste0(
      coeff, " (",
            round(confint(local_price_lm)[2,],1)[1], ", ", 
            round(confint(local_price_lm)[2,],1)[2], ")"
    )
    
    coeff_p <- round(as.numeric(round(exp(sum_percent[,1][2]), 3)) - 1,3) * 100
    
    coeff_display_p <- paste0(
      coeff_p, " (",
            round(exp(confint(local_price_lm_percent)[2,])[1] - 1, 3) * 100, ", ", 
            round(exp(confint(local_price_lm_percent)[2,])[2] - 1, 3) * 100, ")"
    )
    
    trend_dir <- 
      case_when(
        coeff_p <= -1 ~ "Bearish",
        coeff_p >= -1 & coeff_p <= -0.5 ~ "Slow Bearish", 
        coeff_p >= -0.5 & coeff_p <= 0.5 ~ "Sideways", 
        coeff_p >= 0.5 & coeff_p <= 1 ~ "Slow Bllish", 
        TRUE ~ "Bullish"
      )
      
    trend_strength <- 
       case_when(
         p_val <= .05 ~ "Strong",
         p_val >= 0.05 & p_val <= 0.3 ~ "Weak", 
         TRUE ~ "No Trend"
       )
    
    trend_dir <- 
      case_when(
        trend_strength == "No Trend" ~ "Sideways", 
        TRUE ~ trend_dir
      )
    trend_strength <- paste0(trend_strength, " (", p_val, ")")
    
    metric_names <- c("Days used", "Avg. Change", "Avg. Approx. % Change", "Trend Direction", "Trend Strength")
    metrics <- c(as.character(max_date - min_date),
                 
                 coeff_display, 
                 
                 coeff_display_p, 
                 
                 trend_dir, 
                 
                 trend_strength
                 )
    
    data_for_presenting <- 
      data.frame(
        metric_names, 
        metrics
      )
    
    colnames(data_for_presenting) <- c("", "")
    
    data_for_presenting
    
  }
####################################################################################################################
# AGGREGATE INDICATORS
####################################################################################################################

risk_aggregate_box <- 
  function(DATA, CUR_ASSET){
    
    main_val <- round(DATA[DATA$datetime == max(DATA$datetime) & 
                                     DATA$asset == CUR_ASSET,]$scaled_risk, 4)
  
    color_val <- 
      case_when(
        main_val >= 0.85 & main_val <= 0.9 ~ 'orange', 
        main_val >= 0.9 ~ 'red', 
        TRUE ~ 'green'
      )
    
    valueBox(
      
      tags$p(paste("Current risk: ",main_val), style = "font-size: 40%;"),
             
      subtitle = paste("Critical Value: ", 0.9),
      
      color = color_val)
    
  }

slope_aggregate_box <- 
  function(DATA, CUR_ASSET){
    
    data <- DATA %>% filter(asset == CUR_ASSET)
 #   data <- estimate_slopes(data)
    
    main_val <- round(data[data$datetime == max(data$datetime) ,]$coeff_p, 4)
  
    q_85 <- quantile(data$coeff_p, 0.85, na.rm = T)
    q_90 <- quantile(data$coeff_p, 0.90, na.rm = T)
    
    color_val <- 
      case_when(
        main_val >= q_85 & main_val <= q_90 ~ 'orange', 
        main_val >= q_90 ~ 'red', 
        TRUE ~ 'green'
      )
    
    valueBox(
      
      tags$p(paste("Avg. 15 Day Change: ",main_val), style = "font-size: 40%;"),
              
      subtitle = paste("Critical Value: ", round(q_90,2)), 
      
      color = color_val)
  }

ma_ext_aggregate_box <- 
  function(DATA, CUR_ASSET){
    
    main_val <- round(DATA[DATA$datetime == max(DATA$datetime) & 
                                     DATA$asset == CUR_ASSET,]$price_ma_ratio, 4)
  
    q_85 <- quantile(DATA[DATA$asset == CUR_ASSET,]$price_ma_ratio, 0.85, na.rm = T)
    q_90 <- quantile(DATA[DATA$asset == CUR_ASSET,]$price_ma_ratio, 0.90, na.rm = T)
    
    color_val <- 
      case_when(
        main_val >= q_85 & main_val <= q_90 ~ 'orange', 
        main_val >= q_90 ~ 'red', 
        TRUE ~ 'green'
      )

    
    valueBox(
      
      tags$p(paste("MA Extention: ",main_val), style = "font-size: 40%;"),
             
      subtitle = paste("Critical Value: ", round(q_90,2)),
      
      color = color_val)
    
  }

ma_roc_aggregate_box <- 
  function(DATA, CUR_ASSET){
    
    main_val <- round(DATA[DATA$datetime == max(DATA$datetime) & 
                                     DATA$asset == CUR_ASSET,]$ma_roc, 4)
  
    q_85 <- quantile(DATA[DATA$asset == CUR_ASSET,]$ma_roc, 0.85, na.rm = T)
    q_90 <- quantile(DATA[DATA$asset == CUR_ASSET,]$ma_roc, 0.90, na.rm = T)
    
    color_val <- 
      case_when(
        main_val >= q_85 & main_val <= q_90 ~ 'orange', 
        main_val >= q_90 ~ 'red', 
        TRUE ~ 'green'
      )
    
    valueBox(
      
      tags$p(paste("MA Rate of Change: ",main_val), style = "font-size: 40%;"),
             
      subtitle = paste("Critical Value: ", round(q_90,2)),
      
      color = color_val)
    
  }

rsi_aggregate_box <- 
  function(DATA, CUR_ASSET){
    
    main_val <- round(DATA[DATA$datetime == max(DATA$datetime) & 
                                     DATA$asset == CUR_ASSET,]$rsi, 2)
  
    q_85 <- quantile(DATA[DATA$asset == CUR_ASSET,]$rsi, 0.85, na.rm = T)
    q_90 <- quantile(DATA[DATA$asset == CUR_ASSET,]$rsi, 0.90, na.rm = T)
    
   color_val <- 
      case_when(
        main_val >= q_85 & main_val <= q_90 ~ 'orange', 
        main_val >= q_90 ~ 'red', 
        TRUE ~ 'green'
      )
    
    valueBox(
      
      tags$p(paste("14 Day RSI: ",main_val), style = "font-size: 40%;"),
             
      subtitle = paste("Critical Value: ", round(q_90,2)),
      
      color = color_val)
    
  }

price_and_aggs <- 
  function(
    DATA, 
    CUR_ASSET){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)

    data_set$risk_flag <- 
      with(data_set, 
           case_when(
             scaled_risk <= 0.15 ~ -1, 
             scaled_risk >= 0.9 ~ 1, 
             TRUE ~ 0
           )
           )
    
    data_set$ma_ext_flag <- 
      with(data_set, 
           case_when(
             price_ma_ratio <= quantile(data_set$price_ma_ratio, na.rm = T, 0.1) ~ -1, 
             price_ma_ratio >= quantile(data_set$price_ma_ratio, na.rm = T, 0.9) ~ 1, 
             TRUE ~ 0
           )
           )
    
    data_set$ma_roc_flag <- 
      with(data_set, 
           case_when(
             ma_roc <= quantile(data_set$ma_roc, na.rm = T, 0.1) ~ -1, 
             ma_roc >= quantile(data_set$ma_roc, na.rm = T, 0.9) ~ 1, 
             TRUE ~ 0
           )
           )
 
    data_set$slope_flag <- 
      with(data_set, 
           case_when(
             coeff_p <= quantile(data_set$coeff_p, na.rm = T, 0.1) ~ -1, 
             coeff_p >= quantile(data_set$coeff_p, na.rm = T, 0.9) ~ 1, 
             TRUE ~ 0
           )
           )
    
    data_set$rsi_flag <- 
      with(data_set, 
           case_when(
             rsi <= quantile(data_set$rsi, na.rm = T, 0.1) ~ -1, 
             rsi >= quantile(data_set$rsi, na.rm = T, 0.9) ~ 1, 
             TRUE ~ 0
           )
           )
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    data_set$flag_count <- with(data_set, risk_flag + ma_ext_flag + ma_roc_flag + slope_flag + rsi_flag)
    
    with(data_set, 
     plot_ly(
      type = "scatter", 
      mode = 'markers+lines',
      
      x = ~datetime, 
      y = ~(price), 
      
      line = list(color = "lightgrey"), 
      marker = list(color = ~flag_count, 
                    size = 6),
      
      opacity = 0.75, 
      
      color = ~flag_count, 
      colors = gradient_color_palet,
      text = paste(
        "Date: ", datetime, 
        "<br> Price: ", 
            case_when(
              CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                format_dollar(price, 4), 
              TRUE ~ as.character(round(price, 6))
            ), 
        "<br> Risk: ", round(scaled_risk, 3), "Flag: ", risk_flag,
        "<br> MA Ratio: ", round(price_ma_ratio, 3), "Flag: ", ma_ext_flag,
        "<br> MA ROC: ", round(ma_roc, 3), "Flag: ", ma_roc_flag,
        "<br> Slope: ", round(coeff_p, 3), "Flag: ", slope_flag,
        "<br> RSI: ", round(rsi, 2), "Flag: ", rsi_flag, 
        "<br> Count: ", round(flag_count, 4)
      ), 
      hoverinfo = 'text'
      
    ) %>% layout(xaxis = list(range = c(a, b), title = "Time"), 
                 yaxis = list(type = "log", autotick = F, 
                              zeroline = FALSE, showline = FALSE), 
                 title = "Price on Log Base 10 Scale" 
    ) %>% 
      colorbar(len = 1, title = "Total Indicators")
    )
  }

price_and_aggs_lines <- 
  function(
    DATA, 
    CUR_ASSET
  ){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    data_set$risk_flag <- 
      with(data_set, 
           case_when(
             scaled_risk <= 0.15 ~ -1, 
             scaled_risk >= 0.9 ~ 1, 
             TRUE ~ 0
           )
           )
    
    data_set$ma_ext_flag <- 
      with(data_set, 
           case_when(
             price_ma_ratio <= quantile(data_set$price_ma_ratio, na.rm = T, 0.1) ~ -1, 
             price_ma_ratio >= quantile(data_set$price_ma_ratio, na.rm = T, 0.9) ~ 1, 
             TRUE ~ 0
           )
           )
    
    data_set$ma_roc_flag <- 
      with(data_set, 
           case_when(
             ma_roc <= quantile(data_set$ma_roc, na.rm = T, 0.1) ~ -1, 
             ma_roc >= quantile(data_set$ma_roc, na.rm = T, 0.9) ~ 1, 
             TRUE ~ 0
           )
           )
 
    data_set$slope_flag <- 
      with(data_set, 
           case_when(
             coeff_p <= quantile(data_set$coeff_p, na.rm = T, 0.1) ~ -1, 
             coeff_p >= quantile(data_set$coeff_p, na.rm = T, 0.9) ~ 1, 
             TRUE ~ 0
           )
           )
    
    data_set$rsi_flag <- 
      with(data_set, 
           case_when(
             rsi <= quantile(data_set$rsi, na.rm = T, 0.1) ~ -1, 
             rsi >= quantile(data_set$rsi, na.rm = T, 0.9) ~ 1, 
             TRUE ~ 0
           )
           )

    data_set$flag_count <- with(data_set, risk_flag + ma_ext_flag + ma_roc_flag + slope_flag + rsi_flag)
    
    with(data_set, 
         plot_ly() %>% 
           
           add_trace(
            x = ~datetime, 
            y = ~price, 
            
            type = "scatter", 
            mode = "lines", 
            
            line = list(color = color_1), 
            opacity = opacity_level, 
            
            text = paste(
              "Date: ", datetime, 
              "<br> Price: ", 
                  case_when(
                    CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                      format_dollar(price, 4), 
                    TRUE ~ as.character(round(price, 6))
                  ), 
              "<br> Risk: ", round(scaled_risk, 3), "Flag: ", risk_flag,
              "<br> MA Ratio: ", round(price_ma_ratio, 3), "Flag: ", ma_ext_flag,
              "<br> MA ROC: ", round(ma_roc, 3), "Flag: ", ma_roc_flag,
              "<br> Slope: ", round(coeff_p, 3), "Flag: ", slope_flag,
              "<br> RSI: ", round(rsi, 2), "Flag: ", rsi_flag, 
              "<br> Count: ", round(flag_count, 4)
            ), 
            hoverinfo = 'text', 
            name = 'Price'
            ) %>% 
           
           add_trace(
             x = datetime, 
             y = flag_count, 
             
             type = 'scatter', 
             mode = 'lines', 
             
             line = list(color = color_2), 
             opacity = opacity_level, 
             
             text = paste(
               "Date: ", datetime, 
               "<br>Flag Count: ", round(flag_count,2)
             ), 
             hoverinfo = 'text', 
             name = 'Flag Count', 
             yaxis = 'y2'
           ) %>% 
           
           layout(
             xaxis = list(limits = c(a,b), title = ''), 
             yaxis = list(type = 'log', autotick = F, title = 'Price'), 
             yaxis2 = list(overlaying = 'y', side = 'right', title = 'RSI'), 
             margin = y_margins, 
             legend = legend_global
           )
           )
         
  }
    
####################################################################################################################
# OBSERVATION FUNCTIONS 
####################################################################################################################

# historical price add 20 week MA 

historical_w_20_week <- 
  function(DATA, 
           CUR_ASSET, 
           Y_NAME
           ){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)
    
    min_p <- floor(min(log10(data_set$price)))
    max_p <- ceiling(max(log10(data_set$price)))
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    data_set %>% select(datetime, price, week_ma_20, price_ma_ratio)
    
    plot_ly() %>% 
        
      add_trace(
        type = "scatter", 
        mode = "lines", 
        
        x= ~data_set$datetime, 
        y= ~data_set$price, 
        
        opacity = opacity_level, 
        marker = list(size = 2, color = color_1), 
        line = list(width = 2, color = color_1), 
        
        name = "Price",
        
        text = paste('Date: ', data_set$datetime,
                   '<br>Price: ', 
                          case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(data_set$price, 4), 
                            TRUE ~ as.character(round(data_set$price, 6))
                          ),
                   '<br>Risk: ', round(data_set$scaled_risk,4)), 
        
      hoverinfo = 'text'
      ) %>% 
      
      add_trace(
        type = "scatter", 
        mode = "lines", 
        
        x= ~data_set$datetime, 
        y= ~data_set$week_ma_20, 
        
        opacity = opacity_level, 
        marker = list(size = 2, color = color_2), 
        line = list(width = 2, color = color_2), 
        
        name = "20 Week MA",
        
        text = paste('Date: ', data_set$datetime,
                   '<br>20 WMA: ', 
                          case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(data_set$week_ma_20, 4), 
                            TRUE ~ as.character(round(data_set$week_ma_20, 6))
                          )), 
        
      hoverinfo = 'text'
      ) %>% 
      
       add_trace(x = ~data_set$datetime, 
                y = ~data_set$price_ma_ratio, 
                opacity = opacity_level, 
                mode = "lines", 
                type = "scatter", 
                yaxis = "y2", 
                line = list(size = 2, color = color_3), 
                name = "Ratio to 20 Week MA ", 
                text = paste(
                  "Date: ",data_set$datetime, 
                  "<br> Ratio to 20 Week MA: ", round(data_set$price_ma_ratio,2)
                ), 
                hoverinfo = 'text'
                ) %>% 
      
      add_trace(
        x = ~data_set$datetime, 
        y = 1, 
        opacity = opacity_level, 
        mode = "lines", 
        type = "scatter", 
        yaxis = "y2", 
        line = list(size = 1.5, color = "black", dash = "dotted"), 
        text = '', 
        hoverinfo = 'text', 
        name = 'Price = MA level'
      ) %>% 
      
      layout(
        title = "Price on Log 10 Scale", 
        yaxis = list(title = Y_NAME, 
                     type = "log", autotick = F, 
                     zeroline = FALSE, showline = FALSE, 
                     range = c(min_p, max_p)), 
        
        yaxis2 =  list(type = "log", autotick = F, tickprefix=" ", 
                       title = 'Ratio of Price to MA', overlaying = "y", side = "right"), 
        
        xaxis = list(title = "", 
                     range = c(a,b)),
        
        legend = legend_global,
        
        margin = y_margins
        )
  }

historical_price_ma_ratio <- 
  function(DATA, 
           CUR_ASSET
           ){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)
    
    data_2 <- data_set %>% filter(!is.na(price_ma_ratio))
    
    min_r <- floor(min(data_2$price_ma_ratio)) - 1
    max_r <- ceiling(max(data_2$price_ma_ratio)) + 1
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    plot_ly(
      type = "scatter", 
      mode = "markers", 
      data = data_set,
      x = ~datetime, 
      y = ~price_ma_ratio, 
      color = ~scaled_risk, 
      colors = gradient_color_palet, 
      
      line = list(color = "lightgrey"), 
      marker = list(size = 6),
      
      text = paste(
        "Date: ", data_set$datetime, 
        "<br> Price: ", 
            case_when(
              CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                format_dollar(data_set$price, 4), 
              TRUE ~ as.character(round(data_set$price, 6))
            ), 
        "<br> Risk: ", round(data_set$scaled_risk, 4), 
        "<br> Ratio to MA: ", round(data_set$price_ma_ratio, 4)
      ), 
      hoverinfo = 'text'
    ) %>% 
      
      layout(
        xaxis = list(range = c(a,b),
                     title = "Date"),
        yaxis = list(zeroline = FALSE, 
                     #range = c(min_r, max_r), 
                     type = "log", 
                     #tickformat = "%", 
                     title = "Ratio of Price to 20 Week SMA", 
                     autotick = F
                     ),
        legend = legend_global,
        
        margin = y_margins
        
      ) %>% 
      colorbar(len = 1, orientation = 'h', title = "Risk", limits = c(0,1))
      
    
  }

historical_two_asset_w_20_week <- 
  function(DATA, 
           CUR_ASSET,
           ASSET_MA_1
           ){
    
    data_set <- DATA %>% filter(asset %in% c(ASSET_MA_1, CUR_ASSET))
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    plot_data_1 <- data_set %>% filter(asset == CUR_ASSET)
    plot_data_2 <- data_set %>% filter(asset == ASSET_MA_1)
    
    ### main plot 
    
    plot_ly() %>% 
  
      #price 1
      add_trace(
          x = ~plot_data_1$datetime, 
          y = ~plot_data_1$price, 
           
          opacity = opacity_level, 
          line = list(size = 2, color = color_1), 
          
          name = CUR_ASSET, 
          
          type = "scatter", 
          mode = "lines",
          text = paste('Date: ', plot_data_1$datetime,
                   '<br>Price: ', 
                          case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(plot_data_1$price, 4), 
                            TRUE ~ as.character(round(plot_data_1$price, 6))
                          ),
                   '<br>Risk: ', round(plot_data_1$scaled_risk,4)), 
          hoverinfo = 'text'
      ) %>% 
      
      #20 week ma 1
      add_trace(
          x = ~plot_data_1$datetime, 
          y = ~plot_data_1$week_ma_20, 
           
          name = paste(CUR_ASSET, "20 WMA"), 
          
          line = list(size = 2, color = color_2), 
          opacity = opacity_level, 
          type = "scatter", 
          mode = "lines",
          text = paste('Date: ', plot_data_1$datetime,
                   '<br>Price: ', 
                          case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(plot_data_1$week_ma_20, 4), 
                            TRUE ~ as.character(round(plot_data_1$week_ma_20, 6))
                          )
                   ), 
          hoverinfo = 'text'
      ) %>% 
      
      add_trace(
          x = ~plot_data_2$datetime, 
          y = ~plot_data_2$price, 
           
          line = list(size = 2, color = color_3), 
          opacity = opacity_level, 
          yaxis = 'y2',
          type = "scatter", 
          mode = "lines", 
          
          name = ASSET_MA_1,
          text = paste('Date: ', plot_data_2$datetime,
                   '<br>Price: ', 
                          case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(plot_data_2$price, 4), 
                            TRUE ~ as.character(round(plot_data_2$price, 6))
                          ),
                   '<br>Risk: ', round(plot_data_2$scaled_risk,4)), 
          hoverinfo = 'text'
      ) %>% 
      
      #20 week ma 1
      add_trace(
          x = ~plot_data_2$datetime, 
          y = ~plot_data_2$week_ma_20, 
           
          line = list(size = 2, color = color_4), 
          opacity = opacity_level, 
          yaxis = 'y2',
          type = "scatter", 
          mode = "lines", 
          
          name = paste(ASSET_MA_1, "20 WMA"),
          text = paste('Date: ', plot_data_2$datetime,
                   '<br>Price: ', 
                          case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(plot_data_2$week_ma_20, 4), 
                            TRUE ~ as.character(round(plot_data_2$week_ma_20, 6))
                          )), 
          hoverinfo = 'text'
      ) %>% 
      
      layout(
        xaxis = list(range = c(a,b), title = ""),
        yaxis = list(type = "log", autotick = F, title = CUR_ASSET),
        yaxis2 = list(type = "log", overlaying = 'y', side = 'right', autotick = F, title = ASSET_MA_1),
        
        legend = legend_global, 
        
        margin = y_margins
      )
    
  }

historical_two_asset_20_week_ext <- 
  function(DATA, 
           CUR_ASSET,
           ASSET_MA_1
  ){
    data_set <- DATA %>% filter(asset %in% c(ASSET_MA_1, CUR_ASSET))
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
   
    plot_data_1 <- data_set %>% filter(asset == CUR_ASSET)
    plot_data_2 <- data_set %>% filter(asset == ASSET_MA_1)
    
    ### main plot 
    
    plot_ly() %>% 
  
      #price 1
      add_trace(
          x = ~plot_data_1$datetime, 
          y = ~plot_data_1$price_ma_ratio, 
           
          opacity = opacity_level, 
          line = list(size = 2, color = color_1), 
          
          type = "scatter", 
          mode = "lines", 
          text = paste(
            "Date: ", plot_data_1$datetime, 
            "<br>Ratio :", round(plot_data_1$price_ma_ratio, 4)
          ), 
          hoverinfo = 'text', 
          
          name = paste(CUR_ASSET, "Extension")
      ) %>%
      
      add_trace(
          x = ~plot_data_2$datetime, 
          y = ~plot_data_2$price_ma_ratio, 
           
          opacity = opacity_level, 
          line = list(size = 2, color = color_2), 
          
          yaxis = 'y2',
          type = "scatter", 
          mode = "lines", 
          text = paste(
            "Date: ", plot_data_2$datetime, 
            "<br>Ratio :", round(plot_data_2$price_ma_ratio, 4)
          ), 
          hoverinfo = 'text', 
          
          name = paste(ASSET_MA_1, "Extension")
      ) %>% 
      
      layout(
        xaxis = list(range = c(a,b),
                     title = ""),
        yaxis = list(type = "log", title = CUR_ASSET, autotick = F),
        yaxis2 = list(type = "log", overlaying = 'y', side = 'right', title = ASSET_MA_1, autotick = F),
        
        margin = y_margins, 
        legend = legend_global
      )   
  }

historical_two_asset_w_20_week_by_days <- 
  function(DATA, 
           CUTOFF, 
           CUR_ASSET,
           ASSET_MA_1
  ){
    
    data_set <- DATA %>% filter(asset %in% c(ASSET_MA_1, CUR_ASSET))
    
    plot_data_1 <- data_set %>% filter(asset == CUR_ASSET & n <= CUTOFF)
    plot_data_2 <- data_set %>% filter(asset == ASSET_MA_1 & n <= CUTOFF)
    
    a <- min(min(plot_data_1$n), min(plot_data_2$n))
    b <- max(max(plot_data_1$n), max(plot_data_2$n))
    ### main plot 
    
    plot_ly() %>% 
  
      #price 1
      add_trace(
          x = ~plot_data_1$n, 
          y = ~plot_data_1$price, 
           
          opacity = opacity_level, 
          line = list(size = 2, color = color_1), 
          
          name = CUR_ASSET, 
          
          type = "scatter", 
          mode = "lines",
          text = paste('Day: ', plot_data_1$n,
                       "<br>Date: ", plot_data_1$datetime,
                   '<br>Price: ', 
                          case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(plot_data_1$price, 4), 
                            TRUE ~ as.character(round(plot_data_1$price, 6))
                          ),
                   '<br>Risk: ', round(plot_data_1$scaled_risk,4)), 
          hoverinfo = 'text'
      ) %>% 
      
      #20 week ma 1
      add_trace(
          x = ~plot_data_1$n, 
          y = ~plot_data_1$week_ma_20, 
           
          name = paste(CUR_ASSET, "20 WMA"), 
          
          line = list(size = 2, color = color_2), 
          opacity = opacity_level, 
          type = "scatter", 
          mode = "lines",
          text = paste('Day: ', plot_data_1$n,
                       "<br>Date: ", plot_data_1$datetime,
                   '<br>Price: ', 
                          case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(plot_data_1$week_ma_20, 4), 
                            TRUE ~ as.character(round(plot_data_1$week_ma_20, 6))
                          )
                   ), 
          hoverinfo = 'text'
      ) %>% 
      
      add_trace(
          x = ~plot_data_2$n, 
          y = ~plot_data_2$price, 
           
          line = list(size = 2, color = color_3), 
          opacity = opacity_level, 
          yaxis = 'y2',
          type = "scatter", 
          mode = "lines", 
          
          name = ASSET_MA_1,
          text = paste('Day: ', plot_data_2$n,
                       "<br>Date: ", plot_data_2$datetime,
                   '<br>Price: ', 
                          case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(plot_data_2$price, 4), 
                            TRUE ~ as.character(round(plot_data_2$price, 6))
                          ),
                   '<br>Risk: ', round(plot_data_2$scaled_risk,4)), 
          hoverinfo = 'text'
      ) %>% 
      
      #20 week ma 1
      add_trace(
          x = ~plot_data_2$n, 
          y = ~plot_data_2$week_ma_20, 
           
          line = list(size = 2, color = color_4), 
          opacity = opacity_level, 
          yaxis = 'y2',
          type = "scatter", 
          mode = "lines", 
          
          name = paste(ASSET_MA_1, "20 WMA"),
          text = paste('Day: ', plot_data_2$n,
                       "<br>Date: ", plot_data_2$datetime,
                   '<br>Price: ', 
                          case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(plot_data_2$week_ma_20, 4), 
                            TRUE ~ as.character(round(plot_data_2$week_ma_20, 6))
                          )), 
          hoverinfo = 'text'
      ) %>% 
      
      layout(
        xaxis = list(range = c(a,b), title = ""),
        yaxis = list(type = "log", autotick = F, title = CUR_ASSET),
        yaxis2 = list(type = "log", overlaying = 'y', side = 'right', autotick = F, title = ASSET_MA_1),
        
        legend = legend_global, 
        
        margin = y_margins
      )
    
  }

historical_two_asset_20_week_ext_by_days <- 
  function(DATA, 
           CUR_ASSET,
           ASSET_MA_1, 
           CUTOFF
  ){
    data_set <- DATA %>% filter(asset %in% c(ASSET_MA_1, CUR_ASSET))
    
    plot_data_1 <- data_set %>% filter(asset == CUR_ASSET & n <= CUTOFF)
    plot_data_2 <- data_set %>% filter(asset == ASSET_MA_1 & n <= CUTOFF)
    
    a <- min(min(plot_data_1$n), min(plot_data_2$n))
    b <- max(max(plot_data_1$n), max(plot_data_2$n))
    
    ### main plot 
    
    plot_ly() %>% 
  
      #price 1
      add_trace(
          x = ~plot_data_1$n, 
          y = ~plot_data_1$price_ma_ratio, 
           
          opacity = opacity_level, 
          line = list(size = 2, color = color_1), 
          
          type = "scatter", 
          mode = "lines", 
          text = paste(
            "Day: ", plot_data_1$n, 
            "<br>Date: ", plot_data_1$datetime,
            "<br>Ratio :", round(plot_data_1$price_ma_ratio, 4)
          ), 
          hoverinfo = 'text', 
          
          name = paste(CUR_ASSET, "Extension")
      ) %>%
      
      add_trace(
          x = ~plot_data_2$n, 
          y = ~plot_data_2$price_ma_ratio, 
           
          opacity = opacity_level, 
          line = list(size = 2, color = color_2), 
          
          yaxis = 'y2',
          type = "scatter", 
          mode = "lines", 
          text = paste(
            "Day: ", plot_data_2$n, 
            "<br>Date: ", plot_data_2$datetime, 
            "<br>Ratio :", round(plot_data_2$price_ma_ratio, 4)
          ), 
          hoverinfo = 'text', 
          
          name = paste(ASSET_MA_1, "Extension")
      ) %>% 
      
      layout(
        xaxis = list(range = c(a,b),
                     title = ""),
        yaxis = list(type = "log", title = CUR_ASSET, autotick = F),
        yaxis2 = list(type = "log", overlaying = 'y', side = 'right', title = ASSET_MA_1, autotick = F),
        
        margin = y_margins, 
        legend = legend_global
      )   
  }

max_days_ma_table <- 
  function(
    DATA , 
    CUTOFF , 
    CUR_ASSET ,
    ASSET_MA_1
  ){
    data_set <- DATA %>% filter(asset %in% c(ASSET_MA_1, CUR_ASSET))
    
    plot_data_1 <- data_set %>% filter(asset == CUR_ASSET)
    plot_data_2 <- data_set %>% filter(asset == ASSET_MA_1)
    
    sum_tab <- 
      data.frame(
        ass = c(CUR_ASSET, ASSET_MA_1), 
        n = c(
          prettyNum(nrow(plot_data_1), big.mark = ","),
          prettyNum(nrow(plot_data_2), big.mark = ","))
      )
    
    colnames(sum_tab) <- c("Asset", "Max Days")
    
    sum_tab
    
  }

historical_price_and_ath_and_20_week_ma <- 
  function(DATA, 
           CUR_ASSET){
    
    data_set <- DATA %>% filter(asset %in% c(CUR_ASSET))

    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
        
    with(data_set, 
     plot_ly() %>% 
          
      ## add BTC price 
      add_trace(x = ~datetime, 
                y = ~price, 
                opacity = opacity_level, 
                mode = "lines+markers", 
                type = "scatter", 
                marker = list(size = 1, color = "blue"),
                line = list(size = 4, color = "blue"), 
                text = paste(
                  "Date: ", datetime, 
                  "<br> Price: ", paste0("$", round(price)),
                  "<br>Risk: " , round(scaled_risk,4), 
                  "<br> ATH: ", new_ath_flag
                ), 
                hoverinfo = 'text', 
                
                name = "Asset Price"
                ) %>% 
      
      ## add 20 week ma
      add_trace(x = ~datetime, 
                y = ~week_ma_20, 
                opacity = opacity_level, 
                mode = "lines+markers", 
                type = "scatter", 
                marker = list(size = 1, color = "orange"),
                line = list(size = 4, color = "orange"), 
                text = paste(
                  "Date: ", datetime, 
                  "<br> Price: ", paste0("$", round(week_ma_20,4)),
                  "<br> ATH: ", new_ath_ma
                ), 
                hoverinfo = 'text', 
                
                name = "Asset 20 WMA"
                ) %>% 
      
      
      ## add ATHs of BTC 
      add_trace(x = ~data_set[data_set$new_ath_flag == 1, ]$datetime, 
                y = ~data_set[data_set$new_ath_flag == 1, ]$price, 
                mode = "markers", 
                type = "scatter", 
                opacity = opacity_level, 
                marker = list(size = 5, color = "black"),
                text = paste(
                  "Date: ", data_set[data_set$new_ath_flag == 1, ]$datetime, 
                  "<br> Price: ", paste0("$", round(data_set[data_set$new_ath_flag == 1, ]$price,4)),
                  "<br>Risk: " , round(data_set[data_set$new_ath_flag == 1, ]$scaled_risk,4), 
                  "<br> ATH: ", data_set[data_set$new_ath_flag == 1, ]$new_ath_flag
                ), 
                hoverinfo = 'text', 
                
                name = "Asset Price ATH"
                ) %>% 
   
      ## add ATHs of ETH
      add_trace(x = ~data_set[data_set$new_ath_ma == 1, ]$datetime, 
                y = ~data_set[data_set$new_ath_ma == 1, ]$week_ma_20, 
                mode = "markers", 
                type = "scatter", 
                opacity = opacity_level, 
                marker = list(size = 5, color = "red"),
                text = paste(
                  "Date: ", data_set[data_set$new_ath_ma == 1, ]$datetime, 
                  "<br> Price: ", paste0("$", round(data_set[data_set$new_ath_ma == 1, ]$week_ma_20,4)),
                  "<br>Risk: " , round(data_set[data_set$new_ath_ma == 1, ]$scaled_risk,4), 
                  "<br> ATH: ", data_set[data_set$new_ath_ma == 1, ]$new_ath_ma
                ), 
                hoverinfo = 'text', 
                
                name = "20 WMA ATH"
                ) %>% 
      
      layout(yaxis =  list(type = "log", autotick = F, title = "Price"), 
             
             xaxis = list(title = ""),
             
             legend = list(orientation = 'h',xanchor = "center", x = 0.5),
        
             margin = y_margins)
## close 'with' statemtn for data 
)
    
  }

historical_price_and_ma_rate_of_change <- 
  function(DATA,
           CUR_ASSET){
    
    data_set <- DATA %>% filter(asset %in% c(CUR_ASSET))

    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
     
    with(data_set, 
     plot_ly() %>% 
   
       add_trace(x = ~datetime, 
          y = ~price, 
          opacity = opacity_level, 
          mode = "lines+markers", 
          type = "scatter", 
          marker = list(size = 1, color = color_1),
          line = list(size = 4, color = color_1), 
          name = "BTC/USD", 
          text = paste(
            "Date: ",datetime, 
            "<br> Price: ", paste0("$", round(price,2)),
            "<br>Risk: " , round(scaled_risk, 4)
          ), 
          hoverinfo = 'text'
          ) %>% 
          ## add ETH price 
      add_trace(x = ~datetime, 
                y = ~ma_roc, 
                opacity = opacity_level, 
                mode = "lines+markers", 
                type = "scatter", 
                yaxis = "y2", 
                marker = list(size = 1, color = color_2),
                line = list(size = 4, color = color_2), 
                text = paste(
                  "Date: ", datetime, 
                  "<br>Rate of Change : ", round(ma_roc,4)
                ), 
                hoverinfo = 'text'
                ) %>% 
       
       add_trace(x = ~datetime, 
                 y = 1, 
                 opacity = opacity_level, 
                 mode = 'lines', 
                 type = 'scatter', 
                 yaxis = 'y2', 
                 line = list(size = 2, color = 'black'), 
                 text = '', 
                 hoverinfo = 'text', 
                 name = 'Rate of Change = 1') %>% 
      
   
      layout( yaxis =  list(type = "log", autotick = F, title = CUR_ASSET), 
             
              yaxis2 = list(autotick = F,  title = 'Rate of Change', overlaying = "y", side = "right" ), 
              
              xaxis = list(title = ""),
        
              margin = y_margins, 
             
              legend = legend_global)
## close 'with' clause for data 
 )
  }

historical_ma_roc <- 
  function(DATA, 
           CUR_ASSET
           ){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)
    
    data_2 <- data_set %>% filter(!is.na(ma_roc))
    
    min_r <- floor(min(data_2$ma_roc)) - 1
    max_r <- ceiling(max(data_2$ma_roc)) + 1
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    plot_ly(
      type = "scatter", 
      mode = "markers", 
      data = data_set,
      x = ~datetime, 
      y = ~ma_roc, 
      color = ~scaled_risk, 
      colors = gradient_color_palet, 
      
      line = list(color = "lightgrey"), 
      marker = list(size = 6),
      
      text = paste(
        "Date: ", data_set$datetime, 
        "<br> Price: ", 
            case_when(
              CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                format_dollar(data_set$price, 4), 
              TRUE ~ as.character(round(data_set$price, 6))
            ), 
        "<br> Risk: ", round(data_set$scaled_risk, 4), 
        "<br> MA ROC: ", round(data_set$ma_roc, 4)
      ), 
      hoverinfo = 'text'
    ) %>% 
      
      layout(
        xaxis = list(range = c(a,b),
                     title = "Date"),
        yaxis = list(zeroline = FALSE, 
                     #range = c(min_r, max_r), 
                     type = "log", 
                     #tickformat = "%", 
                     title = "20 WMA Rate of Change", 
                     autotick = F
                     ),
        legend = legend_global,
        
        margin = y_margins
        
      ) %>% 
      colorbar(len = 1, orientation = 'h', title = "Risk", limits = c(0,1))
      
    
  }

percent_down_from_ath <- 
  function(
    DATA, 
    CUR_ASSET
  ){
    data_set <- DATA %>% filter(asset == CUR_ASSET)
     
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000    
    
    plot_ly() %>% 
      
      add_trace(
        x = ~data_set$datetime, 
        y =~ data_set$p_down_from_ATH, 
        
        mode = "lines", 
        type = "scatter", 
      
        line = list(color = "darkgrey", opacity = opacity_level), 
        
        text = '', 
        
        hoverinfo = "text", 
        
        name = '% Down Over Time '
        
      ) %>%
      
      add_trace(
        x =~ data_set$datetime, 
        y =~ data_set$p_down_from_ATH, 
        
        mode = "markers", 
        type = "scatter", 
        
        color = data_set$market_stage, 
        colors = c("red", "green", "blue"), 
        marker = list(size = 4), 
        
        text = paste(
          "Date: ", data_set$datetime, 
          "<br> % Down : ", paste0(100*round(data_set$p_down_from_ATH, 4) ,"%"), 
          "<br> Recent Ath: ", 
              case_when(
                  CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                    format_dollar(data_set$recent_ath, 4), 
                  TRUE ~ as.character(round(data_set$recent_ath, 6))
                ),
         "<br> Price: ",  
              case_when(
                CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                  format_dollar(data_set$price, 4), 
                TRUE ~ as.character(round(data_set$price, 6))
              )
        ), 
        
        hoverinfo = "text") %>%
      
      layout(
        margin = y_margins, 
        xaxis = list(range = c(a,b),
                     title = "Date"),
        yaxis = list(zeroline = FALSE, 
                     range = c(-1, 0), 
                     tickformat = "%", 
                     title = "% Down from Most Recent ATH")
      )
    
  }

historical_price_and_ath_one <- 
  function(DATA, 
           CUR_ASSET){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)
     
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000    
    
    with(data_set, 
         plot_ly() %>% 
           
           add_trace(
             x = datetime, 
             y = price, 
             
             type = 'scatter', 
             mode = 'lines + markers', 
             
             marker = list(size = 1, color = color_1),
             line = list(size = 4, color = color_1), 
             opacity = opacity_level, 
             name = CUR_ASSET, 
             
             text = paste(
               "<br> % Down : ", paste0(100*round(p_down_from_ATH, 4) ,"%"), 
                "<br> Recent Ath: ", 
                    case_when(
                        CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                          format_dollar(recent_ath, 4), 
                        TRUE ~ as.character(round(recent_ath, 6))
                      ),
               "<br> Price: ",  
                    case_when(
                      CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                        format_dollar(price, 4), 
                      TRUE ~ as.character(round(price, 6))
                    )
             ), 
             hoverinfo = 'text'
           ) %>% 
           
          add_trace(x = ~data_set[data_set$new_ath_flag == 1, ]$datetime, 
                y = ~data_set[data_set$new_ath_flag == 1, ]$price, 
                mode = "markers", 
                type = "scatter", 
                opacity = opacity_level, 
                marker = list(size = 5, color = "black"),
                name = paste("ATHs of", CUR_ASSET), 
                text = paste(
                  "Date: ", data_set[data_set$new_ath_flag == 1, ]$datetime, 
                  "<br> Price: ", paste0("$", round(data_set[data_set$new_ath_flag == 1, ]$price,4)),
                  "<br>Risk: " , round(data_set[data_set$new_ath_flag == 1, ]$scaled_risk,4), 
                  "<br> ATH: ", data_set[data_set$new_ath_flag == 1, ]$new_ath_flag
                ), 
                hoverinfo = 'text'
                ) %>% 
      
         layout(yaxis =  list(type = "log", autotick = F, title = paste(CUR_ASSET, "Price")), 
             
             xaxis = list(title = ""),
        
              margin = y_margins, 
             
             legend = legend_global)
         
         )
  }

historical_price_and_ath <- 
  function(DATA, 
           ASSET_1, 
           ASSET_2){

    min_d_asset_1 <- min(DATA[DATA$asset == ASSET_1,]$datetime)
    min_d_asset_2 <- min(DATA[DATA$asset == ASSET_2,]$datetime)
    
    max_min_common_date <- max(min_d_asset_1, min_d_asset_2)
    
    two_asset_data <- 
        DATA %>% filter(asset %in% c(ASSET_1, ASSET_2) &
                                 datetime > max_min_common_date)
    
    plot_ly() %>% 
      
      ## add BTC price 
      add_trace(x = ~two_asset_data[two_asset_data$asset == ASSET_1, ]$datetime, 
                y = ~two_asset_data[two_asset_data$asset == ASSET_1, ]$price, 
                opacity = opacity_level, 
                mode = "lines+markers", 
                type = "scatter", 
                marker = list(size = 1, color = color_1),
                line = list(size = 4, color = color_1), 
                name = ASSET_1, 
                text = paste(
                  "Date: ", two_asset_data[two_asset_data$asset == ASSET_1, ]$datetime, 
                  "<br> Price: ", paste0("$", round(two_asset_data[two_asset_data$asset == ASSET_1, ]$price)),
                  "<br>Risk: " , round(two_asset_data[two_asset_data$asset == ASSET_1, ]$scaled_risk,4), 
                  "<br> ATH: ", two_asset_data[two_asset_data$asset == ASSET_1, ]$new_ath_flag
                ), 
                hoverinfo = 'text'
                ) %>% 
      
      ## add ETH price 
      add_trace(x = ~two_asset_data[two_asset_data$asset == ASSET_2, ]$datetime, 
                y = ~two_asset_data[two_asset_data$asset == ASSET_2, ]$price, 
                opacity = opacity_level, 
                yaxis = "y2", 
                mode = "lines+markers", 
                type = "scatter", 
                marker = list(size = 1, color = color_2),
                line = list(size = 4, color = color_2), 
                name = ASSET_2, 
                text = paste(
                  "Date: ", two_asset_data[two_asset_data$asset == ASSET_2, ]$datetime, 
                  "<br> Price: ", paste0("$", round(two_asset_data[two_asset_data$asset == ASSET_2, ]$price,4)),
                  "<br>Risk: " , round(two_asset_data[two_asset_data$asset == ASSET_2, ]$scaled_risk,4), 
                  "<br> ATH: ", two_asset_data[two_asset_data$asset == ASSET_2, ]$new_ath_flag
                ), 
                hoverinfo = 'text'
                ) %>% 
      
      ## add ATHs of BTC 
      add_trace(x = ~two_asset_data[two_asset_data$asset == ASSET_1 & two_asset_data$new_ath_flag == 1, ]$datetime, 
                y = ~two_asset_data[two_asset_data$asset == ASSET_1 & two_asset_data$new_ath_flag == 1, ]$price, 
                mode = "markers", 
                type = "scatter", 
                opacity = opacity_level, 
                marker = list(size = 5, color = "black"),
                name = paste("ATHs of", ASSET_1), 
                text = paste(
                  "Date: ", two_asset_data[two_asset_data$asset == ASSET_1
                                           & two_asset_data$new_ath_flag == 1, ]$datetime, 
                  "<br> Price: ", paste0("$", round(two_asset_data[two_asset_data$asset == ASSET_1
                                                                   & two_asset_data$new_ath_flag == 1, ]$price,4)),
                  "<br>Risk: " , round(two_asset_data[two_asset_data$asset == ASSET_1
                                                      & two_asset_data$new_ath_flag == 1, ]$scaled_risk,4), 
                  "<br> ATH: ", two_asset_data[two_asset_data$asset == ASSET_1
                                               & two_asset_data$new_ath_flag == 1, ]$new_ath_flag
                ), 
                hoverinfo = 'text'
                ) %>% 
      
      ## add ATHs of ETH
      add_trace(x = ~two_asset_data[two_asset_data$asset == ASSET_2 & two_asset_data$new_ath_flag == 1, ]$datetime, 
                y = ~two_asset_data[two_asset_data$asset == ASSET_2 & two_asset_data$new_ath_flag == 1, ]$price, 
                mode = "markers", 
                type = "scatter", 
                yaxis = "y2", 
                opacity = opacity_level, 
                marker = list(size = 5, color = "red"),
                name = paste("ATHs of", ASSET_2), 
                text = paste(
                  "Date: ", two_asset_data[two_asset_data$asset == ASSET_2
                                           & two_asset_data$new_ath_flag == 1, ]$datetime, 
                  "<br> Price: ", paste0("$", round(two_asset_data[two_asset_data$asset == ASSET_2
                                                                   & two_asset_data$new_ath_flag == 1, ]$price,4)),
                  "<br>Risk: " , round(two_asset_data[two_asset_data$asset == ASSET_2
                                                      & two_asset_data$new_ath_flag == 1, ]$scaled_risk,4), 
                  "<br> ATH: ", two_asset_data[two_asset_data$asset == ASSET_2
                                               & two_asset_data$new_ath_flag == 1, ]$new_ath_flag
                ), 
                hoverinfo = 'text'
                ) %>% 
      
      layout(yaxis =  list(type = "log", autotick = F, title = paste(ASSET_1, "Price")), 
             
             yaxis2 = list(type = "log", autotick = F, overlaying = "y", side = "right", title = paste(ASSET_2, "Price") ), 
             
             xaxis = list(title = ""),
        
              margin = y_margins, 
             
             legend = legend_global)

  }

just_ath_timeline <- 
  function(DATA, 
           ASSET_1, 
           ASSET_2){
    
    min_d_asset_1 <- min(master_data[master_data$asset == ASSET_1,]$datetime)
    min_d_asset_2 <- min(master_data[master_data$asset == ASSET_2,]$datetime)
    
    max_min_common_date <- max(min_d_asset_1, min_d_asset_2)
    
    two_asset_aths <- 
      master_data %>% filter(asset %in% c(ASSET_1, ASSET_2) & 
                               new_ath_flag == 1 & 
                               datetime > max_min_common_date)
    
    two_asset_aths[two_asset_aths$asset == ASSET_1, ]$new_ath_flag <- 
      two_asset_aths[two_asset_aths$asset == ASSET_1, ]$new_ath_flag + 
      0.0005
    
    plot_ly(
      data = two_asset_aths, 
      
      type = "scatter", 
      mode = "markers", 
      
      x = ~datetime, 
      y = ~new_ath_flag, 
      
      opacity = opacity_level, 
      color = ~asset, 
      
      text = paste(
        "Asset: ", two_asset_aths$asset, 
        "<br>ATH Date: ", two_asset_aths$datetime, 
        "<br>Risk: ", round(two_asset_aths$scaled_risk,4), 
        "<br>ATH Price: ", format_dollar(two_asset_aths$price)
      ), 
      
      hoverinfo = 'text'
    ) %>% 
      layout(
        margin = y_margins, 
        yaxis = list(range=c(0.9,1.1), 
                     showgrid = FALSE,
                     showticklabels = FALSE),showlegend=FALSE
      )

  }

consequtive_days_plot <- 
  function(DATA, 
           CUR_ASSET,
           CUR_CUTOFF, 
           SIZE_DOTS,
           OPACITY_LEVEL
           ){

      data_set <- DATA %>% filter(asset == CUR_ASSET & datetime >= as.Date('2011-04-20'))
   
      sequences <- unique(data_set[data_set$consequtive_days > CUR_CUTOFF, ]$seq_id)
      
      data_set$display_seq <- data_set$consequtive_days
      
      data_set[!(data_set$seq_id %in% sequences), ]$display_seq <- NA
      
      data_set[data_set$seq_id %in% sequences, ]$consequtive_days <- NA
      
      data_set$price_small_cons <- data_set$price
      
      data_set[data_set$seq_id %in% sequences, ]$price_small_cons <- NA
      
      data_set$price_big_cons <- data_set$price
      
      data_set[!(data_set$seq_id %in% sequences),]$price_big_cons <- NA
      
      data_set_rollup <- 
        data_set %>% group_by(seq_id) %>% 
        summarize(max_len = max(display_seq))
      
      data_set <- merge(data_set, data_set_rollup, by = "seq_id")
      
      a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
      b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
      
      plot_ly() %>% 
        
        add_trace(
          x = ~data_set$datetime, 
          y =~ data_set$price, 
          
          mode = "lines", 
          type = "scatter", 
          
          colors =  gradient_color_palet, 
          
          opacity = opacity_level, 
          marker = list(size = 3, color = "grey"), 
          line = list(color = "grey"), 
          name = 'Non- highlighted daily price',
          text = paste(
          "Date: ", data_set$datetime, 
          "<br> Price: ",  
            case_when(
              CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                format_dollar(data_set$price, 4), 
              TRUE ~ as.character(round(data_set$price, 6))
            ),
          
#          paste0(prettyNum(round(data_set$price), big.mark = ","), "$"), 
          "<br> Risk: ", round(data_set$scaled_risk,3),
          "<br> Volatility: ", paste0(round(data_set$transformed_volatility,4) * 100, "%"),
          "<br> Type: ", data_set$volatility_type,
          "<br> Cons. days: ", data_set$display_seq,
          "<br> Seq. id: ", data_set$seq_id
        ), 
          
        hoverinfo = 'text'
        ) %>% 
      
      add_trace(
        x = ~data_set$datetime, 
        y = ~data_set$price_big_cons, 
        
        mode = "markers", 
        type = "scatter", 
        
        opacity = OPACITY_LEVEL, 
        
        color = ~data_set$max_len, 
        marker = list(size = SIZE_DOTS), 
        colors =  gradient_color_palet, 
        
        text = paste(
          "Date: ", data_set$datetime, 
          "<br> Price: ",  
            case_when(
              CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                format_dollar(data_set$price, 4), 
              TRUE ~ as.character(round(data_set$price, 6))
            ),
          
#          paste0(prettyNum(round(data_set$price), big.mark = ","), "$"), 
          "<br> Risk: ", round(data_set$scaled_risk,3),
          "<br> Volatility: ", paste0(round(data_set$transformed_volatility,4) * 100, "%"),
          "<br> Type: ", data_set$volatility_type,
          "<br> Cons. days: ", data_set$display_seq,
          "<br> Seq. id: ", data_set$seq_id
        ), 
        
        hoverinfo = "text",
        name = 'Highlighted daily price'
      ) %>% 
        
        layout(yaxis = list(type = "log", autotick = F, title = CUR_ASSET),
                xaxis = list(title = "Date", 
                         range = c(a,b)),
        
                margin = y_margins) %>% 
        
        colorbar( title = "Consequtive Days")
  }

consequtive_days_plot2 <- 
  function(DATA, CUR_ASSET){
    
  data_set <- DATA %>% filter(asset == CUR_ASSET & datetime >= as.Date('2011-04-20'))
  
  a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
  b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
  
  with(data_set,
       plot_ly() %>% 
         
         add_trace(
          x = ~datetime, 
          y = ~price, 
          
          type = "scatter", 
          mode = "lines", 
          
          line = list(color = color_1), 
          opacity = opacity_level, 
          
          name = 'Price', 
          text = paste(
            "Date: ", datetime, 
            "<br> Price: ",  
              case_when(
                CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                  format_dollar(price, 4), 
                TRUE ~ as.character(round(price, 6))
              ),
            "<br> Volatility: ", paste0(round(data_set$transformed_volatility,4) * 100, "%"),
            "<br> Type: ", volatility_type,
            "<br> Cons. days: ", consequtive_days
          ), 
          hoverinfo = 'text'
         ) %>% 
         
         add_trace(
           yaxis = 'y2', 
           type = 'scatter', 
           mode = 'lines', 
           
           x = ~datetime, 
           y = ~consequtive_days, 
           
           line = list(color = color_2), 
           opacity = opacity_level,
           
           text = paste(
             "Date: ", datetime, 
             "<br>Cons. Days: ", consequtive_days
           ), 
           
           hoverinfo = 'text', 
           
           name = 'Consequive Days'
         ) %>% 
         
         add_trace(
           x = ~datetime, 
           y = quantile(consequtive_days, 0.9, na.rm = T), 
           
           type = 'scatter', 
           mode = 'lines', 
           yaxis = 'y2', 
           
           opacity = opacity_level, 
           line = list(color = "black", dashed = 'dots'), 
           text = '', 
           hoverinfo = 'text', 
           
           name = '90th percentile of cons. days'
         ) %>% 
         
         layout(xaxis = list(title = "", limits = c(a,b)), 
                yaxis = list(autotick = F, title = "Price", type = 'log'), 
                yaxis2 = list(overlaying = 'y', side = 'right', autotick = F), 
                margin = y_margins, 
                legend = legend_global)
       )
  
  }

consequtive_days_table <- 
  function(DATA, 
           CUR_ASSET){
    
      data_set <- DATA %>% filter(asset == CUR_ASSET & datetime >= as.Date('2011-04-20'))
      
      ## define transformed volatility: basically in terms of % change 
      data_set$transformed_volatility <- data_set$volatility - 1
      
      data_set$volatility_type <- 
        case_when(
          sign(data_set$transformed_volatility) == 1 ~ "Positive volatility", 
          sign(data_set$transformed_volatility) == 0 ~ "Zero volatility",
          TRUE ~ "Negative volatility"
        )
      
      ### consequtive days: consequtive day number that volatity is the same direction: positive or negative
      # basically would like to see how many days in a row can volatility go up and down 
      
      data_set$consequtive_days <- 1
      
      #############
      # NOTE: 
      # "sign" is a bad function here because it doesn't work with 0, so it needs to be fixed. 
      
      for(i in 2:nrow(data_set)){
      
        # if volatility is positive or negative both today and yesterday add one day to consequtive streak, 
          # othewise set streak to 1 
        data_set$consequtive_days[i] <- 
          ifelse(
            sign(data_set$transformed_volatility[i]) ==  
              sign(data_set$transformed_volatility[i-1]) , data_set$consequtive_days[i-1] + 1, 1
          )
        }
      
      data_set$seq_id <- 1
      
      for(i in 2:nrow(data_set)){
        
        ## if previous day volatility is the same sign, i.e. also positive, negative, or zero
        # then keep the same sequence id
        # otherwise add 1 
        
        data_set$seq_id[i] <- 
          ifelse(
            sign(data_set$transformed_volatility[i]) ==  
                sign(data_set$transformed_volatility[i-1]) ,
            
            data_set$seq_id[i-1],
            data_set$seq_id[i-1] + 1
          )
      }
      
      summary_data <- 
        data_set %>% 
        group_by(seq_id) %>% 
        summarise(
          vol_type = unique(volatility_type), 
          max_l = max(consequtive_days)
          )
      
      summary_data_roll_up <- 
        summary_data %>% 
        group_by(max_l ) %>% 
        
        summarize(
          n = n(), 
          p_total = n()/nrow(summary_data), 
          
          p_pos_loc = length(which(vol_type == "Positive volatility"))/n(),
          p_neg_loc = length(which(vol_type == "Negative volatility"))/n(),
          p_no_loc  = length(which(vol_type == "Zero volatility"))/n(), 
          
          p_pos_glob = length(which(vol_type == "Positive volatility"))/nrow(summary_data),
          p_neg_glob = length(which(vol_type == "Negative volatility"))/nrow(summary_data),
          p_no_glob  = length(which(vol_type == "Zero volatility"))/nrow(summary_data)
        )
      
      for(i in 3:length(summary_data_roll_up)){
        summary_data_roll_up[,i] <- 100*round(summary_data_roll_up[,i],4)
      }

      summary_data_roll_up$p_total <- paste0(summary_data_roll_up$p_total, "%")
        
      summary_data_roll_up$p_pos_loc <- paste0(summary_data_roll_up$p_pos_loc, "%")
      summary_data_roll_up$p_neg_loc <- paste0(summary_data_roll_up$p_neg_loc, "%")
      summary_data_roll_up$p_no_loc  <- paste0(summary_data_roll_up$p_no_loc, "%")
        
      summary_data_roll_up$p_pos_glob <- paste0(summary_data_roll_up$p_pos_glob, "%")
      summary_data_roll_up$p_neg_glob <- paste0(summary_data_roll_up$p_neg_glob, "%")
      summary_data_roll_up$p_no_glob  <- paste0(summary_data_roll_up$p_no_glob, "%")
        
      colnames(summary_data_roll_up) <- 
        c("Cons. Days",
             "Number",
             "% of Total",
             "% Positive",
             "% Negative",
             "% Zero",
             "% Positive",
             "% Negative",
             "% Zero")
      ### OUTPUT TABLE 

      sketch = 
        htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(colspan = 2, 'Sepal'),
              th(colspan = 2, 'Petal')
            )
          )
          )
        )
      
      datatable(summary_data_roll_up, rownames = F) %>%
        formatStyle(c(3,6), `border-right` = "dashed 2px", align = 'c')

  }

consequtive_days_sum_plot <- 
  function(DATA, 
           CUR_ASSET){
    
      data_set <- DATA %>% filter(asset == CUR_ASSET & datetime >= as.Date('2011-04-20'))
      
      ## define transformed volatility: basically in terms of % change 
      data_set$transformed_volatility <- data_set$volatility - 1
      
      data_set$volatility_type <- 
        case_when(
          sign(data_set$transformed_volatility) == 1 ~ "Positive volatility", 
          sign(data_set$transformed_volatility) == 0 ~ "Zero volatility",
          TRUE ~ "Negative volatility"
        )
      
      ### consequtive days: consequtive day number that volatity is the same direction: positive or negative
      # basically would like to see how many days in a row can volatility go up and down 
      
      data_set$consequtive_days <- 1
      
      #############
      # NOTE: 
      # "sign" is a bad function here because it doesn't work with 0, so it needs to be fixed. 
      
      for(i in 2:nrow(data_set)){
      
        # if volatility is positive or negative both today and yesterday add one day to consequtive streak, 
          # othewise set streak to 1 
        data_set$consequtive_days[i] <- 
          ifelse(
            sign(data_set$transformed_volatility[i]) ==  
              sign(data_set$transformed_volatility[i-1]) , data_set$consequtive_days[i-1] + 1, 1
          )
        }
      
      data_set$seq_id <- 1
      
      for(i in 2:nrow(data_set)){
        
        ## if previous day volatility is the same sign, i.e. also positive, negative, or zero
        # then keep the same sequence id
        # otherwise add 1 
        
        data_set$seq_id[i] <- 
          ifelse(
            sign(data_set$transformed_volatility[i]) ==  
                sign(data_set$transformed_volatility[i-1]) ,
            
            data_set$seq_id[i-1],
            data_set$seq_id[i-1] + 1
          )
      }
      
      summary_data <- 
        data_set %>% 
        group_by(seq_id) %>% 
        summarise(
          vol_type = unique(volatility_type), 
          max_l = max(consequtive_days)
          )
      
      summary_data_plot <- 
        summary_data %>% 
        
        group_by(max_l, vol_type) %>% 
        
        summarise(n = n())
  
      glob_count <- 
        summary_data %>% 
        
        group_by(max_l) %>% 
        
        summarise(n_tot_loc = n())
      
      summary_data_plot <- 
        merge(summary_data_plot, glob_count, by = "max_l")
      
      summary_data_plot$n_tot_glob <- length(unique(summary_data$seq_id))
      
      summary_data_plot$p_loc <- with(summary_data_plot, n/n_tot_loc)
      
      summary_data_plot$p_glob <- with(summary_data_plot, n/n_tot_glob)
      
      plot_ly(
        type = "scatter", 
        mode = "lines+markers", 
        
        data = summary_data_plot, 
        x = ~max_l, 
        y = ~p_loc, 
        
        color = ~vol_type, 
        size = ~n, 
        
        text = paste(
          "Cons. Days: ", summary_data_plot$max_l, 
          "<br>N: ", summary_data_plot$n, 
          "<br>% of Same Cons. Days: ", paste0(round(summary_data_plot$p_loc, 4) * 100, "%")
        ), 
        
        hoverinfo = 'text'
      ) %>% 
        
        layout(
          margin = y_margins, 
          xaxis = list(tick0 = 1, dtick = 1, tickmode = 'linear', title = 'Sequence Length'), 
          yaxis = list(tick0 = 0, dtick = 0.1, tickmode = 'linear', tickformat = "%", title = '% of Total Within Same Length')
        )
  }

consequtive_days_sum_plot_2 <- 
  function(DATA, 
           CUR_ASSET){
    
      data_set <- DATA %>% filter(asset == CUR_ASSET & datetime >= as.Date('2011-04-20'))
      
      ## define transformed volatility: basically in terms of % change 
      data_set$transformed_volatility <- data_set$volatility - 1
      
      data_set$volatility_type <- 
        case_when(
          sign(data_set$transformed_volatility) == 1 ~ "Positive volatility", 
          sign(data_set$transformed_volatility) == 0 ~ "Zero volatility",
          TRUE ~ "Negative volatility"
        )
      
      ### consequtive days: consequtive day number that volatity is the same direction: positive or negative
      # basically would like to see how many days in a row can volatility go up and down 
      
      data_set$consequtive_days <- 1
      
      #############
      # NOTE: 
      # "sign" is a bad function here because it doesn't work with 0, so it needs to be fixed. 
      
      for(i in 2:nrow(data_set)){
      
        # if volatility is positive or negative both today and yesterday add one day to consequtive streak, 
          # othewise set streak to 1 
        data_set$consequtive_days[i] <- 
          ifelse(
            sign(data_set$transformed_volatility[i]) ==  
              sign(data_set$transformed_volatility[i-1]) , data_set$consequtive_days[i-1] + 1, 1
          )
        }
      
      data_set$seq_id <- 1
      
      for(i in 2:nrow(data_set)){
        
        ## if previous day volatility is the same sign, i.e. also positive, negative, or zero
        # then keep the same sequence id
        # otherwise add 1 
        
        data_set$seq_id[i] <- 
          ifelse(
            sign(data_set$transformed_volatility[i]) ==  
                sign(data_set$transformed_volatility[i-1]) ,
            
            data_set$seq_id[i-1],
            data_set$seq_id[i-1] + 1
          )
      }
      
      summary_data <- 
        data_set %>% 
        group_by(seq_id) %>% 
        summarise(
          vol_type = unique(volatility_type), 
          max_l = max(consequtive_days)
          )
      
      summary_data_plot <- 
        summary_data %>% 
        
        group_by(max_l, vol_type) %>% 
        
        summarise(n = n())
  
      glob_count <- 
        summary_data %>% 
        
        group_by(max_l) %>% 
        
        summarise(n_tot_loc = n())
      
      summary_data_plot <- 
        merge(summary_data_plot, glob_count, by = "max_l")
      
      summary_data_plot$n_tot_glob <- length(unique(summary_data$seq_id))
      
      summary_data_plot$p_loc <- with(summary_data_plot, n/n_tot_loc)
      
      summary_data_plot$p_glob <- with(summary_data_plot, n/n_tot_glob)
      
      plot_ly(
        type = "scatter", 
        mode = "lines+markers", 
        
        data = summary_data_plot, 
        x = ~max_l, 
        y = ~p_glob, 
        
        color = ~vol_type, 
        size = ~n, 
        
        text = paste(
          "Cons. Days: ", summary_data_plot$max_l, 
          "<br>N: ", summary_data_plot$n, 
          "<br>% of Same Cons. Days: ", paste0(round(summary_data_plot$p_glob, 4) * 100, "%")
        ), 
        
        hoverinfo = 'text'
      ) %>% 
        
        layout(
          margin = y_margins, 
          xaxis = list(tick0 = 1, dtick = 1, tickmode = 'linear', title = 'Sequence Length'), 
          yaxis = list(tick0 = 0, dtick = 0.1, tickmode = 'linear', 
                       tickformat = "%", title = '% of Total Sequences')
        )
  }

historical_log_slopes_w_price <- 
  function(DATA, 
           CUR_ASSET, 
           DAYS){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
        
    with(data_set, 
         
      plot_ly() %>% 
        add_trace(
          x = ~datetime, 
          y = ~price, 
          
          type = "scatter", 
          mode = 'lines',
          opacity = opacity_level, 
          line = list(color = color_1), 
          
          name = 'Price', 
          
          text = paste(
            "Date: ", datetime, 
            "<br>Price: ", format_dollar(price), 
            "<br>Risk: ", round(scaled_risk, 3), 
            "<br>Coef %: ", round(coeff_p, 3)
          ), 
          hoverinfo = 'text'
        ) %>% 
       
       add_trace(
         x = ~datetime, 
         y = ~coeff_p, 
         
         opacity = opacity_level, 
         type = "scatter", 
         mode = "lines", 
         
         name = "Slopes (Coef %)", 
         yaxis = 'y2',
         opacity = opacity_level, 
         line = list(color = color_2), 
         text = paste(
           "Date: ", datetime, 
           "<br>Coef %: ", round(coeff_p, 3)
         ), 
         hoverinfo = 'text'
       ) %>% 
        
      add_trace(
        x = ~datetime, 
        y = ~quantile(coeff_p, 0.9, na.rm = T), 
        type = "scatter", 
         mode = "lines", 
        yaxis = 'y2', 
        opacity = opacity_level, 
        line = list(color = "black", dashed = "dots"), 
        name = "90th Quantile of Slopes", 
        text = '',
        hoverinfo = 'text'
        
      ) %>% 
        
      add_trace(
        x = ~datetime, 
        y = ~quantile(coeff_p, 0.1, na.rm = T), 
        type = "scatter", 
         mode = "lines", 
        yaxis = 'y2', 
        opacity = opacity_level, 
        line = list(color = "black", dashed = "dots"), 
        name = "10th Quantile of Slopes",
        text = '',
        hoverinfo = 'text'
        
      ) %>% 
       
      layout(yaxis = list(type = "log", title = CUR_ASSET, autotick = F),
             yaxis2 = list(overlaying = 'y', side = 'right', autotick = F, title = "Record of Slope", zeroline = F),
             xaxis = list(range = c(a, b), title = ""), 
             legend = legend_global, 
             margin = y_margins
             ) 
    )
  }

historical_log_slopes_w_risk <- 
  function(DATA, 
           CUR_ASSET, 
           DAYS){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)
   
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
        
    with(data_set,
    plot_ly() %>% 
      
      add_trace(
        type = "scatter", 
        mode = "lines+markers", 
        x = ~datetime, 
        y = ~coeff_p, 
        color = ~scaled_risk,
        colors = gradient_color_palet,
        
        line = list(color = "lightgrey"),
         text = paste(
          "Date: ", datetime, 
          "<br> Risk: ", round(scaled_risk, 4), 
          "<br> Coef %: ", round(coeff_p, 4)
        ), 
        hoverinfo = 'text'
      ) %>% 
      layout(
        xaxis = list(range = c(a,b),
                     title = ""),
        yaxis = list(zeroline = FALSE,
                     #range = c(min_r, max_r),
                     #tickformat = "%",
                     title = "Coef %",
                     autotick = F
                     ),
        margin = y_margins

       )   %>%
      colorbar(len = 1, orientation = 'h', title = "Risk", limits = c(0,1))
    )
      
  }

hist_two_asset_slopes <- 
  function(
    DATA, 
    CUR_ASSET, 
    ASSET_2){
    
    data_set1 <- DATA %>% filter(asset %in% c(CUR_ASSET))
    data_set2 <- DATA %>% filter(asset %in% c(ASSET_2))
    
    results <- 
      merge(data_set1, 
            data_set2, 
            by = 'datetime', 
            all = F)
    min_d <- min(results$datetime)
    max_d <- max(results$datetime)
    
    ### main plot 
    a <- as.numeric(min_d) * 24 * 60 * 60 * 1000
    b <- as.numeric(max_d) * 24 * 60 * 60 * 1000
    
    with(results, 
    plot_ly() %>% 
  
      #slopes 1
      add_trace(
          x = ~datetime, 
          y = ~coeff_p.x, 
           
          opacity = opacity_level, 
          line = list(size = 2, color = color_1), 
          
          type = "scatter", 
          mode = "lines", 
          text = paste(
            "Date: ", datetime, 
            "<br> Coeff %:", round(coeff_p.x, 4)
          ), 
          hoverinfo = 'text', 
          
          name = paste(CUR_ASSET, "Coef %")
      ) %>%
      
      #slopes 2
      add_trace(
          x = ~datetime, 
          y = ~coeff_p.y, 
           
          opacity = opacity_level, 
          line = list(size = 2, color = color_2), 
          
          yaxis = 'y2', 
          type = "scatter", 
          mode = "lines", 
          
          text = paste(
            "Date: ", datetime, 
            "<br> Coeff %:", round(coeff_p.y, 4)
          ), 
          
          hoverinfo = 'text', 
          name = paste(ASSET_2, "Coef %")
      ) %>% 
      
      layout(
        xaxis = list(range = c(a,b),
                     title = ""),
        yaxis = list( title = CUR_ASSET, autotick = F, zeroline = F),
        yaxis2 = list( overlaying = 'y', side = 'right', title = ASSET_2, autotick = F, zeroline = F),
        
        margin = y_margins, 
        legend = legend_global
      )   
    )
    
      }
          
historical_price_and_rsi <- 
  function(
    DATA, 
    CUR_ASSET, 
    RSI_DAYS, 
    UPPER, 
    LOWER, 
    QUANTILE
  ){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)
    
    data_set$rsi_int <- RSI(data_set$price, n = RSI_DAYS)
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    with(data_set, 
         plot_ly() %>% 
           
           add_trace(
            x = ~datetime, 
            y = ~price, 
            
            type = "scatter", 
            mode = "lines", 
            
            line = list(color = color_1), 
            opacity = opacity_level, 
            
            text = paste(
              "Date: ", datetime, 
              "<br>Price", case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(data_set$price, 4), 
                            TRUE ~ as.character(round(data_set$price, 6))
                          ),
              "<br>RSI: ", round(rsi_int, 2)), 
            hoverinfo = 'text', 
            name = 'Price'
            ) %>% 
           
           add_trace(
             x = datetime, 
             y = rsi_int, 
             
             type = 'scatter', 
             mode = 'lines', 
             
             line = list(color = color_2), 
             opacity = opacity_level, 
             
             text = paste(
               "Date: ", datetime, 
               "<br>RSI: ", round(rsi_int,2)
             ), 
             hoverinfo = 'text', 
             name = 'RSI', 
             yaxis = 'y2'
           ) %>% 
           
           add_trace(
             x = datetime, 
             y = UPPER, 
             
             type = 'scatter', 
             mode = 'lines', 
             
             opacity = opacity_level, 
             line = list(color = 'black'), 
             
             text = '',
             hoverinfo = 'text', 
             
             name = 'RSI Upper Limit', 
             yaxis = 'y2'
           ) %>% 
           
           add_trace(
             x = datetime, 
             y = LOWER, 
             
             type = 'scatter', 
             mode = 'lines', 
             
             opacity = opacity_level, 
             line = list(color = 'black'), 
             
             text = '',
             hoverinfo = 'text', 
             
             name = 'RSI Lower Limit', 
             yaxis = 'y2'
           ) %>% 
           
           add_trace(
             x = datetime, 
             y = quantile(rsi_int, QUANTILE, na.rm = T), 
             
             type = 'scatter', 
             mode = 'lines', 
             
             opacity = opacity_level, 
             line = list(color = color_3, dash = 'dash'), 
             
             text = '',
             hoverinfo = 'text', 
             
             name = 'RSI Selected Quantile', 
             yaxis = 'y2'
             
           ) %>% 
           
           layout(
             xaxis = list(limits = c(a,b), title = ''), 
             yaxis = list(type = 'log', autotick = F, title = 'Price'), 
             yaxis2 = list(overlaying = 'y', side = 'right', title = 'RSI'), 
             margin = y_margins, 
             legend = legend_global
           )
           )
         
  }
    
rsi_and_risk <- 
  function(
    DATA, 
    CUR_ASSET, 
    RSI_DAYS
  ){
    data_set <- DATA %>% filter(asset == CUR_ASSET)
    
    data_set$rsi <- RSI(data_set$price, days = RSI_DAYS)
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    with(data_set,
    plot_ly() %>% 
      
      add_trace(
        type = "scatter", 
        mode = "lines+markers", 
        x = ~datetime, 
        y = ~rsi, 
        color = ~scaled_risk,
        colors = gradient_color_palet,
        
        line = list(color = "lightgrey"),
         text = paste(
          "Date: ", datetime, 
          "<br> Risk: ", round(scaled_risk, 4), 
          "<br> RSI: ", round(rsi, 2)
        ), 
        hoverinfo = 'text'
      ) %>% 
      layout(
        xaxis = list(range = c(a,b),
                     title = ""),
        yaxis = list(zeroline = FALSE,
                     #range = c(min_r, max_r),
                     #tickformat = "%",
                     title = "RSI"
                     ),
        margin = y_margins

       )   %>%
      colorbar(len = 1, orientation = 'h', title = "Risk", limits = c(0,1))
    )
  }
    
two_asset_rsi <- 
  function(
    DATA, 
    CUR_ASSET, 
    ASSET_2,
    RSI_DAYS
  ){
    
    data_set1 <- DATA %>% filter(asset %in% c(CUR_ASSET))
    data_set2 <- DATA %>% filter(asset %in% c(ASSET_2))
    
    data_set1$rsi <- RSI(data_set1$price, days = RSI_DAYS)
    data_set2$rsi <- RSI(data_set2$price, days = RSI_DAYS)
    
    results <- 
      merge(data_set1, 
            data_set2, 
            by = 'datetime', 
            all = F)
    
    ## slopes of asset 1 
    min_d <- min(results$datetime)
    max_d <- max(results$datetime)
    
    ### main plot 
    a <- as.numeric(min_d) * 24 * 60 * 60 * 1000
    b <- as.numeric(max_d) * 24 * 60 * 60 * 1000   
    
    with(results, 
    plot_ly() %>% 
  
      #slopes 1
      add_trace(
          x = ~datetime, 
          y = ~rsi.x, 
           
          opacity = opacity_level, 
          line = list(size = 2, color = color_1), 
          
          type = "scatter", 
          mode = "lines", 
          text = paste(
            "Date: ", datetime, 
            "<br>RSI:", round(rsi.x, 2)
          ), 
          hoverinfo = 'text', 
          
          name = paste(CUR_ASSET, "RSI")
      ) %>%
      
      #slopes 2
      add_trace(
          x = ~datetime, 
          y = ~rsi.y, 
           
          opacity = opacity_level, 
          line = list(size = 2, color = color_2), 
          
          yaxis = 'y2', 
          type = "scatter", 
          mode = "lines", 
          
          text = paste(
            "Date: ", datetime, 
            "<br>RSI:", round(rsi.y, 2)
          ), 
          
          hoverinfo = 'text', 
          name = paste(ASSET_2, "RSI")
      ) %>% 
      
      layout(
        xaxis = list(#range = c(a,b),
                     title = ""),
        yaxis = list( title = paste("RSI", CUR_ASSET), zeroline = F),
        yaxis2 = list( overlaying = 'y', side = 'right', title = paste("RSI", ASSET_2), zeroline = F),
        
        margin = y_margins, 
        legend = legend_global
      )   
    )
  }

####################################################################################################################
# DETECTION FUNCTIONS 
####################################################################################################################

## historical price and risk 
historical_price_plot <- 
  function(
    DATA, 
    CUR_ASSET, 
    Y_NAME
  ){
    
    name_x <- as.character(Y_NAME)
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    min_price <- floor(min(log10(data_set$price)))
    max_price <- ceiling(max(log10(data_set$price))) 
    # now, we need static plots that contain historic data and historic risk levels
    
    plot_ly(
      type = "scatter", 
      mode = 'markers+lines',
      
      data = data_set, 
      x = ~datetime, 
      y = ~(price), 
      
      line = list(color = "lightgrey"), 
      marker = list(color = ~scaled_risk, 
                    size = 6),
      
      color = ~scaled_risk, 
      colors = gradient_color_palet, 
      
      text = paste('Date: ', data_set$datetime,
                   '<br>Price: ', 
                          case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(data_set$price, 4), 
                            TRUE ~ as.character(round(data_set$price, 6))
                          ),
                   '<br>Risk: ', round(data_set$scaled_risk,4)), 
      hoverinfo = 'text'
      
    ) %>% layout(xaxis = list(range = c(a, b), title = "Time"), 
                 yaxis = list(type = "log", autotick = F, 
                              zeroline = FALSE, showline = FALSE,  
                              title = Y_NAME, 
                              range = c(min_price,max_price)), 
                 title = "Price on Log Base 10 Scale" 
    ) %>% 
      colorbar(len = 1, title = "Risk")
  }

historical_risk_plot <- 
  function(
    DATA, 
    CUR_ASSET
  ){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET) %>% arrange(datetime)
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    # now, we need static plots that contain historic data and historic risk levels
    
    plot_ly(
      type = "scatter", 
      mode = 'markers+lines',
      
      data = data_set, 
      x = ~datetime, 
      y = ~scaled_risk, 
      
      marker = list(size = 1, color = "dark grey"),
      line = list(size = 4, color = "#36454F"),
      group_by = 1, 
      text = paste('Date: ', data_set$datetime,
                   '<br>Risk: ', round(data_set$scaled_risk,4), 
                   '<br>Market Stage: ', data_set$market_stage), 
      hoverinfo = 'text'
      
    ) %>% layout(showlegend = FALSE, 
                 xaxis = list(range = c(a, b), title = "Time"), 
                 yaxis = list(zeroline = FALSE, showline = FALSE,  
                              title = "Risk", 
                              tickvals = seq(from = 0, to = 1, by = 0.1)),
                shapes = list( hline(.5, color = "blue"), hline(.9, color = "red"))
    )  #%>% add_trace(x = ~datetime, y = ~scaled_risk, mode = "lines", line = list(color = "grey", group = NA)) 
    
  }

## MA rate of change plot and historical values of thresholds
ma_rate_of_change_main_plot <- 
  function(DATA, 
           CUR_ASSET, 
           HARD, 
           SOFT){
    
    data_set <- DATA %>% filter(asset %in% c(CUR_ASSET))
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    ### two horizontal lines: minimum Rate of Change (ROC) at risk level beoynd 90
    ###                       90th quantile of historical ratios
    
    quant_90_ratio_b <- quantile(data_set$ma_roc, HARD, na.rm = T)
    quant_90_ratio_s <- quantile(data_set$ma_roc, SOFT, na.rm = T)
    
        
    with(data_set, 
    plot_ly() %>% 
    
    add_trace(x = ~datetime, 
              y = ~ma_roc, 
              opacity = opacity_level, 
              mode = "lines", 
              type = "scatter", 
              line = list(size = 4, color = "#3E4050"), 
              text = paste(
                "Date: ", datetime, 
                "<br>MA ROC : ", round(ma_roc,4)
              ), 
              hoverinfo = 'text'
              ) %>% 
    
    
    layout( yaxis = list( title = 'Rate of Change'), 
            
            xaxis = list(title = ""),
           
            legend = list(orientation = 'h',xanchor = "center", x = 0.5),
        
            margin = y_margins, 
            
            shapes = list(hline(quant_90_ratio_b, color = "red"),
                          hline(quant_90_ratio_s, color = "pink"),
                          hline(1, color = "black")))
    )
  }

ma_rate_of_change_hist_thresholds <- 
  function(DATA, 
           CUR_ASSET, 
           HARD, 
           SOFT){
    
    data_set <- DATA %>% filter(asset %in% c(CUR_ASSET))
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    ### two horizontal lines: minimum Rate of Change (ROC) at risk level beoynd 90
    ###                       90th quantile of historical ratios
        
    data_for_test <- data_set %>% filter(!is.na(ma_roc))
    
    ## create emolty data frame for results 
    results <- 
      data.frame(
        datetime = rep(NA, nrow(data_for_test)), 
        
        quantile_cutoff_b = rep(NA, nrow(data_for_test)),
        quantile_cutoff_s = rep(NA, nrow(data_for_test)),
        
        p_days_above_b = rep(NA, nrow(data_for_test)),
        p_days_above_s = rep(NA, nrow(data_for_test))
      )
    
    for(i in 1:nrow(data_for_test)){
      results$datetime[i] <- data_for_test$datetime[i]
      
      results$quantile_cutoff_b[i] <- quantile(data_for_test$ma_roc[1:i], HARD, na.rm = T)
      results$quantile_cutoff_s[i] <- quantile(data_for_test$ma_roc[1:i], SOFT, na.rm = T)
      
      results$n_days_above_b[i] <- length(which(
        data_for_test$ma_roc[1:i] > results$quantile_cutoff_b[i]))
      
      results$n_days_above_s[i] <- length(which(
        data_for_test$ma_roc[1:i] > results$quantile_cutoff_s[i]))
    }
    
    results$n <- seq(from = 1, to = nrow(results), by = 1)
      
    results$datetime <- as.Date(results$datetime, origin = as.Date("1970-01-01"))
    
    #### thresholdds over time 
    with(results, 
    plot_ly() %>% 
    
      add_trace(x = ~datetime, 
                y = ~quantile_cutoff_b, 
                opacity = opacity_level, 
                mode = "lines", 
                type = "scatter", 
                name = "strict",
                line = list(size = 4, color = "red"), 
                text = paste(
                  "Date: ", datetime, 
                  "<br>Strict Percentile Cutoff : ", round(quantile_cutoff_b,4), 
                  "<br>N Above Threshold: ", n_days_above_b
                ), 
                hoverinfo = 'text'
                ) %>% 
        
      add_trace(x = ~datetime, 
                y = ~quantile_cutoff_s, 
                opacity = opacity_level, 
                mode = "lines", 
                type = "scatter", 
                name = "relaxed",
                line = list(size = 4, color = "pink"), 
                text = paste(
                  "Date: ", datetime, 
                  "<br>Relaxed Percentile Cutoff : ", round(quantile_cutoff_s,4), 
                  "<br>N Above Threshold: ", n_days_above_s
                ), 
                hoverinfo = 'text'
                ) %>% 
      
      layout(margin = y_margins, 
             xaxis = list(title = ""), 
             yaxis = list(title = "Historical Cutoff Values"))
      
      )
  }

ma_rate_of_change_hist_margin <- 
  function(DATA, 
           CUR_ASSET, 
           HARD, 
           SOFT){
    
    data_set <- DATA %>% filter(asset %in% c(CUR_ASSET))
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    ### two horizontal lines: minimum Rate of Change (ROC) at risk level beoynd 90
    ###                       90th quantile of historical ratios
        
    data_for_test <- data_set %>% filter(!is.na(ma_roc))
    
    ## create emolty data frame for results 
    results <- 
      data.frame(
        datetime = rep(NA, nrow(data_for_test)), 
        
        quantile_cutoff_b = rep(NA, nrow(data_for_test)),
        quantile_cutoff_s = rep(NA, nrow(data_for_test)),
        
        p_days_above_b = rep(NA, nrow(data_for_test)),
        p_days_above_s = rep(NA, nrow(data_for_test))
      )
    
    for(i in 1:nrow(data_for_test)){
      results$datetime[i] <- data_for_test$datetime[i]
      
      results$quantile_cutoff_b[i] <- quantile(data_for_test$ma_roc[1:i], HARD, na.rm = T)
      results$quantile_cutoff_s[i] <- quantile(data_for_test$ma_roc[1:i], SOFT, na.rm = T)
      
      results$n_days_above_b[i] <- length(which(
        data_for_test$ma_roc[1:i] > results$quantile_cutoff_b[i]))
      
      results$n_days_above_s[i] <- length(which(
        data_for_test$ma_roc[1:i] > results$quantile_cutoff_s[i]))
    }
    
    results$n <- seq(from = 1, to = nrow(results), by = 1)
      
    results$datetime <- as.Date(results$datetime, origin = as.Date("1970-01-01"))
    
    results$margin <- with(results, quantile_cutoff_b - quantile_cutoff_s)
     
    with(results, 
    plot_ly() %>% 
    
    add_trace(x = ~datetime, 
              y = ~margin, 
              opacity = opacity_level, 
              mode = "lines", 
              type = "scatter", 
              name = "strict",
              line = list(size = 4, color = "red"), 
              text = paste(
                "Date: ", datetime, 
                "<br>Historical Margin Value : ", round(margin,4)
              ), 
              hoverinfo = 'text'
              ) %>% 
      layout(
        
        margin = y_margins, 
        
        xaxis = list(title = ""), 
        yaxis = list(title = "Margin Between Cutoff's", zeroline = F), 
        
        shapes = list(
          hline(mean(margin), color = "blue"),
          hline(mean(margin) - sd(margin), color = "green"),
          hline(mean(margin) + sd(margin), color = "green")
        )
      )
    ) 
  }

## Price extension from MA  plot and historical values of thresholds

ma_ext_main_plot <- 
  function(DATA, 
           CUR_ASSET, 
           HARD, 
           SOFT){
    
    data_set <- DATA %>% filter(asset %in% c(CUR_ASSET))
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    
    ### two horizontal lines: minimum Rate of Change (ROC) at risk level beoynd 90
    ###                       90th quantile of historical ratios
    
    quant_90_ratio_b <- quantile(data_set$price_ma_ratio, HARD, na.rm = T)
    quant_90_ratio_s <- quantile(data_set$price_ma_ratio, SOFT, na.rm = T)
    
        
    with(data_set, 
    plot_ly() %>% 
    
    add_trace(x = ~datetime, 
              y = ~price_ma_ratio, 
              opacity = opacity_level, 
              mode = "lines", 
              type = "scatter", 
              line = list(size = 4, color = "#3E4050"), 
              text = paste(
                "Date: ", datetime, 
                "<br>Price to MA Ratio : ", round(price_ma_ratio,4)
              ), 
              hoverinfo = 'text'
              ) %>% 
    
    
    layout( yaxis = list( title = '20 WMA Extension (Log Scale)', type = "log", autotick = F), 
            
            xaxis = list(title = ""),
           
            legend = list(orientation = 'h',xanchor = "center", x = 0.5),
        
            margin = y_margins, 
            
            shapes = list(hline(quant_90_ratio_b, color = "red"),
                          hline(quant_90_ratio_s, color = "pink"),
                          hline(1, color = "black")))
    )
  }

ma_ext_hist_thresholds <- 
  function(DATA, 
           CUR_ASSET, 
           HARD, 
           SOFT){
    
    data_set <- DATA %>% filter(asset %in% c(CUR_ASSET))
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    
    ### two horizontal lines: minimum Rate of Change (ROC) at risk level beoynd 90
    ###                       90th quantile of historical ratios
        
    data_for_test <- data_set %>% filter(!is.na(price_ma_ratio))
    
    ## create emolty data frame for results 
    results <- 
      data.frame(
        datetime = rep(NA, nrow(data_for_test)), 
        
        quantile_cutoff_b = rep(NA, nrow(data_for_test)),
        quantile_cutoff_s = rep(NA, nrow(data_for_test)),
        
        p_days_above_b = rep(NA, nrow(data_for_test)),
        p_days_above_s = rep(NA, nrow(data_for_test))
      )
    
    for(i in 1:nrow(data_for_test)){
      results$datetime[i] <- data_for_test$datetime[i]
      
      results$quantile_cutoff_b[i] <- quantile(data_for_test$price_ma_ratio[1:i], HARD, na.rm = T)
      results$quantile_cutoff_s[i] <- quantile(data_for_test$price_ma_ratio[1:i], SOFT, na.rm = T)
      
      results$n_days_above_b[i] <- length(which(
        data_for_test$price_ma_ratio[1:i] > results$quantile_cutoff_b[i]))
      
      results$n_days_above_s[i] <- length(which(
        data_for_test$price_ma_ratio[1:i] > results$quantile_cutoff_s[i]))
    }
    
    results$n <- seq(from = 1, to = nrow(results), by = 1)
      
    results$datetime <- as.Date(results$datetime, origin = as.Date("1970-01-01"))
    
    #### thresholdds over time 
    with(results, 
    plot_ly() %>% 
    
      add_trace(x = ~datetime, 
                y = ~quantile_cutoff_b, 
                opacity = opacity_level, 
                mode = "lines", 
                type = "scatter", 
                name = "strict",
                line = list(size = 4, color = "red"), 
                text = paste(
                  "Date: ", datetime, 
                  "<br>Strict Percentile Cutoff : ", round(quantile_cutoff_b,4), 
                  "<br>N Above Threshold: ", n_days_above_b
                ), 
                hoverinfo = 'text'
                ) %>% 
        
      add_trace(x = ~datetime, 
                y = ~quantile_cutoff_s, 
                opacity = opacity_level, 
                mode = "lines", 
                type = "scatter", 
                name = "relaxed",
                line = list(size = 4, color = "pink"), 
                text = paste(
                  "Date: ", datetime, 
                  "<br>Relaxed Percentile Cutoff  : ", round(quantile_cutoff_s,4), 
                  "<br>N Above Threshold: ", n_days_above_s
                ), 
                hoverinfo = 'text'
                ) %>% 
      
      layout(margin = y_margins, 
             yaxis = list(title = "Historical Cutoff Values"), 
             xaxis = list(title = ""))
      
      )
  }

ma_ext_hist_margin <- 
  function(DATA, 
           CUR_ASSET, 
           HARD, 
           SOFT){
    
    data_set <- DATA %>% filter(asset %in% c(CUR_ASSET))
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    
    ### two horizontal lines: minimum Rate of Change (ROC) at risk level beoynd 90
    ###                       90th quantile of historical ratios
        
    data_for_test <- data_set %>% filter(!is.na(price_ma_ratio))
    
    ## create emolty data frame for results 
    results <- 
      data.frame(
        datetime = rep(NA, nrow(data_for_test)), 
        
        quantile_cutoff_b = rep(NA, nrow(data_for_test)),
        quantile_cutoff_s = rep(NA, nrow(data_for_test)),
        
        p_days_above_b = rep(NA, nrow(data_for_test)),
        p_days_above_s = rep(NA, nrow(data_for_test))
      )
    
    for(i in 1:nrow(data_for_test)){
      results$datetime[i] <- data_for_test$datetime[i]
      
      results$quantile_cutoff_b[i] <- quantile(data_for_test$price_ma_ratio[1:i], HARD, na.rm = T)
      results$quantile_cutoff_s[i] <- quantile(data_for_test$price_ma_ratio[1:i], SOFT, na.rm = T)
      
      results$n_days_above_b[i] <- length(which(
        data_for_test$price_ma_ratio[1:i] > results$quantile_cutoff_b[i]))
      
      results$n_days_above_s[i] <- length(which(
        data_for_test$price_ma_ratio[1:i] > results$quantile_cutoff_s[i]))
    }
    
    results$n <- seq(from = 1, to = nrow(results), by = 1)
      
    results$datetime <- as.Date(results$datetime, origin = as.Date("1970-01-01"))
    
    results$margin <- with(results, quantile_cutoff_b - quantile_cutoff_s)
     
    with(results, 
    plot_ly() %>% 
    
    add_trace(x = ~datetime, 
              y = ~margin, 
              opacity = opacity_level, 
              mode = "lines", 
              type = "scatter", 
              name = "strict",
              line = list(size = 4, color = "red"), 
              text = paste(
                "Date: ", datetime, 
                "<br>Historical Margin Value : ", round(margin,4)
              ), 
              hoverinfo = 'text'
              ) %>% 
      layout(
        
        margin = y_margins, 
        
        xaxis = list(title = ""), 
        yaxis = list(title = "Margin Between Cutoff's", zeroline = F), 
        
        shapes = list(
          hline(mean(margin), color = "blue"),
          hline(mean(margin) - sd(margin), color = "green"),
          hline(mean(margin) + sd(margin), color = "green")
        )
      )
    ) 
  }

## Historical Short term slopes on log scale with historical thresholds 

hist_slopes_detection <- 
  function(DATA, 
           CUR_ASSET, 
           HARD, 
           SOFT){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    ### two horizontal lines: minimum Rate of Change (ROC) at risk level beoynd 90
    ###                       90th quantile of historical ratios
    
    quant_90_ratio_b <- quantile(data_set$coeff_p, HARD, na.rm = T)
    quant_90_ratio_s <- quantile(data_set$coeff_p, SOFT, na.rm = T)
    
        
    with(data_set, 
    plot_ly() %>% 
    
    add_trace(x = ~datetime, 
              y = ~coeff_p, 
              opacity = opacity_level, 
              mode = "lines", 
              type = "scatter", 
              line = list(size = 4, color = "#3E4050"), 
              text = paste(
                "Date: ", datetime, 
                "<br>Average 15 Day Change (Slope) : ", round(coeff_p,4)
              ), 
              hoverinfo = 'text'
              ) %>% 
    
    
    layout( yaxis = list( title = 'Historical Average 15 Day Change (Slope) (Log Scale)', type = "log", autotick = F), 
            
            xaxis = list(title = ""),
           
            legend = list(orientation = 'h',xanchor = "center", x = 0.5),
        
            margin = y_margins, 
            
            shapes = list(hline(quant_90_ratio_b, color = "red"),
                          hline(quant_90_ratio_s, color = "pink")))
    )
  }

hist_slopes_thresholds <- 
  function(DATA, 
           CUR_ASSET, 
           HARD, 
           SOFT){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)
     
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000

    ### two horizontal lines: minimum Rate of Change (ROC) at risk level beoynd 90
    ###                       90th quantile of historical ratios
    
    ## create emolty data frame for results 
    results <- 
      data.frame(
        datetime = rep(NA, nrow(data_set)), 
        
        quantile_cutoff_b = rep(NA, nrow(data_set)),
        quantile_cutoff_s = rep(NA, nrow(data_set)),
        
        p_days_above_b = rep(NA, nrow(data_set)),
        p_days_above_s = rep(NA, nrow(data_set))
      )
    
    for(i in 1:nrow(data_set)){
      results$datetime[i] <- data_set$datetime[i]
      
      results$quantile_cutoff_b[i] <- quantile(data_set$coeff_p[1:i], HARD, na.rm = T)
      results$quantile_cutoff_s[i] <- quantile(data_set$coeff_p[1:i], SOFT, na.rm = T)
      
      results$n_days_above_b[i] <- length(which(
        data_set$coeff_p[1:i] > results$quantile_cutoff_b[i]))
      
      results$n_days_above_s[i] <- length(which(
        data_set$coeff_p[1:i] > results$quantile_cutoff_s[i]))
    }
    
    results$n <- seq(from = 1, to = nrow(results), by = 1)
      
    results$datetime <- as.Date(results$datetime, origin = as.Date("1970-01-01"))
    
    #### thresholdds over time 
    with(results, 
    plot_ly() %>% 
    
      add_trace(x = ~datetime, 
                y = ~quantile_cutoff_b, 
                opacity = opacity_level, 
                mode = "lines", 
                type = "scatter", 
                name = "strict",
                line = list(size = 4, color = "red"), 
                text = paste(
                  "Date: ", datetime, 
                  "<br>Strict Percentile Cutoff : ", round(quantile_cutoff_b,4), 
                  "<br>N Above Threshold: ", n_days_above_b
                ), 
                hoverinfo = 'text'
                ) %>% 
        
      add_trace(x = ~datetime, 
                y = ~quantile_cutoff_s, 
                opacity = opacity_level, 
                mode = "lines", 
                type = "scatter", 
                name = "relaxed",
                line = list(size = 4, color = "pink"), 
                text = paste(
                  "Date: ", datetime, 
                  "<br>Relaxed Percentile Cutoff : ", round(quantile_cutoff_s,4), 
                  "<br>N Above Threshold: ", n_days_above_s
                ), 
                hoverinfo = 'text'
                ) %>% 
      
      layout(margin = y_margins, 
             xaxis = list(title = ""), 
             yaxis = list(title = "Historical Cutoff Values", zeroline = F))
      
      )
  }

hist_slopes_margin <- 
  function(DATA, 
           CUR_ASSET, 
           HARD, 
           SOFT){
    
     data_set <- DATA %>% filter(asset == CUR_ASSET)
      
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    ### two horizontal lines: minimum Rate of Change (ROC) at risk level beoynd 90
    ###                       90th quantile of historical ratios
    
    ## create emolty data frame for results 
    results <- 
      data.frame(
        datetime = rep(NA, nrow(data_set)), 
        
        quantile_cutoff_b = rep(NA, nrow(data_set)),
        quantile_cutoff_s = rep(NA, nrow(data_set)),
        
        p_days_above_b = rep(NA, nrow(data_set)),
        p_days_above_s = rep(NA, nrow(data_set))
      )
    
    for(i in 1:nrow(data_set)){
      results$datetime[i] <- data_set$datetime[i]
      
      results$quantile_cutoff_b[i] <- quantile(data_set$coeff_p[1:i], HARD, na.rm = T)
      results$quantile_cutoff_s[i] <- quantile(data_set$coeff_p[1:i], SOFT, na.rm = T)
      
      results$n_days_above_b[i] <- length(which(
        data_set$coeff_p[1:i] > results$quantile_cutoff_b[i]))
      
      results$n_days_above_s[i] <- length(which(
        data_set$coeff_p[1:i] > results$quantile_cutoff_s[i]))
    }
    
    results$n <- seq(from = 1, to = nrow(results), by = 1)
      
    results$datetime <- as.Date(results$datetime, origin = as.Date("1970-01-01"))
    
    results$margin <- with(results, quantile_cutoff_b - quantile_cutoff_s)
     
    with(results, 
    plot_ly() %>% 
    
    add_trace(x = ~datetime, 
              y = ~margin, 
              opacity = opacity_level, 
              mode = "lines", 
              type = "scatter", 
              name = "strict",
              line = list(size = 4, color = "red"), 
              text = paste(
                "Date: ", datetime, 
                "<br>Historical Margin Value : ", round(margin,4)
              ), 
              hoverinfo = 'text'
              ) %>% 
      layout(
        
        margin = y_margins, 
        
        yaxis = list(title = "Margin Between Cutoff's", zeroline = F),
        xaxis = list(title = ""), 
        
        shapes = list(
          hline(mean(margin, na.rm = T), color = "blue"),
          hline(mean(margin, na.rm = T) - sd(margin, na.rm = T), color = "green"),
          hline(mean(margin, na.rm = T) + sd(margin, na.rm = T), color = "green")
        )
      )
    ) 
  }

## Historical RSI 

hist_rsi_detection<- 
  function(
    DATA, 
    CUR_ASSET, 
    HARD, 
    SOFT
  ){
    data_set <- DATA %>% filter(asset == CUR_ASSET)
    
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    ### two horizontal lines: minimum Rate of Change (ROC) at risk level beoynd 90
    ###                       90th quantile of historical ratios
    
    quant_90_ratio_b <- quantile(data_set$rsi, HARD, na.rm = T)
    quant_90_ratio_s <- quantile(data_set$rsi, SOFT, na.rm = T)
    
        
    with(data_set, 
    plot_ly() %>% 
    
    add_trace(x = ~datetime, 
              y = ~rsi, 
              opacity = opacity_level, 
              mode = "lines", 
              type = "scatter", 
              line = list(size = 4, color = "#3E4050"), 
              text = paste(
                "Date: ", datetime, 
                "<br>RSI : ", round(rsi,2)
              ), 
              hoverinfo = 'text'
              ) %>% 
    
    
    layout( yaxis = list( title = 'Historical 14 Day RSI'), 
            
            xaxis = list(title = ""),
           
            legend = list(orientation = 'h',xanchor = "center", x = 0.5),
        
            margin = y_margins, 
            
            shapes = list(hline(quant_90_ratio_b, color = "red"),
                          hline(quant_90_ratio_s, color = "pink")))
    )
  }

hist_rsi_thresholds <- 
  function(DATA, 
           CUR_ASSET, 
           HARD, 
           SOFT){
    
    data_set <- DATA %>% filter(asset == CUR_ASSET)
     
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000

    ### two horizontal lines: minimum Rate of Change (ROC) at risk level beoynd 90
    ###                       90th quantile of historical ratios
    
    ## create emolty data frame for results 
    results <- 
      data.frame(
        datetime = rep(NA, nrow(data_set)), 
        
        quantile_cutoff_b = rep(NA, nrow(data_set)),
        quantile_cutoff_s = rep(NA, nrow(data_set)),
        
        p_days_above_b = rep(NA, nrow(data_set)),
        p_days_above_s = rep(NA, nrow(data_set))
      )
    
    for(i in 1:nrow(data_set)){
      results$datetime[i] <- data_set$datetime[i]
      
      results$quantile_cutoff_b[i] <- quantile(data_set$rsi[1:i], HARD, na.rm = T)
      results$quantile_cutoff_s[i] <- quantile(data_set$rsi[1:i], SOFT, na.rm = T)
      
      results$n_days_above_b[i] <- length(which(
        data_set$rsi[1:i] > results$quantile_cutoff_b[i]))
      
      results$n_days_above_s[i] <- length(which(
        data_set$rsi[1:i] > results$quantile_cutoff_s[i]))
    }
    
    results$n <- seq(from = 1, to = nrow(results), by = 1)
      
    results$datetime <- as.Date(results$datetime, origin = as.Date("1970-01-01"))
    
    #### thresholdds over time 
    with(results, 
    plot_ly() %>% 
    
      add_trace(x = ~datetime, 
                y = ~quantile_cutoff_b, 
                opacity = opacity_level, 
                mode = "lines", 
                type = "scatter", 
                name = "strict",
                line = list(size = 4, color = "red"), 
                text = paste(
                  "Date: ", datetime, 
                  "<br>Strict Percentile Cutoff : ", round(quantile_cutoff_b,4)
                ), 
                hoverinfo = 'text'
                ) %>% 
        
      add_trace(x = ~datetime, 
                y = ~quantile_cutoff_s, 
                opacity = opacity_level, 
                mode = "lines", 
                type = "scatter", 
                name = "relaxed",
                line = list(size = 4, color = "pink"), 
                text = paste(
                  "Date: ", datetime, 
                  "<br>Relaxed Percentile Cutoff : ", round(quantile_cutoff_s,4)
                ), 
                hoverinfo = 'text'
                ) %>% 
      
      layout(margin = y_margins, 
             yaxis = list(title = "Historical Cutoff Values", zeroline = F), 
             xaxis = list(title = ""))
      
      )
  }

hist_rsi_margin <- 
  function(DATA, 
           CUR_ASSET, 
           HARD, 
           SOFT){
    
     data_set <- DATA %>% filter(asset == CUR_ASSET)
      
    a <- as.numeric(min(data_set$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(data_set$datetime)) * 24 * 60 * 60 * 1000
    
    ### two horizontal lines: minimum Rate of Change (ROC) at risk level beoynd 90
    ###                       90th quantile of historical ratios
    
    ## create emolty data frame for results 
    results <- 
      data.frame(
        datetime = rep(NA, nrow(data_set)), 
        
        quantile_cutoff_b = rep(NA, nrow(data_set)),
        quantile_cutoff_s = rep(NA, nrow(data_set)),
        
        p_days_above_b = rep(NA, nrow(data_set)),
        p_days_above_s = rep(NA, nrow(data_set))
      )
    
    for(i in 1:nrow(data_set)){
      results$datetime[i] <- data_set$datetime[i]
      
      results$quantile_cutoff_b[i] <- quantile(data_set$rsi[1:i], HARD, na.rm = T)
      results$quantile_cutoff_s[i] <- quantile(data_set$rsi[1:i], SOFT, na.rm = T)
      
      results$n_days_above_b[i] <- length(which(
        data_set$rsi[1:i] > results$quantile_cutoff_b[i]))
      
      results$n_days_above_s[i] <- length(which(
        data_set$rsi[1:i] > results$quantile_cutoff_s[i]))
    }
    
    results$n <- seq(from = 1, to = nrow(results), by = 1)
      
    results$datetime <- as.Date(results$datetime, origin = as.Date("1970-01-01"))
    
    results$margin <- with(results, quantile_cutoff_b - quantile_cutoff_s)
     
    with(results, 
    plot_ly() %>% 
    
    add_trace(x = ~datetime, 
              y = ~margin, 
              opacity = opacity_level, 
              mode = "lines", 
              type = "scatter", 
              name = "strict",
              line = list(size = 4, color = "red"), 
              text = paste(
                "Date: ", datetime, 
                "<br>Historical Margin Value : ", round(margin,4)
              ), 
              hoverinfo = 'text'
              ) %>% 
      layout(
        
        margin = y_margins, 
        
        xaxis = list(title = ""), 
        yaxis = list(title = "Margin Between Cutoff's", zeroline = F), 
        
        shapes = list(
          hline(mean(margin, na.rm = T), color = "blue"),
          hline(mean(margin, na.rm = T) - sd(margin, na.rm = T), color = "green"),
          hline(mean(margin, na.rm = T) + sd(margin, na.rm = T), color = "green")
        )
      )
    ) 
  }
####################################################################################################################
# FORECAST FUNCTIONS 
####################################################################################################################

#### copy paste volatility patterns: evaluate risk of new prices

copy_paste_score_volatilty_pattern <- 
  function(INITIAL_DATA, 
           CUR_ASSET, 
           FORECAST_DATE_SET_1_1,
           FORECAST_DATE_SET_1_2,
           FORECAST_DATE_SET_2_1,
           FORECAST_DATE_SET_2_2){
    
    #### select first volatility pattern and calculate prices accordingly 
    
      ##all historical data 
      hist_data <- INITIAL_DATA %>% filter(asset == CUR_ASSET)
      hist_data$type <- "Historical"
      
      ##select volatilty pattern
      forecast_data <- hist_data %>% filter(datetime >= FORECAST_DATE_SET_1_1 & 
                                            datetime <= FORECAST_DATE_SET_1_2)
      
      forecast_vector <- forecast_data$volatility
      
      days_to_forecast <- length(forecast_vector)
      
      last_price_to_base_off <- hist_data$price[nrow(hist_data)]
      forecast_results <- c()
    
      forecast_days <- seq(from = (max(hist_data$datetime)+1), to = (max(hist_data$datetime)+days_to_forecast), by = 1)
      
      for(i in 1:days_to_forecast){
        iter_forecast <- last_price_to_base_off * forecast_vector[i]
        forecast_results <- c(forecast_results, iter_forecast)
        last_price_to_base_off <- forecast_results[length(forecast_results)]
      }
      
      forcasted_data <- 
        data.frame(
          forecast_days, 
          forecast_results, 
          forecast_vector
        )
      
      colnames(forcasted_data) <- c('datetime', 'price', 'volatility')
      forcasted_data$type <- "Copy-Pasted"
      
      ####data with forecasted bull market
      function_local_data <- 
        rbind(
            hist_data %>% select(datetime, price, volatility, type),
            forcasted_data
              ) 

      ### select second pattern and calculate prices again 
      forecast_data <- hist_data %>% filter(datetime >= FORECAST_DATE_SET_2_1 & 
                                              datetime <= FORECAST_DATE_SET_2_2)
      
      forecast_vector <- forecast_data$volatility
      days_to_forecast <- length(forecast_vector)
      
      last_price_to_base_off <- function_local_data$price[nrow(function_local_data)]
      forecast_results <- c()
    
      forecast_days <- seq(from = (max(function_local_data$datetime)+1), 
                           to = (max(function_local_data$datetime)+days_to_forecast), by = 1)
      
      for(i in 1:days_to_forecast){
        iter_forecast <- last_price_to_base_off * forecast_vector[i]
        forecast_results <- c(forecast_results, iter_forecast)
        last_price_to_base_off <- forecast_results[length(forecast_results)]
      }
      
      forcasted_data <- 
        data.frame(
          forecast_days, 
          forecast_results, 
          forecast_vector
        )
      
      colnames(forcasted_data) <- c('datetime', 'price', 'volatility')
      forcasted_data$type <- "Copy-Pasted"
      
      ## finisehd data 
      function_local_data <- rbind(
        function_local_data, 
        forcasted_data
      ) 
      
      ### now onto risk 
      function_local_data$n <- seq(from = 1, to = nrow(function_local_data), by = 1)
  
      if(CUR_ASSET == "BTC/USD"){
        function_local_data <- BTC_risk_metric(DATA = function_local_data, 
                                    DAY_MA = 50, 
                                    POWER = 2, 
                                    AVG_VOLATILITY_TIMEFRAME = 30,
                                  
                                    Y2_f = 16.7, 
                                    Y1_f = 18.5, 
                                    X2_f = 2634, 
                                    X1_f = 1156,
                                    POWER_TR = 1,
                                  
                                    Y2_f_l = 3,
                                    Y1_f_l = 2,
                                    X2_f_l = 1555,
                                    X1_f_l = 414)
      }
      
      if(CUR_ASSET %in% c("ETH/USD", "LINK/USD")){
        function_local_data <- ALT_risk_metric(DATA = function_local_data, 
                                               DAY_MA = 50, TIME_POWER = 2, RISK_POWER = 2, 
                                               AVG_VOLATILITY_TIMEFRAME = 30)
      }
      
      if(CUR_ASSET %in% c("ADA/USD", "THETA/USD", "VET/USD")){
        function_local_data <- ALT_risk_metric(DATA = function_local_data, 
                                               DAY_MA = 50, TIME_POWER = 10, RISK_POWER = 2, 
                                               AVG_VOLATILITY_TIMEFRAME = 30)
      }
      
      if(CUR_ASSET %in% c("ETH/BTC")){
        function_local_data <- ALT_risk_metric(DATA = function_local_data, 
                                               DAY_MA = 50, TIME_POWER = 10, RISK_POWER = 2.5, 
                                               AVG_VOLATILITY_TIMEFRAME = 30)
      }
      
      name_x <- as.character("Date")
    
    a <- as.numeric(min(function_local_data$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(function_local_data$datetime)) * 24 * 60 * 60 * 1000
    
    min_price <- floor(min(log10(function_local_data$price)))
    max_price <- ceiling(max(log10(function_local_data$price))) 
    # now, we need static plots that contain historic data and historic risk levels
    
    start_x <- max(hist_data$datetime)
    end_x <- max(hist_data$datetime)
    start_y <- 0 # min(function_local_data$price)
    end_y <- max(function_local_data$price)
  
    plot_ly(
      type = "scatter", 
      mode = 'markers+lines',
      
      data = function_local_data, 
      x = ~datetime, 
      y = ~(price), 
      
      line = list(color = "lightgrey"), 
      marker = list(color = ~scaled_risk, 
                    size = 6),
      
      color = ~scaled_risk, 
      colors = gradient_color_palet, 
      
      text = paste('Date: ', function_local_data$datetime,
                   '<br>Price: ', 
                          case_when(
                            CUR_ASSET %in% c(usd_lil_value_assets, usd_big_value_assets) ~ 
                                  format_dollar(function_local_data$price, 4), 
                            TRUE ~ as.character(round(function_local_data$price, 6))
                          ),
                   '<br>Risk: ', round(function_local_data$scaled_risk,4), 
                   '<br>Type: ', function_local_data$type), 
      hoverinfo = 'text'
      
    ) %>% layout(xaxis = list(range = c(a, b), title = "Time"), 
                 yaxis = list(type = "log", autotick = F, 
                              zeroline = FALSE, showline = FALSE,  
                              title = "BTC/USD", 
                              range = c(min_price,max_price)), 
                 title = "Price on Log Base 10 Scale",
        
                margin = y_margins, 
                shapes = list( vline(color = "blue", x0 = start_x, x1 = end_x, y0 = start_y, y1 = end_y) )
    ) %>% 
      colorbar(len = 1, title = "Risk")
      
      
  }

#### copy paste volatility patterns: evaluate risk of new values 

copy_paste_risk <- 
  function(INITIAL_DATA, 
           CUR_ASSET, 
           FORECAST_DATE_SET_1_1,
           FORECAST_DATE_SET_1_2,
           FORECAST_DATE_SET_2_1,
           FORECAST_DATE_SET_2_2){
    
    #### select first volatility pattern and calculate prices accordingly 
    
      ##all historical data 
      hist_data <- INITIAL_DATA %>% filter(asset == CUR_ASSET)
      hist_data$type <- "Historical"
      
      ##select volatilty pattern
      forecast_data <- hist_data %>% filter(datetime >= FORECAST_DATE_SET_1_1 & 
                                            datetime <= FORECAST_DATE_SET_1_2)
      
      forecast_vector <- forecast_data$volatility
      
      days_to_forecast <- length(forecast_vector)
      
      last_price_to_base_off <- hist_data$price[nrow(hist_data)]
      forecast_results <- c()
    
      forecast_days <- seq(from = (max(hist_data$datetime)+1), to = (max(hist_data$datetime)+days_to_forecast), by = 1)
      
      for(i in 1:days_to_forecast){
        iter_forecast <- last_price_to_base_off * forecast_vector[i]
        forecast_results <- c(forecast_results, iter_forecast)
        last_price_to_base_off <- forecast_results[length(forecast_results)]
      }
      
      forcasted_data <- 
        data.frame(
          forecast_days, 
          forecast_results, 
          forecast_vector
        )
      colnames(forcasted_data) <- c('datetime', 'price', 'volatility')
      forcasted_data$type <- "Copy-Pasted"
      
      ####data with forecasted bull market
      function_local_data <- 
        rbind(
            hist_data %>% select(datetime, price, volatility, type),
            forcasted_data
              ) 

      ### select second pattern and calculate prices again 
      forecast_data <- hist_data %>% filter(datetime >= FORECAST_DATE_SET_2_1 & 
                                              datetime <= FORECAST_DATE_SET_2_2)
      
      forecast_vector <- forecast_data$volatility
      days_to_forecast <- length(forecast_vector)
      
      last_price_to_base_off <- function_local_data$price[nrow(function_local_data)]
      forecast_results <- c()
    
      forecast_days <- seq(from = (max(function_local_data$datetime)+1), 
                           to = (max(function_local_data$datetime)+days_to_forecast), by = 1)
      
      for(i in 1:days_to_forecast){
        iter_forecast <- last_price_to_base_off * forecast_vector[i]
        forecast_results <- c(forecast_results, iter_forecast)
        last_price_to_base_off <- forecast_results[length(forecast_results)]
      }
      
      forcasted_data <- 
        data.frame(
          forecast_days, 
          forecast_results, 
          forecast_vector
        )
      
      colnames(forcasted_data) <- c('datetime', 'price', 'volatility')
      forcasted_data$type <- "Copy-Pasted"
      
      ## finisehd data 
      function_local_data <- rbind(
        function_local_data, 
        forcasted_data
      ) 
      
      ### now onto risk 
      function_local_data$n <- seq(from = 1, to = nrow(function_local_data), by = 1)
      
      if(CUR_ASSET == "BTC/USD"){
        function_local_data <- BTC_risk_metric(DATA = function_local_data, 
                                    DAY_MA = 50, 
                                    POWER = 2, 
                                    AVG_VOLATILITY_TIMEFRAME = 30,
                                  
                                    Y2_f = 16.7, 
                                    Y1_f = 18.5, 
                                    X2_f = 2634, 
                                    X1_f = 1156,
                                    POWER_TR = 1,
                                  
                                    Y2_f_l = 3,
                                    Y1_f_l = 2,
                                    X2_f_l = 1555,
                                    X1_f_l = 414)
      }
      
      if(CUR_ASSET %in% c("ETH/USD", "LINK/USD")){
        function_local_data <- ALT_risk_metric(DATA = function_local_data, 
                                               DAY_MA = 50, TIME_POWER = 2, RISK_POWER = 2, 
                                               AVG_VOLATILITY_TIMEFRAME = 30)
      }
      
      if(CUR_ASSET %in% c("ADA/USD", "THETA/USD", "VET/USD")){
        function_local_data <- ALT_risk_metric(DATA = function_local_data, 
                                               DAY_MA = 50, TIME_POWER = 10, RISK_POWER = 2, 
                                               AVG_VOLATILITY_TIMEFRAME = 30)
      }
      
      if(CUR_ASSET %in% c("ETH/BTC")){
        function_local_data <- ALT_risk_metric(DATA = function_local_data, 
                                               DAY_MA = 50, TIME_POWER = 10, RISK_POWER = 2.5, 
                                               AVG_VOLATILITY_TIMEFRAME = 30)
      }
      
      name_x <- as.character("Date")
    
    a <- as.numeric(min(function_local_data$datetime)) * 24 * 60 * 60 * 1000
    b <- as.numeric(max(function_local_data$datetime)) * 24 * 60 * 60 * 1000

    # now, we need static plots that contain historic data and historic risk levels
    
    start_x <- max(hist_data$datetime)
    end_x <- max(hist_data$datetime)
    start_y <- 0 # min(function_local_data$price)
    end_y <- max(function_local_data$price)
    
    plot_ly(
      type = "scatter", 
      mode = 'markers+lines',
      
      data = function_local_data, 
      x = ~datetime, 
      y = ~scaled_risk, 
      
      marker = list(size = 1, color = "dark grey"),
      line = list(size = 4, color = "#36454F"),
      group_by = 1, 
      text = paste('Date: ', function_local_data$datetime,
                   '<br>Risk: ', round(function_local_data$scaled_risk,4), 
                   '<br>Market Stage: ', function_local_data$market_stage, 
                    '<br>Type: ', function_local_data$type), 
      hoverinfo = 'text'
      
    ) %>% layout(showlegend = FALSE, 
                 xaxis = list(range = c(a, b), title = "Time"), 
                 yaxis = list(zeroline = FALSE, showline = FALSE,  
                              title = "Risk", 
                              tickvals = seq(from = 0, to = 1, by = 0.1)),
        
                margin = y_margins, 
                shapes = list( hline(.5, color = "green"), 
                                hline(.9, color = "red"), 
                                vline(color = "blue", x0 = start_x, x1 = end_x, y0 = start_y, y1 = end_y) )
    )  #%>% add_trace(x = ~datetime, y = ~scaled_risk, mode = "lines", line = list(color = "grey", group = NA)) 
    
      
      
  }

### interactive plot to play around with results of prophet forecasting 
prophet_test <- 
  function(DATA , 
            CUR_ASSET , 
            TRAIN_END , 
            TEST_END,
            FOURIER,
            PERIODS,
            SEASON
  ){
    
    train_data <- DATA %>% filter(asset == CUR_ASSET & datetime <= TRAIN_END)
    test_data <-  DATA %>% filter(asset == CUR_ASSET & datetime > TRAIN_END & 
                                                        datetime <= TEST_END)
    
    ##data to pass to prophet
    train_data_prophet <- train_data %>% select(datetime, price) %>% mutate(price = log10(price))
    colnames(train_data_prophet) <- c("ds", "y")
    # 
    # prophet_naked_model <- prophet(train_data_prophet, 
    #                                growth = 'logistic', 
    #                                 daily.seasonality=FALSE, 
    #                                 weekly.seasonality = FALSE, 
    #                                 yearly.seasonality = TRUE,
    #                                 seasonality.mode = "multiplicative") 
    

    ## create model
    # prophet_naked_model <- prophet(daily.seasonality=FALSE)
    # prophet_naked_model <- add_seasonality(prophet_naked_model, 
    #                                        name=SEASON, period=PERIODS, fourier.order=FOURIER, 
    #                                        mode = "multiplicative")
    # 
    # prophet_naked_model <- fit.prophet(prophet_naked_model, train_data_prophet)
    
    prophet_naked_model <- prophet(train_data_prophet,
                                    daily.seasonality=F, 
                                    seasonality.mode = "multiplicative")

    ## create forecast, note: days to forecast is the difference between test end date and train end date
    day_to_forecast <- as.numeric(TEST_END - TRAIN_END)
    ## create future dates  
    future <- make_future_dataframe(prophet_naked_model, periods = day_to_forecast)
    ## forecast proce on future days
    forecast <- predict(prophet_naked_model, future) %>% select(ds, yhat, yhat_lower, yhat_upper)
    
    ## transform values back to normal for now 
    forecast$yhat <- 10^forecast$yhat
    forecast$yhat_lower <- 10^forecast$yhat_lower
    forecast$yhat_upper <- 10^forecast$yhat_upper
    
    ## label train and test values as such
    forecast$ds <- as.Date(forecast$ds)
    
    forecast$type <- 
      with(forecast, 
      case_when(
        ds <= TRAIN_END ~ "Train", 
        TRUE ~ "Test"
      ))
    
    colnames(forecast) <- c("datetime", "yhat", "yhat_lower", "yhat_upper", "type")
    ## join original prices and risk estimates 
    forecast <- 
      merge(forecast, 
            DATA %>% filter(asset == CUR_ASSET & datetime <= TEST_END) %>% select(datetime, price, scaled_risk), 
            by = "datetime", 
            all = T
      )
    
    forecast_long <- 
      forecast %>% 
      pivot_longer(
        cols = c("yhat", "yhat_lower", "yhat_upper", "price"), 
        names_to = "price_type", 
        values_to = "value"
      )
    
    forecast_long$price_type <- 
      with(forecast_long, 
           case_when(
             price_type == "yhat" ~ "Predicted",
             price_type == "yhat_lower" ~ "Predicted Lower",
             price_type == "yhat_upper" ~ "Predicted Upper",
             price_type == "price" ~ "Observed"
           ))
    
    plot_ly(
      data = forecast_long, 
      
      type = "scatter", 
      mode = "lines", 
      
      y = ~value, 
      x = ~datetime, 
      color = ~price_type, 
      colors = c("black", "red", "blue", "blue")
    ) %>% 
      layout(yaxis = list(type = "log", 
                          autotick = F))

    
  }
###############
# Collect Data# 
###############

#BTC
{
  tempfile <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
  save_object(object = "s3://crypto-data-shiny/all_available_BTC.csv", file = tempfile)
  all_data <- read.csv(tempfile) %>% select(all_of(c('datetime', 'price', 'open', 'high', 'low'))) %>% arrange(datetime)

  all_data_EDA <- all_data %>% filter(
                                        # !(datetime >= as.Date("2014-02-13") & datetime <= as.Date("2014-02-25") ) & 
                                          # datetime >= as.Date("2010-10-26"))
                                          datetime >= as.Date("2010-12-01"))
  
  all_data_EDA <- modify_raw_data(DATA = all_data_EDA, ASSET_NAME = "BTC/USD")

  all_data_EDA <- BTC_risk_metric(DATA = all_data_EDA, 
                                    DAY_MA = 50, 
                                    POWER = 2, 
                                    AVG_VOLATILITY_TIMEFRAME = 30,
                                  
                                    Y2_f = 16.7, 
                                    Y1_f = 18.5, 
                                    X2_f = 2634, 
                                    X1_f = 1156,
                                    POWER_TR = 1,
                                  
                                    Y2_f_l = 3,
                                    Y1_f_l = 2,
                                    X2_f_l = 1555,
                                    X1_f_l = 414)
    
  all_data_EDA$market_stage <-
    as.factor(with(all_data_EDA,

                   case_when(
                     datetime < as.Date("2011-01-05") |
                       (datetime >= as.Date("2011-11-02") & datetime <= as.Date("2013-01-04")) |
                       (datetime >= as.Date("2015-01-02") & datetime <= as.Date("2016-08-14")) |
                       (datetime >= as.Date("2019-01-15") & datetime <= as.Date("2019-05-15")) |
                       (datetime >= as.Date("2019-07-16") & datetime <= as.Date("2020-09-30")) ~ "Accumulation",

                     (datetime >= as.Date("2011-01-05") & datetime <= as.Date("2011-06-15")) |
                       (datetime >= as.Date("2013-01-04") & datetime <= as.Date("2014-02-01")) |
                       (datetime >= as.Date("2016-08-15") & datetime <= as.Date("2018-01-15")) |
                       (datetime >= as.Date("2019-01-15") & datetime <= as.Date("2019-05-15")) |
                       (datetime >= as.Date("2019-05-16") & datetime <= as.Date("2019-07-15")) ~ "Bull",

                     datetime >= as.Date("2020-10-01") ~ "Current and Unknown",
                     TRUE ~ "Bear"
                   )
     ))
  
  all_data_EDA$ms <-
  with(all_data_EDA,
       case_when(
         datetime <= as.Date('2011-11-18') ~ 1,
         datetime >= as.Date('2011-11-19') & datetime <= as.Date('2015-01-14') ~ 2,
         datetime >= as.Date('2015-01-15') & datetime <= as.Date('2018-12-15') ~ 3,
         datetime >= as.Date('2018-12-16') ~ 4
       ))
    # to define market stages we need to find the date of the first and the last date of each market cycle:
  ms_dates <-
    all_data_EDA %>%
    group_by(ms) %>%
    summarise(first_ms_date = min(datetime),
              last_ms_date = max(datetime) )

  # to define market stages we need to find the date of the first and the last date of ATH's in each market cycle:
  ms_ath_dates <-
    all_data_EDA %>%
    filter(new_ath_flag == 1) %>%
    group_by(ms) %>%
    summarise(first_ath_date = min(datetime),
              last_ath_date = max(datetime),
              first_ath = min(price),
              last_ath = max(price))

  # join these dates to the data set and define market stages
  all_data_EDA <- merge(all_data_EDA, ms_dates, by = "ms")
  all_data_EDA <- merge(all_data_EDA, ms_ath_dates %>% select(ms, first_ath_date, last_ath_date), by = "ms")

  # define market stages now
  # define market stages now

  all_data_EDA$market_stage <- "str"

  all_data_EDA[all_data_EDA$ms %in% c(1,2,3), ]$market_stage <-
    with(all_data_EDA[all_data_EDA$ms %in% c(1,2,3), ],
         case_when(
           datetime >= first_ms_date & datetime < first_ath_date ~ "Accumulation",
           datetime >= first_ath_date & datetime <= last_ath_date ~ "Bull market",
           datetime > last_ath_date & datetime <= last_ms_date ~ "Bear market"
         ))
   
  all_data_EDA[all_data_EDA$ms == 4, ]$market_stage <-
    with(all_data_EDA[all_data_EDA$ms == 4, ],
         case_when(
           datetime >= first_ms_date & datetime < first_ath_date ~ "Accumulation",
           datetime >= first_ath_date & datetime <= last_ms_date ~ "Bull market"
         )
         )

  all_data_EDA[all_data_EDA$ms == 1 & all_data_EDA$market_stage == "Accumulation", ]$market_stage <- "Bull market"
  
  all_data_EDA <- all_data_EDA %>% 
    select(-first_ath_date, -first_ms_date, -last_ath_date, -last_ms_date, -raw_risk_slope_a, -week_ma_50, -ms)
  
  ## these data set are used to finalize ALT coins data sets 
  btc_20_week_flag <-
    all_data_EDA %>%

    select(datetime, price, week_ma_20) %>%

    mutate(
      market_stage = case_when(
        price > week_ma_20~ "BTC > 20 week MA",
        TRUE ~ "BTC < 20 week MA"
      )) %>% drop_na() %>% select(-price, - week_ma_20)

  btc_prices <-
    all_data_EDA %>%
    select(datetime, price) %>%
    rename(btc_price = price)
  
  
}

#ETH 
{
  tempfile <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
  save_object(object = "s3://crypto-data-shiny/all_available_ETH.csv", file = tempfile)
  all_data_EDA_ETH <- read.csv(tempfile) %>% select(all_of(c('datetime', 'price', 'open', 'high', 'low'))) %>% arrange(datetime)
 
  ### Modify data with functions
  all_data_EDA_ETH <- modify_raw_data(DATA = all_data_EDA_ETH, ASSET_NAME = "ETH/USD")
  all_data_EDA_ETH <- ALT_risk_metric(DATA = all_data_EDA_ETH, 
                                               DAY_MA = 50, TIME_POWER = 2, RISK_POWER = 2, 
                                               AVG_VOLATILITY_TIMEFRAME = 30)
  ### Market stage 
  all_data_EDA_ETH <- 
    merge(all_data_EDA_ETH, btc_20_week_flag, by = "datetime")
  
  
}

#LINK
{
  
  tempfile <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
  save_object(object = "s3://crypto-data-shiny/all_available_LINK.csv", file = tempfile)
  all_data_LINK_EDA <- read.csv(tempfile) %>% select(all_of(c('datetime', 'price', 'open', 'high', 'low'))) %>% arrange(datetime)
 
  ### Modify data with functions
  all_data_LINK_EDA <- modify_raw_data(DATA = all_data_LINK_EDA, ASSET_NAME = "LINK/USD")
  all_data_LINK_EDA <- ALT_risk_metric(DATA = all_data_LINK_EDA, 
                                               DAY_MA = 50, TIME_POWER = 2, RISK_POWER = 2, 
                                               AVG_VOLATILITY_TIMEFRAME = 30)
  
  all_data_LINK_EDA <- 
    merge(all_data_LINK_EDA, btc_20_week_flag, by = "datetime")
  
}

#ADA
{
  tempfile <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
  save_object(object = "s3://crypto-data-shiny/all_available_ADA.csv", file = tempfile)
  all_data_ADA_EDA <- read.csv(tempfile) %>% select(all_of(c('datetime', 'price', 'open', 'high', 'low'))) %>% arrange(datetime)
  
  all_data_ADA_EDA <- all_data_ADA_EDA %>% filter(datetime >= as.Date("2018-03-15"))
  
    ### Modify data with functions
  all_data_ADA_EDA <- modify_raw_data(DATA = all_data_ADA_EDA, ASSET_NAME = "ADA/USD")
  all_data_ADA_EDA <- ALT_risk_metric(DATA = all_data_ADA_EDA, 
                                               DAY_MA = 50, TIME_POWER = 10, RISK_POWER = 2, 
                                               AVG_VOLATILITY_TIMEFRAME = 30)
  
  all_data_ADA_EDA <- 
    merge(all_data_ADA_EDA, btc_20_week_flag, by = "datetime")
  
}

#THETA
{
  tempfile <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
  save_object(object = "s3://crypto-data-shiny/all_available_THETA.csv", file = tempfile)
  all_data_THETA_EDA <- read.csv(tempfile) %>% select(all_of(c('datetime', 'price', 'open', 'high', 'low')))   %>% arrange(datetime)
  
  all_data_THETA_EDA <- modify_raw_data(DATA = all_data_THETA_EDA, ASSET_NAME = "THETA/USD")
  all_data_THETA_EDA <- ALT_risk_metric(DATA = all_data_THETA_EDA, 
                                               DAY_MA = 50, TIME_POWER = 10, RISK_POWER = 2, 
                                               AVG_VOLATILITY_TIMEFRAME = 30)
  all_data_THETA_EDA <- 
    merge(all_data_THETA_EDA, btc_20_week_flag, by = "datetime")
  
}

#VET 
{
  tempfile <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
  save_object(object = "s3://crypto-data-shiny/all_available_VET.csv", file = tempfile)
  all_data_VET_EDA <- read.csv(tempfile) %>% select(all_of(c('datetime', 'price', 'open', 'high', 'low'))) %>% arrange(datetime)
  
  all_data_VET_EDA <- modify_raw_data(DATA = all_data_VET_EDA, ASSET_NAME = "VET/USD")
  all_data_VET_EDA <- ALT_risk_metric(DATA = all_data_VET_EDA, 
                                               DAY_MA = 50, TIME_POWER = 10, RISK_POWER = 2, 
                                               AVG_VOLATILITY_TIMEFRAME = 30)  

  all_data_VET_EDA <-
    merge(all_data_VET_EDA, btc_20_week_flag, by = "datetime")

}

# ETH/BTC
{
  e <- all_data_EDA_ETH %>% 
    select(datetime, price, high, low, open, market_stage)
  
  eth_btc_data <- e %>% left_join(btc_prices, by = "datetime")
  
  eth_btc_data$price <- with(eth_btc_data, price / btc_price)
  eth_btc_data$high  <- with(eth_btc_data, high / btc_price)
  eth_btc_data$low   <- with(eth_btc_data, low / btc_price)
  eth_btc_data$open  <- with(eth_btc_data, open / btc_price)

  eth_btc_data <- eth_btc_data %>% select(-btc_price)
  
  eth_btc_data <- modify_raw_data(DATA = eth_btc_data, ASSET_NAME = "ETH/BTC")
  eth_btc_data <- ALT_risk_metric(DATA = eth_btc_data, 
                                               DAY_MA = 50, TIME_POWER = 10, RISK_POWER = 2.5, 
                                               AVG_VOLATILITY_TIMEFRAME = 30)
  
}

## stack the data sets together to create one master data set 

master_data <- 
  rbind(all_data_EDA 
        ,all_data_EDA_ETH
        ,all_data_LINK_EDA
        ,all_data_ADA_EDA
        ,all_data_THETA_EDA
        ,all_data_VET_EDA
        ,eth_btc_data
        ) %>% 
  mutate(datetime = as.Date(datetime))

###########################
# Finished Data Collection#
###########################

server_side <- 
  function(input, output){ 
    
####################################################################################################################
# FRONT PAGE SUMMARY RESULTS
####################################################################################################################
      output$price_box <- 
        renderValueBox({
          price_value_box(
            DATA = master_data, 
            cur_asset = input$assets
            )
        })
      
      output$ma_box <- 
        renderValueBox({
          ma_value_box(
            DATA = master_data, 
            cur_asset = input$assets
          )
        })  
      
      output$risk_box <- 
        renderValueBox({
          risk_value_box(
            DATA = master_data, 
            cur_asset = input$assets
          )
        })    
      
      output$ath_box <- 
        renderValueBox({
          ath_value_box(
            DATA = master_data, 
            cur_asset = input$assets
          )
        })
    
    ######## boxes and text for date summary
    output$date_range_min_price <- 
      renderValueBox({
        range_min_price_value(
          DATA = master_data, 
          cur_asset = input$assets, 
          min_date = input$min_date, 
          max_date = input$max_date
        )
      })
    
    output$date_range_max_price <- 
      renderValueBox({
        range_max_price_value(
          DATA = master_data, 
          cur_asset = input$assets, 
          min_date = input$min_date, 
          max_date = input$max_date
        )
      })
      
    output$data_range_price_open <- 
      renderValueBox({
        range_price_close(
          DATA = master_data, 
          cur_asset = input$assets, 
          min_date = input$min_date, 
          max_date = input$max_date
        )
      })
    
    output$data_range_price_close <- 
      renderValueBox({
        range_open_price(
          DATA = master_data, 
          cur_asset = input$assets, 
          min_date = input$min_date, 
          max_date = input$max_date
        )
      })
    
    output$data_range_price_change <- 
      renderValueBox({
        range_max_price_change(
          DATA = master_data, 
          cur_asset = input$assets, 
          min_date = input$min_date, 
          max_date = input$max_date
        )
      })
    
    output$data_range_price_p_change <- 
      renderValueBox({
        range_max_price_range(
          DATA = master_data, 
          cur_asset = input$assets, 
          min_date = input$min_date, 
          max_date = input$max_date
        )
      })
    
    #######boxes for volatility of selsected days 
    output$data_range_pos_vol <- 
      renderValueBox({
        range_count_pos_days(
          DATA = master_data, 
          cur_asset = input$assets, 
          min_date = input$min_date, 
          max_date = input$max_date
        )
      })
    
    output$data_range_avg_pos_vol <- 
      renderValueBox({
        range_count_avg_pos_vol(
          DATA = master_data, 
          cur_asset = input$assets, 
          min_date = input$min_date, 
          max_date = input$max_date
        )
      })
   
    output$data_range_neg_vol <- 
      renderValueBox({
        range_count_neg_days(
          DATA = master_data, 
          cur_asset = input$assets, 
          min_date = input$min_date, 
          max_date = input$max_date
        )
      })
    
    output$data_range_avg_neg_vol <- 
      renderValueBox({
        range_count_avg_neg_vol(
          DATA = master_data, 
          cur_asset = input$assets, 
          min_date = input$min_date, 
          max_date = input$max_date
        )
      }) 
    
    ########price dynamics plot: 
    
    output$price_dynamics <- 
      renderPlotly({
        range_price_dynamics_plot(
          DATA = master_data, 
          cur_asset = input$assets, 
          min_date = input$min_date, 
          max_date = input$max_date
        )
      })
    
    output$price_dynamics_summary <- 
      renderTable(
        price_dynamics_summary(
          DATA = master_data, 
          cur_asset = input$assets, 
          min_date = input$min_date, 
          max_date = input$max_date
        )
      )
####################################################################################################################
# AGGREGATE INDICATORS RESULTS
####################################################################################################################
    
    #value boxes
    output$risk_agg_box <- 
      renderValueBox(
          risk_aggregate_box(
            DATA = master_data,
            CUR_ASSET = input$assets
      ))
    
    output$slope_agg_box <- 
      renderValueBox(
          slope_aggregate_box(
            DATA = master_data,
            CUR_ASSET = input$assets
            )
      )
    
    output$ma_ext_agg_box <- 
      renderValueBox({
          ma_ext_aggregate_box(
            DATA = master_data, 
            CUR_ASSET = input$assets)
      })
    
    output$ma_roc_agg_box <- 
      renderValueBox({
          ma_roc_aggregate_box(
            DATA = master_data, 
            CUR_ASSET = input$assets)
      })
    
    output$rsi_agg_box <- 
      renderValueBox({
          rsi_aggregate_box(
            DATA = master_data, 
            CUR_ASSET = input$assets)
      })
    
    #price plots with indicators 
    output$data_w_agg_inc <- 
      renderPlotly(
        price_and_aggs(
          DATA = master_data, 
            CUR_ASSET = input$assets
        )
      )
    
    #price plots with indicators 
    output$price_w_agg_inc_ <- 
      renderPlotly(
        price_and_aggs_lines(
          DATA = master_data, 
            CUR_ASSET = input$assets
        )
      )
    
####################################################################################################################
# OBSERVATION RESULTS 
####################################################################################################################
      ## HISTORICAL PRICE AND 20 WEEK MOVING AVERAGE 
      output$price_ma <- 
        renderPlotly({
          historical_w_20_week(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            Y_NAME = input$assets
          )
        })
      
      ## HISTORICAL RATIO OF PRICE AND 20 WEEK MA
      output$price_ma_ratio <- 
        renderPlotly({
          historical_price_ma_ratio(
            DATA = master_data, 
            CUR_ASSET = input$assets
          )
        })
      
      ## TWO ASSETS WITH PRICE AND 20 WEEK MA 
      
      output$two_asset_ma <- 
        renderPlotly({
          historical_two_asset_w_20_week(
            DATA = master_data, 
            CUR_ASSET = input$assets,
            ASSET_MA_1 = input$ma_asset_1
          )
        })
      
      ## TWO ASSETS' EXTENSIONS
      output$two_asset_me_extension <- 
        renderPlotly({
          historical_two_asset_20_week_ext(
            DATA = master_data, 
            CUR_ASSET = input$assets,
            ASSET_MA_1 = input$ma_asset_1
          )
        })
      
      ## TWO ASSETS WITH PRICE AND 20 WEEK MA BY DAYS 
      output$two_asset_ma_by_days <- 
        renderPlotly({
          historical_two_asset_w_20_week_by_days(
            DATA = master_data, 
            CUTOFF = input$cutoff_two_asset_ma, 
            CUR_ASSET = input$assets,
            ASSET_MA_1 = input$ma_asset_1
          )
        })
      
      ## TWO ASSETS' EXTENSIONS BY DAYS 
      output$two_asset_ma_extensions_by_days <- 
        renderPlotly({
          historical_two_asset_20_week_ext_by_days(
            DATA = master_data, 
            CUTOFF = input$cutoff_two_asset_ma, 
            CUR_ASSET = input$assets,
            ASSET_MA_1 = input$ma_asset_1
          )
        })
      
      ## TWO ASSETS' MAX DAYS 
      output$max_days_ma <- 
        renderTable(
          max_days_ma_table(
            DATA = master_data, 
            CUTOFF = input$cutoff_two_asset_ma, 
            CUR_ASSET = input$assets,
            ASSET_MA_1 = input$ma_asset_1
          )
        )
      
      ## price and 20 week MA rate of change 
      output$price_and_ma_rate_of_change <- 
        renderPlotly({
          historical_price_and_ma_rate_of_change(
            DATA = master_data, 
            CUR_ASSET = input$assets
          )
        })
      
      ## historical 20 week MA Rate of Change with risk color 
      output$ma_rate_of_change_w_risk <- 
        renderPlotly({
          historical_ma_roc(
            DATA = master_data, 
            CUR_ASSET = input$assets
          )
        })
      
      
      ## ATH of price and 20 week MA 
      output$price_and_ma_aths <- 
        renderPlotly({
          historical_price_and_ath_and_20_week_ma(
            DATA = master_data, 
            CUR_ASSET = input$assets
          )
        })
      
      ## ATH plots 
      #######two asset plot
      output$two_asset_ath <- 
        renderPlotly({
          historical_price_and_ath(
            DATA = master_data, 
            ASSET_1 = input$assets ,
            ASSET_2 = input$assets2
          )
        })
      
      #####ath timelines 
      output$ath_timeline <- 
        renderPlotly({
          just_ath_timeline(
            DATA = master_data, 
            ASSET_1 = input$assets ,
            ASSET_2 = input$assets2
          )
        })
      
      
      ## SINGLE ASSET ATHS
      output$single_asset_aths <- 
        renderPlotly(
          historical_price_and_ath_one(
            DATA = master_data, 
            CUR_ASSET = input$assets
          )
        )
      
      ## % DOWN FROM RECENT ATH 
      output$p_down_ath_plot <- 
        renderPlotly({
          percent_down_from_ath(
            DATA = master_data, 
            CUR_ASSET = input$assets
          )
        })
      
     ## CONSEQUTIVE DAYS PLOT 
        output$cons_days_plot <- 
          renderPlotly({
            consequtive_days_plot(
              DATA = master_data, 
              CUR_ASSET = input$assets, 
              CUR_CUTOFF = input$cons_day_slider, 
              SIZE_DOTS = input$size_dots_slider, 
              OPACITY_LEVEL = input$opacity_slider
            )
          }) 
        
      ## consequtive days plot version 2 
        output$cons_days_plot2 <- 
          renderPlotly(
            consequtive_days_plot2(
              DATA = master_data, 
              CUR_ASSET = input$assets
            )
          )
        
      ## CONSEQUTIVE DAYS TABLE 
        output$cons_days_table <- 
          renderDT ({
            consequtive_days_table(
              DATA = master_data, 
              CUR_ASSET = input$assets
            )
          })
        
      ## CONSEQUTIVE DAYS PLOT 1 
        output$consequtive_sum_plot <- 
          renderPlotly ({
            consequtive_days_sum_plot(
              DATA = master_data, 
              CUR_ASSET = input$assets
            )
          })
      
      ## CONSEQUTIVE DAYS PLOT 2 
        output$consequtive_sum_plot_2 <- 
          renderPlotly ({
            consequtive_days_sum_plot_2(
              DATA = master_data, 
              CUR_ASSET = input$assets
            )
          })
        
      ## historical slopes with price 
      output$hist_log_slopes_w_price <- 
        renderPlotly(
          historical_log_slopes_w_price(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            DAYS = input$days_slopes)
        )
      
      ## historical slopes with risk 
      output$historical_log_slopes_w_risk <- 
        renderPlotly(
          historical_log_slopes_w_risk(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            DAYS = input$days_slopes)
        )
      
      ## historical slopes of two assets 
      output$two_asset_slopes <- 
        renderPlotly(
          hist_two_asset_slopes(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            ASSET_2 = input$assets3
          )
        )
      
      ## rsi and price 
      output$rsi_and_price <- 
        renderPlotly(
          historical_price_and_rsi(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            RSI_DAYS = input$rsi_days, 
            UPPER = input$upper_rsi, 
            LOWER = input$lower_rsi,
            QUANTILE = input$quantile_rsi
          )
        )
      
      ## rsi and risk 
      output$rsi_and_risk <- 
        renderPlotly(
          rsi_and_risk(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            RSI_DAYS = input$rsi_days
          )
        )
      
      ## two asset rsi
      output$two_asset_rsi_output <- 
        renderPlotly(
          two_asset_rsi(
            DATA = master_data, 
            CUR_ASSET = input$assets,  
            ASSET_2 = input$second_asset_rsi,
            RSI_DAYS = input$rsi_days
          )
        )

####################################################################################################################
# DETECTION RESULTS
####################################################################################################################
       
      ### price and risk plots 
      ## HISTORICAL PRICE WITH COLORED RISK LEVELS
      output$hist_price <- 
        renderPlotly({
          historical_price_plot(
              DATA = master_data, 
              CUR_ASSET = input$assets, 
              Y_NAME = input$assets
          )
        })
      
      ## HISTORICAL RISK LEVEL WITH MASRKET STAGE 
      output$hist_risk <- 
        renderPlotly({
          historical_risk_plot(
            DATA = master_data, 
            CUR_ASSET = input$assets
          )
        })
      
      ## MA rate of change detection tool 
      output$ma_roc_detection <- 
        renderPlotly(
          ma_rate_of_change_main_plot(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            HARD = input$ma_roc_hard_cut, 
            SOFT = input$ma_roc_soft_cut
          )
        )
      ## MA rate of change historical threshold 
      output$ma_roc_hist_thr <- 
        renderPlotly(
          ma_rate_of_change_hist_thresholds(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            HARD = input$ma_roc_hard_cut, 
            SOFT = input$ma_roc_soft_cut
          )
        )
      
      ## MA rate of change historical margin 
      output$ma_roc_hist_margin <- 
        renderPlotly(
          ma_rate_of_change_hist_margin(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            HARD = input$ma_roc_hard_cut, 
            SOFT = input$ma_roc_soft_cut
          )
        )

      ## MA extension detection tool 
      output$ma_ext_detection <- 
        renderPlotly(
          ma_ext_main_plot(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            HARD = input$ma_ext_hard_cut, 
            SOFT = input$ma_ext_soft_cut
          )
        )
      ## MA extension historical threshold 
      output$ma_ext_hist_thr <- 
        renderPlotly(
          ma_ext_hist_thresholds(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            HARD = input$ma_ext_hard_cut, 
            SOFT = input$ma_ext_soft_cut
          )
        )
      
      ## MA extension historical margin 
      output$ma_ext_hist_margin <- 
        renderPlotly(
          ma_ext_hist_margin(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            HARD = input$ma_ext_hard_cut, 
            SOFT = input$ma_ext_soft_cut
          )
        )
       
      ## historical slopes detection tool 
      output$hist_slopes_detection <- 
        renderPlotly(
          hist_slopes_detection(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            HARD = input$hist_slopes_hard_cut, 
            SOFT = input$hist_slopes_soft_cut
          )
        )
      ## historical slopes threshold 
      output$hist_slopes_hist_thr <- 
        renderPlotly(
          hist_slopes_thresholds(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            HARD = input$hist_slopes_hard_cut, 
            SOFT = input$hist_slopes_soft_cut
          )
        )
      
      ## historical slopes margin 
      output$hist_slopes_hist_margin <- 
        renderPlotly(
          hist_slopes_margin(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            HARD = input$hist_slopes_hard_cut, 
            SOFT = input$hist_slopes_soft_cut
          )
        )      
      
    ## historical rsi detection tool 
      output$hist_rsi_detection <- 
        renderPlotly(
          hist_rsi_detection(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            HARD = input$hist_rsi_hard_cut, 
            SOFT = input$hist_rsi_soft_cut
          )
        )
      ## historical rsi threshold 
      output$hist_rsi_thresholds <- 
        renderPlotly(
          hist_rsi_thresholds(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            HARD = input$hist_rsi_hard_cut, 
            SOFT = input$hist_rsi_soft_cut
          )
        )
      
      ## historical rsi margin 
      output$hist_rsi_margin <- 
        renderPlotly(
          hist_rsi_margin(
            DATA = master_data, 
            CUR_ASSET = input$assets, 
            HARD = input$hist_rsi_hard_cut, 
            SOFT = input$hist_rsi_soft_cut
          )
        )      
####################################################################################################################
# FORECAST RESULTS 
####################################################################################################################
       
      ## COPY PASTE VOLATILITY PATTERNS 
        output$copy_paste_vol <- 
          renderPlotly({
            copy_paste_score_volatilty_pattern(
              INITIAL_DATA = master_data, 
               CUR_ASSET = input$assets, 
               FORECAST_DATE_SET_1_1 = input$forcast_date_1_1,
               FORECAST_DATE_SET_1_2 = input$forcast_date_1_2,
               FORECAST_DATE_SET_2_1 = input$forcast_date_2_1,
               FORECAST_DATE_SET_2_2 = input$forcast_date_2_2)
          })
        
        output$copy_paste_risk <- 
          renderPlotly({
            copy_paste_risk(
              INITIAL_DATA = master_data, 
               CUR_ASSET = input$assets, 
               FORECAST_DATE_SET_1_1 = input$forcast_date_1_1,
               FORECAST_DATE_SET_1_2 = input$forcast_date_1_2,
               FORECAST_DATE_SET_2_1 = input$forcast_date_2_1,
               FORECAST_DATE_SET_2_2 = input$forcast_date_2_2)
          })
      
        output$prophet_model_test <- 
            renderPlotly({
              prophet_test(
                DATA = master_data, 
                  CUR_ASSET = input$assets, 
                  TRAIN_END = input$prophet_cutoff_train, 
                  TEST_END = input$prophet_cutoff_test, 
                  FOURIER = input$fourier,
                  PERIODS = input$ph_periods, 
                  SEASON = input$season_type
              )
            })
        
      }

sidebar <- 
  dashboardSidebar(
    width = 300, 
    sidebarMenu(


      dateInput("min_date", label = "Select Start Date:", value = (max(master_data$datetime)-15))
      
      ,dateInput("max_date", label = "Select End Date:", value = (max(master_data$datetime)))
      
      ,selectizeInput("assets", "Symbol Search: ", unique(master_data$asset))
      
#      ,sliderInput("cons_day_slider", "Select Cons. Days Highlight Cutoff", min = 1, max = 20, value = 5, step = 1)
      
#      ,sliderInput("size_dots_slider", "Select Size of Highlight Dots", min = 3, max = 20, value = 8, step = 1)
      
      ,menuItem("Introduction", tabName = "intro")
      ,menuItem("Risk and Market Dynamic", tabName = "risk_data")
      ,menuItem("Aggregate Indicators", tabName = "agg_ind")
      ,menuItem("Observations", tabName = "obs_data")
      ,menuItem("Detection Tools", tabName = "det_tools")
      ,menuItem("Forecast", tabName = "forecast_data")
      
    )
)

body <-
  dashboardBody(
    tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }
                                '))), 
    tabItems(
      tabItem(tabName = "intro", 
              tags$h1('Cryptocurrency Risk Advanced Analytics'), 
              br(), 
              
              HTML("Using empirical risk minimization, <i> a principle in statistical 
                 learning theory which defines a family of learning algorithms and is used to 
                 give theoretical bounds on their performance</i>, we can identify the peak 
                 of the bubble. We estimate risk based on factors like current price, volatility, 
                among others. 
                 <br>
                 <b>So, risk for each price represent probability that a given price is a peak of a 
                 speculative bubble</b>"), 
              
              br(), 
              br(), 
              
              HTML("On December 16, 2017, <b> Bitcoin reached an all time high at a risk level of 0.9946</b>,
                 We’re anticipating another peak of the bubble soon, and our risk 
                 analysis will let us know when that is."), 
              
              br(), 
              br(), 
              
              tags$h2("Dashboard Features"), 
              
              HTML("
                 <ul>
                 
                 <li> Interactive Graphs 
                 
                      <ul>
                      
                      <li> Hover and select a box for a summary of <i> specific data points </i> </li>
                      
                      <li> Zoom in and out with simple double clicks</li>
                      
                      <li> Interactive legends</li>
                      
                      <li> Specify date ranges</li>
                      
                       
                 
                 </li>
                 
                 <li> Multiple Tabs for each Cryptocurrency
                 
                      <ul>
                      
                      <li> <b>Recent Price and Risk: </b> 
                              Analyze prices, risks, and 20-week moving averages for 
                              recent data (14 days by default).
                          </li>
                      
                      <li> <b>Historical Price and Risk: </b>  
                              Analyze prices, risks, and market cycles for all-time data.
                          </li>

                      <li> <b>Historical Price and 20-Week Moving Average: </b>  
                              Analyze the relationship between prices and 20-week 
                              simple moving averages. The 20-week MA is a solid level of 
                              support during a bull market.

                          </li>
                      
                      </ul>
                 
                 </li>
                 
                 </ul>
                 "), 
              tags$h2('Other Dashboards'), 
              HTML('
              <li>
                  There are other wodnerful resources that assess current market stage. Resources are listed below: 
                   
                  <ul>
                      <li>
                      <a href="https://narrative-investing.io/reports/narrative_report_Fear_crypto_.html">
                        Narrative Index: Analysis of sentiment of tweets, headlines, posts, etc.</a>
                      </li>
                      
                      <li>
                      <a href="https://alternative.me/crypto/fear-and-greed-index/">
                        Fear and Greed Index</a>
                      </li>
                      
                      <li>
                      <a href="https://www.blockchaincenter.net/bitcoin-rainbow-chart/">
                        Bitcoin Rainbow Chart</a>
                      </li>
                      
                      <li>
                      <a href="https://stats.buybitcoinworldwide.com/stock-to-flow/">
                        Bitcoin Stock-to-Flow Model</a>
                      </li>
                      
                      <li>
                      <a href="https://wenbullrun.com">
                        When Bull Run</a>
                      </li>
                      
                      <li>
                      <a href="https://www.coinglass.com/merge/BTC-USD">
                        Order Book Data </a>
                      </li>
                      
                      <li>
                      <a href="https://charts.bitbo.io/long-term-power-law/">
                        Power Law Curves <a/>
                      </li?
                      
                      
                      
                      
                  </ul>
                   
              </li>')
      ),
      
      tabItem(tabName = "risk_data", 
               fluidRow(
                 mainPanel(
                   titlePanel(h2("Current Highlights", align = "center")), 
                   valueBoxOutput('price_box', width = 6),
                   valueBoxOutput('risk_box', width = 6),
                   valueBoxOutput('ma_box', width = 6),
                   valueBoxOutput('ath_box', width = 6),
                   width = 12
                 ), 
                 mainPanel(
                    titlePanel(h2(
                                  paste0("Selected Date Range Summary"), 
                                  align = "center")
                               ), 
                    
                    tabsetPanel( type = "tabs", 
                       tabPanel("Price Dynamics", 
                               box(plotlyOutput('price_dynamics'), width = 8), 
                               box(tableOutput('price_dynamics_summary'), width = 4, title = "Summary Table")), 
                      tabPanel("Price Levels",
                               h3("Price", align = "center"), 
                               valueBoxOutput('date_range_min_price', width = 2),
                               valueBoxOutput('date_range_max_price', width = 2), 
                               valueBoxOutput('data_range_price_open', width = 2), 
                               valueBoxOutput('data_range_price_close', width = 2), 
                               valueBoxOutput('data_range_price_change', width = 2),
                               valueBoxOutput('data_range_price_p_change', width = 2), 
                               h3("Volatility", align = "center"),
                               valueBoxOutput('data_range_pos_vol', width = 3),
                               valueBoxOutput('data_range_avg_pos_vol', width = 3),
                               valueBoxOutput('data_range_neg_vol', width = 3),
                               valueBoxOutput('data_range_avg_neg_vol', width = 3)
                               )
                    ), 
                    
                    width = 12
                 ))
               ), 
      
      tabItem(tabName = 'agg_ind', 
              titlePanel(h2("Market Indicators", align = 'center')),  
              fluidRow(
                valueBoxOutput('risk_agg_box', width = 2),
                valueBoxOutput('slope_agg_box', width = 3),
                valueBoxOutput('ma_ext_agg_box', width = 2),
                valueBoxOutput('ma_roc_agg_box', width = 3),
                valueBoxOutput('rsi_agg_box', width = 2)
              ), 
              titlePanel(h2("Historical Levels of Market Indicators", align = 'center')), 
              fluidRow(
                box(plotlyOutput('data_w_agg_inc'), width = 10), 
                box(numericInput(inputId = "agg_low_cut", 
                                 label = "Lowest Bucket to Show", 
                                 value = -5, 
                                 min = -5, 
                                 max = 5), width = 2), 
                box(numericInput(inputId = "agg_low_cut", 
                                 label = "Highest Bucket to Show", 
                                 value = 5, 
                                 min = -5, 
                                 max = 5), width = 2), 
                box(plotlyOutput('price_w_agg_inc_'), width = 10)
              )
              ),
      
      tabItem(tabName = "obs_data", 
              tabsetPanel(type = "tab", 
                          
                          tabPanel("All Time Price and 20 Week MA", 
                                   tabsetPanel(type = "tabs", 
                                     tabPanel("Extension from 20 Week MA", 
                                              fluidRow(
                                      box(title = "Daily price and Moving Average", 
                                       width = 10,
                                       plotlyOutput('price_ma',
                                                    width = "auto", height = "auto")),
                                      
                                      box(title = "Extension from 20 Week MA", 
                                       width = 10,
                                       plotlyOutput('price_ma_ratio',
                                                    width = "auto", height = "auto"))
                                   )), 
                                   tabPanel("Two Asset Extension", 
                                            tabsetPanel(type = "tabs", 
                                                        tabPanel("By Dates",
                                                          fluidRow(
                                                  box(title = "Two-Asset Price with MA",
                                                      plotlyOutput('two_asset_ma',
                                                                width = "auto", height = "auto"), width = 10),
                                                  box(selectizeInput("ma_asset_1", "Select Second Asset: ",
                                                                    c("ADA/USD", "BTC/USD","ETH/USD","LINK/USD"
                                                                      ,"THETA/USD", "VET/USD", 
                                                                      "ETH/BTC", "LINK/BTC"), 
                                                                    selected = "ETH/USD"), width = 2
                                                   ),
                                                  box(title = "Two-Asset Extensions",
                                                      plotlyOutput('two_asset_me_extension',
                                                                width = "auto", height = "auto"), width = 10)
                                                  
                                                   )
                                                  ), 
                                                        tabPanel("By Days",
                                                          fluidRow(
                                                            box(title = "Two-Asset Price with MA" 
                                                                ,plotlyOutput('two_asset_ma_by_days')
                                                                ,width = 10
                                                                ), 
                                                            box(sliderInput("cutoff_two_asset_ma", 
                                                                            "Select max day number", 
                                                                            min = 2, 
                                                                            max = nrow(all_data_EDA), 
                                                                            step = 1, 
                                                                            value = nrow(all_data_EDA)
                                                                            ), 
                                                                tableOutput('max_days_ma'), 
                                                                width = 2), 
                                                            box(title = "Two-Asset Extensions"
                                                                ,width = 10
                                                                ,plotlyOutput('two_asset_ma_extensions_by_days')
                                                                )
                                                          ))
                                                        )
                                            ), 
                                   
                                   tabPanel("ATH of Asset and 20 Week MA",
                                            fluidRow(
                                              box(plotlyOutput('price_and_ma_aths'), width = 10)
                                            )), 
                                   
                                   tabPanel("20 Week MA Rate of Change", 
                                            fluidRow(
                                              box(plotlyOutput('price_and_ma_rate_of_change'), width = 10),
                                              box(plotlyOutput('ma_rate_of_change_w_risk'), width = 10)
                                            ))
                                   )),
                          
                          tabPanel("All Time High's",
                                   tabsetPanel(type = "tabs", 
                                               tabPanel("Selected Asset", 
                                                    fluidRow(
                                                      box(plotlyOutput('single_asset_aths'), width = 10), 
                                                      box(plotlyOutput('p_down_ath_plot'), width = 10)
                                                    )), 
                                               
                                               tabPanel("Two Assets", 
                                                   fluidRow(
                                                      box(title = "Two-Asset Price with ATH",
                                                          plotlyOutput('two_asset_ath',
                                                                    width = "auto", height = "auto"), width = 10),
                                                      box(selectizeInput("assets2", "Select Second Asset: ",
                                                                        c("ADA/USD", "BTC/USD","ETH/USD","LINK/USD"
                                                                          ,"THETA/USD", "VET/USD", 
                                                                          "ETH/BTC", "LINK/BTC"), 
                                                                        selected = "ETH/USD"),
                                                         width = 2), 
                                                      box(title = "Timeline of ATHs for Two Assets",
                                                          plotlyOutput('ath_timeline',
                                                                    width = "auto", height = "auto"), width = 10)
                                                    
                                                   )
                                               )
                                   )),

                          tabPanel("Consequtive Days", 
                                   fluidRow(
                                     box(title = "Daily Price with Highlighted Consequtive Days of Same Type Volatility",
                                       width = 10,
                                       plotlyOutput('cons_days_plot', width = "auto", height = "auto")
                                     ), 
                                     box(sliderInput("cons_day_slider", 
                                                      "Select Cons. Days Highlight Cutoff", 
                                                      min = 1, max = 20, value = 5, step = 1), 
                                          sliderInput("size_dots_slider", 
                                                      "Select Size of Highlight Dots", 
                                                      min = 3, max = 20, value = 8, step = 1), 
                                          sliderInput("opacity_slider", 
                                                      "Select Opacity of Highlight Dots", 
                                                      min = 0.1, max = 1, value = 0.75, step = 0.05), width = 2), 
                                     box(plotlyOutput('cons_days_plot2'), width = 10)
                                     ), 
                                     fluidRow(
                                       tabsetPanel(type = "tabs", 
                                                   tabPanel("% of Total Within Sequence", 
                                                            box(plotlyOutput('consequtive_sum_plot'), width = 10)), 
                                                   tabPanel("% of Total Sequences", 
                                                            box(plotlyOutput('consequtive_sum_plot_2'), width = 10)), 
                                                   tabPanel("Summary Table",
                                                            box(DTOutput('cons_days_table'), width = 10)))
                                     )
                                    
                                   ), 

                          tabPanel("Historical Slopes", 
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Selected Asset",
                                                        fluidRow(
                                                          box(plotlyOutput('hist_log_slopes_w_price'), width = 10),
                                                          box(sliderInput("days_slopes", 
                                                                        "Number of Days to Estimate Coef %", 
                                                                        min = 7, max = 45, value = 15, step = 1), width = 2), 
                                                          box(plotlyOutput('historical_log_slopes_w_risk'), width = 10)
                                                          ) ), 
                                               tabPanel("Two Assets",
                                                        fluidRow(
                                                          box(
                                                             plotlyOutput('two_asset_slopes'), 
                                                            width = 10), 
                                                          box(selectizeInput("assets3", "Select Second Asset: ",
                                                                        c("ADA/USD", "BTC/USD","ETH/USD","LINK/USD"
                                                                          ,"THETA/USD", "VET/USD", 
                                                                          "ETH/BTC", "LINK/BTC"), 
                                                                        selected = "ETH/USD"), width = 2)
                                                        )))
                                   ), 
                          
                          tabPanel("RSI", 
                                   tabsetPanel(type = 'tabs', 
                                               
                                               tabPanel('Selected Asset', 
                                                        fluidRow(
                                                           box(plotlyOutput('rsi_and_price'), width = 10), 
                                                           box(
                                                            sliderInput("rsi_days", 
                                                                        "RSI Day Basis", 
                                                                        min = 7, max = 900, value = 14, step = 1),
                                                            sliderInput("upper_rsi", 
                                                                        "RSI Upper Limit", 
                                                                        min = 60, max = 90, value = 80, step = 1),
                                                            sliderInput("lower_rsi", 
                                                                        "RSI Lower Limit", 
                                                                        min = 10, max = 40, value = 20, step = 1), 
                                                            sliderInput("quantile_rsi", 
                                                                        "Quantile of RSI", 
                                                                        min = 0.1, max = 0.9, value = 0.75, step = 0.01), 
                                                            width = 2), 
                                                           box(plotlyOutput('rsi_and_risk'), width = 10)
                                                        )), 
                                               tabPanel('Two Assets',
                                                        fluidRow(
                                                          box(plotlyOutput('two_asset_rsi_output'), width = 10),
                                                          box(selectizeInput("second_asset_rsi", 
                                                                             "Select Second Asset: ",
                                                                        c("ADA/USD", "BTC/USD","ETH/USD","LINK/USD"
                                                                          ,"THETA/USD", "VET/USD", 
                                                                          "ETH/BTC", "LINK/BTC"), 
                                                                        selected = "ETH/USD"), width = 2)
                                                        ))))
                          )
              ),
      tabItem(tabName = "det_tools",
              tabsetPanel(type = "tabs", 
                          
                          tabPanel("All Time Price and Risk",
                                   fluidRow(box(title = "Daily Price Data and Risk Levels  ",
                                         width = 10,
                                         plotlyOutput('hist_price',
                                                      width = "auto", height = "auto")),
                                     box(title = "Market Cycles and Risk Levels",
                                         width = 10,
                                         plotlyOutput('hist_risk',
                                                      width = "auto", height = "auto")))), 
                          tabPanel("20 Week MA Extension", 
                                   fluidRow(
                                     box(plotlyOutput('ma_ext_detection'), width = 10), 
                                     box(sliderInput("ma_ext_hard_cut", 
                                                      "Strict (Red) Cutoff", 
                                                      min = 0.01, max = 1, value = 0.9, step = 0.01), 
                                          sliderInput("ma_ext_soft_cut", 
                                                      "Relaxed (Pink) Cutoff", 
                                                      min = 0.1, max = 1, value = 0.85, step = 0.01), width = 2), 
                                     box(plotlyOutput('ma_ext_hist_thr'), width = 6), 
                                     box(plotlyOutput('ma_ext_hist_margin'), width = 6)
                                   )), 
                          tabPanel("20 Week MA ROC", 
                                   fluidRow(
                                     box(plotlyOutput('ma_roc_detection'), width = 10), 
                                     box(sliderInput("ma_roc_hard_cut", 
                                                      "Strict (Red) Curoff", 
                                                      min = 0.01, max = 1, value = 0.9, step = 0.01), 
                                          sliderInput("ma_roc_soft_cut", 
                                                      "Relaxed (Pink) Curoff", 
                                                      min = 0.1, max = 1, value = 0.85, step = 0.01), width = 2), 
                                     box(plotlyOutput('ma_roc_hist_thr'), width = 6), 
                                     box(plotlyOutput('ma_roc_hist_margin'), width = 6)
                                   )), 
                          tabPanel("Historical Short Term Slopes",
                                   fluidRow(
                                     box(plotlyOutput('hist_slopes_detection'), width = 10), 
                                     box(sliderInput("hist_slopes_hard_cut", 
                                                      "Strict (Red) Curoff", 
                                                      min = 0.01, max = 1, value = 0.9, step = 0.01), 
                                          sliderInput("hist_slopes_soft_cut", 
                                                      "Relaxed (Pink) Curoff", 
                                                      min = 0.1, max = 1, value = 0.85, step = 0.01), width = 2),
                                     box(plotlyOutput('hist_slopes_hist_thr'),width = 6),
                                     box(plotlyOutput('hist_slopes_hist_margin'),width = 6)
                                   ) ), 
                          tabPanel("Historical RSI",
                                   fluidRow(
                                     box(plotlyOutput('hist_rsi_detection'), width = 10), 
                                     box(sliderInput("hist_rsi_hard_cut", 
                                                      "Strict (Red) Curoff", 
                                                      min = 0.01, max = 1, value = 0.9, step = 0.01), 
                                          sliderInput("hist_rsi_soft_cut", 
                                                      "Relaxed (Pink) Curoff", 
                                                      min = 0.1, max = 1, value = 0.85, step = 0.01), width = 2),
                                     box(plotlyOutput('hist_rsi_thresholds'),width = 6),
                                     box(plotlyOutput('hist_rsi_margin'),width = 6)
                                   ) )
                                   )
                          ),
      
      tabItem(tabName = "forecast_data", 
              tabsetPanel(type = "tabs", 
                          tabPanel("Copy Volatility Pattern",
                                   fluidRow(
                                     box(plotlyOutput('copy_paste_vol'), width = 10), 
                                      box(
                                        dateInput("forcast_date_1_1", label = "Set 1 Start Date:", 
                                                  value = as.Date("2017-09-14")), 
                                        
                                        dateInput("forcast_date_1_2", label = "Set 1 Start Date:", 
                                                  value = as.Date("2017-12-16")), 
                                                                            
                                        dateInput("forcast_date_2_1", label = "Set 2 Start Date:", 
                                                  value = as.Date("2017-12-17")), 
                                                                                                              
                                        dateInput("forcast_date_2_2", label = "Set 2 End Date:", 
                                                  value = as.Date("2018-12-18")), 
                                        width = 2
                                        ), 
                                     box(plotlyOutput('copy_paste_risk'), width = 10)
                                     )
                            )
                          # , 
                          #   tabPanel("Prophet Model Test", 
                          #            fluidRow(
                          #              box(plotlyOutput('prophet_model_test'), width = 10), 
                          #              box(
                          #                
                          #                numericInput("fourier", label = "Fourier Parameter:", 
                          #                             value = 3), 
                          #                numericInput("ph_periods", label = "Periods Parameter:", 
                          #                             value = 720), 
                          #                selectizeInput("season_type", label = "Seasonality Type: ",
                          #                               selected = "yearly", 
                          #                               choices = c("weekly", "monthy", "yearly")), 
                          #                 dateInput("prophet_cutoff_train", label = "End Date of Training Period:", 
                          #                         value = as.Date("2021-01-01")), 
                          #               
                          #                dateInput("prophet_cutoff_test", label = "End Date of Testing Period:", 
                          #                         value = as.Date("2021-03-01")),
                          #                
                          #                width = 2)
                          #            ))
              
              )
              )
              
    )
    
  )


shinyApp(
  ui = dashboardPage(
    dashboardHeader(title =  "Coin Galaxy", titleWidth = 300),
    sidebar,
    body
  ),
  server = server_side
)
