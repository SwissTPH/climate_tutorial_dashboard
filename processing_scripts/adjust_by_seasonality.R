###################################
# Function that takes as input a time series of rainfall and adjusts it 
# according to a selected seasonality profile
#
# created by israel.uwakuba@swisstph.ch
# 26.11.2024
##################################

seasonality_fun = function(X_df, seasonality){
  
  # decompose the timeseries
  dY = decompose(ts(X_df, frequency = 365))
  
  # fit a polynomial to the seasonal component of the decomposed timeseries
  Y0 = dY$seasonal[1:365]
  x = 1:length(Y0)
  mod = lm(Y0 ~ poly(x, 6, raw=TRUE))
  
  pars_df0 = unname(unlist(coef(mod)))
  
  poly_fun = function(pars_df,x){
    
    # pars_df=pars_df0
    # 
    # pars_df[7]=pars_df0[7]
    pred_Y = pars_df[1] + (x*pars_df[2] + (pars_df[3]*(x^2)) + 
                             (pars_df[4]*(x^3)) + (pars_df[5]*(x^4)) + 
                             (pars_df[6]*(x^5))+(pars_df[7]*(x^6)))
    
    return(pred_Y)
  }
  
  # extend, compress or stretch the seasonality using the fitted polynomial
  #amplified_seasonality=2*poly_fun(pars_df =pars_df0,x=x)
  amplified_seasonality = 2*Y0
  longer_seasonality = poly_fun(pars_df =pars_df0,x=x*(1-0.3))
  shorter_seasonality = poly_fun(pars_df =pars_df0,x=x*(1+0.3))
  
  
  seasonal_adjusted = NULL
  # remove the seasonality of the original data and add the modified seasonality
  if(grepl("amplified", seasonality)){
    seasonal_adjusted = pmax(0,(dY$x-dY$seasonal) + rep(amplified_seasonality, length.out=length(X_df)))
  } else if(grepl("longer", seasonality)){
    seasonal_adjusted = pmax(0,(dY$x-dY$seasonal) + rep(longer_seasonality, length.out=length(X_df)))
  } else if(grepl("shorter", seasonality)){
    seasonal_adjusted = pmax(0,(dY$x-dY$seasonal) + rep(shorter_seasonality, length.out=length(X_df)))
  }
  
  return(seasonal_adjusted)
}
