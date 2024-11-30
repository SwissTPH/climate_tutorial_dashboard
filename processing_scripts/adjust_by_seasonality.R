###################################
# Function that takes as input a time series of rainfall and adjusts it 
# according to a selected seasonality profile
#
# created by israel.uwakuba@swisstph.ch
# 26.11.2024
##################################

seasonality_fun_v1 = function(X_df, seasonality){
  
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

seasonality_fun_v2=function(X_df, seasonality){
  
  # decompose the timeseries
  dY=decompose(ts(X_df, frequency = 365), type = "multi")
  
  # fit a polynomial to the seasonal component of the decomposed timeseries
  Y0=dY$seasonal[1:365]
  
  x=1:length(Y0)
  mod=lm(Y0 ~ poly(x, 6, raw=TRUE))
  
  pars_df0=unname(unlist(coef(mod)))
  
  poly_fun=function(pars_df,x){
    pred_Y=pars_df[1]+(x*pars_df[2]+(pars_df[3]*(x^2))+(pars_df[4]*(x^3))+(pars_df[5]*(x^4))+(pars_df[6]*(x^5))+(pars_df[7]*(x^6)))
    
    return(pred_Y)
  }
  
  # the fitted seasonality
  X=poly_fun(pars_df =pars_df0,x=x)
  
  # extend, compress or stretch the seasonality using the fitted polynomial
  amplified_seasonality=2*Y0
  longer_seasonality=poly_fun(pars_df =pars_df0,x=x*(1-0.3))

  # use exponential to make the seasonality shorter. easier to accomplish than with polynomial
  # first fit the modelled seaonality to a double exponential, then adjust the rates to get a shorter seaon
  shorter_form<-formula(X~(((exp(-A1*(x-B1)^2)+1*exp(-A1*(x-B2)^2)))))
  shorter_mod<-nls(shorter_form, start=list(A1=0.001,B1=100,B2=300), control = nls.control(maxiter = 500))
  
  
  
  # to get a shoter seasonality, make the slope sharper i.e. increase it's rate of change
  
  A1_short=coef(shorter_mod)["A1"]*(20)
  B1=coef(shorter_mod)["B1"]
  B2=coef(shorter_mod)["B2"]
  
  Xs0=X*(((exp(-A1_short*(x-B1)^2)+exp(-A1_short*(x-B2)^2))))
  # then rescale the shorter seasonality to match the range of the orignal seasonality X
  shorter_seasonality=(max(X) - min(X))/(max(Xs0) - min(Xs0)) * (Xs0 - max(Xs0)) + max(X)
  
  
  # remove the seasonality of the original data and add the modified seasonality
  if(grepl("amplified", seasonality)){
    return(pmax(0,(dY$x/dY$seasonal)*rep(amplified_seasonality, length.out=length(X_df))))
  }else if(grepl("longer", seasonality)){
    pmax(0,(dY$x/dY$seasonal)*rep(longer_seasonality, length.out=length(X_df)))
  }else if(grepl("shorter", seasonality)){
    pmax(0,(dY$x/dY$seasonal)*rep(shorter_seasonality, length.out=length(X_df)))
  }
  
}

seasonality_fun=function(X_df,seasonality){
  
  #seasonality="shorter"
  #X_df=met_df$norm_ppt
  
  # decompose the timeseries
  dY=decompose(ts(X_df, frequency = 365), type = "multi")
  
  # fit a polynomial to the seasonal component of the decomposed timeseries
  Y0=dY$seasonal[1:365]
  
  # plot(dY)
  # 
  # plot(dY$x[1:365]/Y0,type="l")
  
  #plot(Y0,type="l")
  
  x=1:length(Y0)
  mod=lm(Y0 ~ poly(x, 6, raw=TRUE))
  
  pars_df0=unname(unlist(coef(mod)))
  
  poly_fun=function(pars_df,x){
    
    # pars_df=pars_df0
    # 
    # pars_df[7]=pars_df0[7]
    pred_Y=pars_df[1]+(x*pars_df[2]+(pars_df[3]*(x^2))+(pars_df[4]*(x^3))+(pars_df[5]*(x^4))+(pars_df[6]*(x^5))+(pars_df[7]*(x^6)))
    
    return(pred_Y)
  }
  
  # the fitted seasonality
  X=poly_fun(pars_df =pars_df0,x=x)
  
  # extend, compress or stretch the seasonality using the fitted polynomial
  #amplified_seasonality=2*poly_fun(pars_df =pars_df0,x=x)
  amplified_seasonality=2*Y0
  longer_seasonality=poly_fun(pars_df =pars_df0,x=x*(1-0.3))
  #shorter_seasonality=poly_fun(pars_df =pars_df0,x=x*(1+0.3))
  #shorter_seasonality=poly_fun(pars_df =pars_df0,x=x)*exp(1.5*((X-max(X)))/min(X))
  
  # use exponential to make the seasonality shorter. easier to accomplish than with polynomial
  # first fit the modelled seaonality to a double exponential, then adjust the rates to get a shorter seaon
  shorter_form<-formula(X~(((exp(-A1*(x-B1)^2)+1*exp(-A1*(x-B2)^2)))))
  
  shorter_mod<-nls(shorter_form,start=list(A1=0.001,B1=100,B2=300),control = list(maxiter=1e3))
  #summary(shorter_mod)
  #cor(X,predict(shorter_mod))
  #coef(shorter_mod)
  #plot(x,X)
  #lines(x,predict(shorter_mod),col="red",lty=2,lwd=3)
  
  
  # to get a shoter seasonality, make the slope sharper i.e. increase it's rate of change
  
  #A1_long=coef(shorter_mod)["A1"]*(1-0.5)
  A1_short=coef(shorter_mod)["A1"]*(20)
  B1=coef(shorter_mod)["B1"]
  B2=coef(shorter_mod)["B2"]
  
  Xs0=X*(((exp(-A1_short*(x-B1)^2)+exp(-A1_short*(x-B2)^2))))
  # then rescale the shorter seasonality to match the range of the orignal seasonality X
  shorter_seasonality=(max(X) - min(X))/(max(Xs0) - min(Xs0)) * (Xs0 - max(Xs0)) + max(X)
  
  
  #Xl0=X*(((exp(-A1_long*(x-B1)^2)+exp(-A1_long*(x-B2)^2))))
  # then rescale the shorter seasonality to match the range of the orignal seasonality X
  #longer_seasonality=(max(X) - min(X))/(max(Xl0) - min(Xl0)) * (Xl0 - max(Xl0)) + max(X)
  
  
  # plot(longer_seasonality,type="l")
  # lines(shorter_seasonality)
  # lines(X)
  
  # par(mfrow=c(1,1))
  # plot(X,type="l")
  # lines(Xs0,col="blue")
  # lines(shorter_seasonality,col="blue")
  
  # remove the seasonality of the original data and add the modified seasonality
  if(grepl("amplified", seasonality)){
    return(pmax(0,(dY$x/dY$seasonal)*rep(amplified_seasonality, length.out=length(X_df))))
  }else if(grepl("longer", seasonality)){
    pmax(0,(dY$x/dY$seasonal)*rep(longer_seasonality, length.out=length(X_df)))
  }else if(grepl("shorter", seasonality)){
    pmax(0,(dY$x/dY$seasonal)*rep(shorter_seasonality, length.out=length(X_df)))
  }
  
}


