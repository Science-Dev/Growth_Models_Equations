# Growth Models


# Growth Models Definition ------------------------------------------------



# Schumacher Growth Models ------------------------------------------------




# Schumacher model with 2 parameters A (Form 1)

Schum_mod.a <- deriv( ~a*(exp(-b/t)), c("a", "b"), function(t, a, b){} )




# Schumacher model with 2 parameters B

Schum_mod <- deriv( ~a*(exp(b/t)), c("a", "b"), function(t, a, b){} )




# Schumacher model with 3 parameters B

Schum_mod.c <- deriv( ~a*exp(b/(t^c)), c("a", "b", "c"), function(t, a, b, c){} )







# Weibull Growth Model ----------------------------------------------------


# Weibull model

Weib_mod <- deriv( ~a*(1-exp(-b*(t^c))), c("a", "b", "c"), function(t, a, b, c){} )


# Weibull model

Weib_mod.b <- deriv( ~a*(1-b*exp(-c*((t)^d))), c("a", "b", "c", "d"), function(t, a, b, c, d){} )




# Logistic Growth Model ---------------------------------------------------


# Logistic model with 3 parameters

Logist_mod <- deriv( ~a/(1+b*exp(-c*t)), c("a", "b", "c"), function(t, a, b, c){} )


Logist_mod.b <- deriv( ~a/(1+exp(b-c*t)), c("a", "b", "c"), function(t, a, b, c){} )





# Gompertz Growth Model ---------------------------------------------------


# Gompertz model with 3 parameters

Gomp_mod <- deriv( ~a*exp(-b*exp(-c*t)), c("a", "b", "c"), function(t, a, b, c){} )








# Exponential Growth Model ------------------------------------------------


# Exponential model with 2 parameters

Exp_mod <- deriv( ~a*exp(b*t), c("a", "b"), function(t, a, b){} )










# Negative Exponential Growth Model ----------------------------------------------


# Negative Exponential model y(t) = α * ( 1 - exp(-k * t))

Negexp_mod <- deriv( ~a*(1-exp(-b*t)), c("a", "b"), function(t, a, b){} )







# Chapman Richards Growth Model -------------------------------------------


# Chapman-Richards base model with 3 parameters

CR_mod <- deriv( ~a*(1-exp(-b*t))^c, c("a", "b", "c"), function(t, a, b, c){} )


# Richards model with 4 parameters

Rich_mod.b <- deriv( ~a*(1+b*exp(-c*t))^d, c("a", "b", "c", "d"), function(t, a, b, c, d){} )


# Richards model with 4 parameters

Rich_mod <- deriv( ~a*((1-b*exp(-c*t))^(1/(1-d))), c("a", "b", "c", "d"), function(t, a, b, c, d){} )

# Richards model with 4 parameters

Rich_mod.c <- deriv( ~y0+a*(1-exp(-b*t))^c, c("a", "b", "c", "y0"), function(t, a, b, c, y0){} )



# Sigmoidal Growth Model --------------------------------------------------


# Sigmoidal model with 3 parameters

Sig_mod <- deriv( ~a/(1+exp(-((t-c)/b))), c("a", "b", "c"), function(t, a, b, c){} )







# Korf Growth Model -------------------------------------------------------


# Korf model

Korf_mod <- deriv( ~a*(exp(-(b/t^c))), c("a", "b", "c"), function(t, a, b, c){} )







# Model Predictive Performance ------------------------------------


sesgo <- function(mod,data){
  
  newdat <- data.frame(EDAD=data$EDAD)
  
  y.obs <- data$HC_D
  
  y.pred <- predict(mod,newdat,type="response", level=0)
  
  b <- mean(y.obs-y.pred)
  
  return(b)
  
}



rcme <- function(mod,data){
  
  newdat <- data.frame(EDAD=data$EDAD)
  
  y.obs <- data$HC_D
  
  y.pred <- predict(mod,newdat,type="response", level=0)
  
  rmse <- sqrt(mean((y.obs-y.pred)^2))
  
  return(rmse)
  
}



EF <- function(mod,data){
  
  newdat <- data.frame(EDAD=data$EDAD)
  
  y.obs <- data$HC_D
  
  y.pred <- predict(mod,newdat,type="response", level=0)
  
  y.media <- mean(y.obs)
  
  num <- sum((y.obs-y.pred)^2)
  
  den <- sum((y.obs-y.media)^2)
  
  ef <- (1-(num/den))*100
  
  return(ef)
  
}



