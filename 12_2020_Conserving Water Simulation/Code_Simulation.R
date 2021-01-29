
# Task 1 -------------------------------------------------------------------

Ysim <- function(p, m, la, n) {
  # simulate monthly rainfall
  A <- rbinom(n, 1, p) #simulate A with rbinom
  B <- rgamma(n, shape = m, scale = 1/la) #simulate B with rgamma
  X <- A * B
  return(sum(X)) #Y = sum(A*B)
}


F_Y <- function(x, p, m, la, n) {
  # cdf of monthly rainfall
  P_N0 <- dbinom(0, size = n, prob = p) #simulate Pr(N = 0)
  P_C_x <- pgamma(x, shape = (1:n)*m, scale = 1/la) #simulate P(C<x)
  P_Nk <- dbinom(1:n, size = n, prob = p) #simulate Pr(N = k)
  return(P_N0 + sum(P_C_x* P_Nk))
}

p = 0.27; m = 0.24; la = 0.043; n =31
k = 10000
Y_simulations <- replicate(k, Ysim(p,m,la,n))

bins <- seq(0,max(Y_simulations), by = 20)
Fy <- sapply(bins, function(X) F_Y(X,p, m, la, n))

mu <- k * (Fy[-1] - Fy[-length(bins)])
s <- k * (Fy[-1] - Fy[-length(bins)]) * (1 - Fy[-1] + Fy[-length(bins)])

CI_Fy_min = mu - qnorm(0.975)*s/sqrt(k)
CI_Fy_max = mu + qnorm(0.975)*s/sqrt(k)


#Plotting histogram
hist(Y_simulations, xlab = "", main = "") 

#Adding points
points(x = (bins[-1] + bins[-length(bins)])/2,
       y = mu, col = "red", pch=19)
#Adding CI bars
arrows((bins[-1] + bins[-length(bins)])/2, CI_Fy_min,
       (bins[-1] + bins[-length(bins)])/2, CI_Fy_max,
       length=0)



# Task 2 -------------------------------------------------------------------

Melbourne_rainfall_mm <- read.csv("Melbourne_rainfall_mm-1.csv")

days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

p_hat <- Melbourne_rainfall_mm$mean_days_rain / days_in_month #Estimation of p_i
p_hat

Y_hat <- Melbourne_rainfall_mm$mean_rainfall #Estimation of rainfall
m_hat_la <- function(la, M) la * Y_hat[M] / (days_in_month[M] * p_hat[M]) #m_i in terms of lambda_i


#Writing F_Y in function of lambda
F_y_la <- function(x,la, M) {
  F_Y(x, p = p_hat[M], m = m_hat_la(la, M), la = la, n = days_in_month[M])
}

#Writing Loss function in terms of lambda_i
Loss <- function(la, M) {
  d1 <- Melbourne_rainfall_mm$decile_1_rainfall[M]
  d5 <- Melbourne_rainfall_mm$median_rainfall[M]
  d9 <- Melbourne_rainfall_mm$decile_9_rainfall[M]
  L = ((F_y_la(d1, la = la, M) - 0.1)^2 / 0.09) + 
    ((F_y_la(d5, la, M) - 0.5)^2 / 0.25) + 
    ((F_y_la(d9, la, M) - 0.9)^2 / 0.09)
  
  return(L)
}

#Estimation of lambda_i by minimizing Loss function
lam_hat <- sapply(1:12, function(M) optimise(function(l) Loss(la = l, M), interval = c(0,1))$minimum)
lam_hat

m_hat <- lam_hat * Y_hat / (days_in_month * p_hat)
m_hat

pars_estimate <- data.frame(Month = Melbourne_rainfall_mm$Month,
                            n_days = days_in_month,
                            p_hat = round(p_hat, 3),
                            m_hat = round(m_hat, 3),
                            lam_hat = round(lam_hat, 3))

#Parameters estimates for the first month
pars_estimate[1,]



# Task 3 -------------------------------------------------------------------

tanksim <- function(num_years, maxraintank, maxgreytank, phat, mhat, lahat, plotflag = F) {
  # Daily simulation of domestic rainwater and greywater tanks
  #
  # num_years is length of simulation in years
  # maxraintank and maxgreytank are max capacity of each tank (in litres)
  # phat, mhat and lahat are parameters for the daily rainfall sim, one for each month
  #
  # Returns volume of water saved (in litres) for each year of the simulation
  # If plotflag is TRUE then the levels of each tank at the end of each day are plotted
  Xsim <- function(month) {
    # simulate rainfall for a day in given month
    rbinom(1, 1, phat[month])*rgamma(1, mhat[month], lahat[month])
  }
  # Constants
  days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  mean_max_temp <- c(25.9, 25.8, 23.9, 20.3, 16.7, 14.1, 13.5, 15.0, 17.2, 19.7, 22.0, 24.2)
  roofarea <- 100 # m^2
  gardenarea <- 200 # m^2
  flushsize <- 5 # litres per flush
  numflush <- function() rbinom(1, 15, .8) # flushes per day for four people, at home half the day
  showersize <- 35 # in litres
  numshower <- 4 # showers per day for four people
  washsize <- 35 # litres per load
  numwash <- function() rbinom(1, 8, .125) # washes per day for four people
  
  #initialization of counters
  rain_tank = 0; rain_tank_rec <- c()
  grey_tank = 0; grey_tank_rec <- c()
  garden_rain_depth <- c() #tracking water depth in garden
  savings = 0
  for (year in 1:num_years)
    for(M in 1:12) {
      depth_garden_mm = mean_max_temp[M] / 15 #depth required for each three days period 
      for(j in 1:days_in_month[M]) {
        
        #Collected water
        
        #rainfall simulation
        rainfall_sim <- Xsim(month = M)
        rain_tank <- rain_tank + roofarea * rainfall_sim
        if (rain_tank > maxraintank) #Check the volume is below the capacity
          rain_tank <- maxraintank
        
        #greywater simulation
        greywater_sim <- numshower*showersize + numwash()*washsize
        grey_tank <- grey_tank + greywater_sim
        if (grey_tank > maxgreytank)
          grey_tank <- maxgreytank
        
        
        #Used water
        
        #Flush toilets simulation and update tanks
        flush <- flushsize * numflush()
        if(grey_tank >= flush){
          grey_tank = grey_tank - flush
          flush = c(flush, 0)
        } else {
          flush = c(flush, flush-grey_tank)
          grey_tank = 0
        }
        
        if(rain_tank >= flush[2]){
          rain_tank = rain_tank - flush[2]
          flush = c(flush, 0)
        } else {
          flush = c(flush, flush-rain_tank)
          rain_tank = 0
        }
        
        #Record saved water
        savings <- savings + flush[1] - flush[length(flush)]
        
        
        #Calculate the water depth needed in the garden
        dificit_depth_garden <- sum(tail(garden_rain_depth,2) + rainfall_sim) - 3 * depth_garden_mm
        
        if(dificit_depth_garden < 0) {
          garden <- abs(dificit_depth_garden) * gardenarea
          garden_rain_depth = c(garden_rain_depth, abs(dificit_depth_garden))
        } else {
          garden <- 0
          garden_rain_depth = c(garden_rain_depth, rainfall_sim)
        }
        
        #update tanks
        if(rain_tank >= garden){
          rain_tank = rain_tank - garden
          garden = c(garden, 0)
        } else {
          garden = c(garden, garden-rain_tank)
          rain_tank = 0
        }
        
        if(grey_tank >= garden[2]){
          grey_tank = grey_tank - garden[2]
          garden = c(garden, 0)
        } else {
          garden = c(garden, garden-grey_tank)
          grey_tank = 0
        }
        
        #record saved water
        savings <- savings + garden[1] - garden[length(garden)]
        
        
        #track volumes in tanks
        rain_tank_rec <- c(rain_tank_rec, rain_tank)
        grey_tank_rec <- c(grey_tank_rec, grey_tank)
      }
    }
  
  #Plotting levels of tanks if plotflag argument set to TRUE
  if(plotflag) {
    plot(rain_tank_rec, type = "l", col = "blue",
         xlab = "Days", ylab = "Litres")
    lines(grey_tank_rec, type = "l", col = "red")
  }
  
  #return total water saved during the period of simulation
  return(savings)
  
}


tanksim(1, 3000, 1000, p_hat, m_hat, lam_hat, T)