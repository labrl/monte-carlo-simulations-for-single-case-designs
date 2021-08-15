#Conducting Monte Carlo Simulations to Generate and Analyze Single-Case Graphs 

#To make sure that you get the same results, set random seed: 
set.seed(48151)

#This function creates a time series with n points, an autocorrelation of a,
#and a constant of ct
create_time_series <- function(n, a, ct){
  
  #Create an empty vector to hold values
  time_series = c()
  
  #Compute first point (no autocorrelation possible)
  point1 = rnorm(1)
  
  #Add point1 to the time series
  time_series = c(time_series, point1)
  
  #Repeat the process below for all subsequent points
  for (i in 2:n){
    
    #Compute autocorrelated point
    point = a*time_series[i-1] + rnorm(1)
    
    #Add autocorrelated point to time series
    time_series = c(time_series, point)
  }

  #Add constant to all points
  time_series = time_series + ct
  
  #Return the time series
  return(time_series)
}

#This function creates data for an AB series with nb_pointsA in Phase A, 
#nb_pointsB in Phase B, and a standardized mean difference of smd 
create_AB_data <- function(time_series, nb_pointsA, nb_pointsB, smd){
  
  #Compute total number of points
  total_points = nb_pointsA + nb_pointsB
  
  #Extract Phase A values
  PhaseA = time_series[1:nb_pointsA]
  
  #Extract and add smd to values of Phase B
  PhaseB = time_series[(nb_pointsA+1):total_points] + smd 
  
  #Create labels 
  labels = c(rep('A', nb_pointsA), rep('B', nb_pointsB))
  
  #Combine labels and values in same list
  AB_data = list(labels, c(PhaseA, PhaseB))
  
  #Return AB data
  return(AB_data)
}

#Function to add trend of tr degrees to AB series (optional)

add_trend <- function(AB_data, tr){
 
  #Identify middle point around which to pivot trend
  middle_point = median(1:length(AB_data[[2]]))
  
  #Apply trend to all points
  for (i in 1:length(AB_data[[2]])){
    
    #Compute distance to middle point of each point
    distance = i - middle_point
    
    #Add trend to each point using trigonometry (tangent of radians)
    AB_data[[2]][i] = AB_data[[2]][i] + distance*tan(tr*pi/180)
  }
  
  #Return trended AB_data
  return (AB_data)
}


#Function to produce AB graph

#Adapted from Bulte, I., & Onghena, P. (2012). When the truth hits you between 
#the eyes: A software tool for the visual analysis of single-case experimental 
#data. Methodology, 8, 104-114.

ABgraph <- function (AB_data){
  
  #Set axes titles
  xlab = "Measurement Times"
  ylab = "Behavior"
  
  #Session values
  x <- 1:length(AB_data[[2]])
  
  #Plot points
  plot(x, AB_data[[2]], xlab = xlab, ylab = ylab, 
                          ylim = c(0, max(AB_data[[2]])*1.2), 
                          xlim = c(0, length(AB_data[[2]])),
                          pch = 16, axes = FALSE)
  
  #Connect points in Phase A
  lines(x[AB_data[[1]] == "A"], AB_data[[2]][AB_data[[1]] == "A"])
  
  #Connect point in Phase B
  lines(x[AB_data[[1]] == "B"], AB_data[[2]][AB_data[[1]] == "B"])
  
  #Draw phase change line
  lines(c(sum(AB_data[[1]] == "A") + 0.5, sum(AB_data[[1]] == 
                          "A") + 0.5), c(0, max(AB_data[[2]])*1.2), 
                          lty = 2)

  #Add x axis 
  axis(1, tck=-0.01)
  
  #Add y axis
  axis(2, tck=-0.01, label = NA)
  
  #Add label for Phase A
  mtext('Phase A', side = 3, at = (sum(AB_data[[1]] == "A") + 
                                     1)/2)
  
  #Add label for Phase B
  mtext('Phase B', side = 3, at = (sum(AB_data[[1]] == "A") + 
                                     (sum(AB_data[[1]] == "B") + 1)/2))
}

#Apply the CDC method

#List of cutoff values from Fisher et al. (2003)
Fisheretal = c(NA,NA,3,4,5,6,6,7,8,8,9,9,10,11,12,12,12,13,13,13,14,14,15)

#Function to apply the CDC
CDC_method <- function(AB_data){
  
  #Index of points for each phase
  idx_A = which(AB_data[[1]]=='A')
  idx_B = which(AB_data[[1]]=='B')
  
  #Extract mean of phase A and increase by 0.25 standard deviation 
  mean_line = mean(AB_data[[2]][idx_A]) + 0.25*sd(AB_data[[2]][idx_A])

  #Run linear regression for Phase A
  lr_A = lm(AB_data[[2]][idx_A] ~ idx_A)
  
  #Project line to Phase B and add 0.25 standard deviation
  trend_line = idx_B*lr_A$coefficients[2]+lr_A$coefficients[1] + 
    0.25*sd(AB_data[[2]][idx_A])
  
  #Count number of points falling above both lines
  count= sum(AB_data[[2]][idx_B] > trend_line & AB_data[[2]][idx_B] > mean_line)
  
  #Compare to Fisher et al. 
  if (count >= Fisheretal[length(idx_B)]){
    
    #Return 1 if clear change (i.e., count equal or higher than cutoff)
    return (1)
  } 
  else { 
    
    #Return 0 if clear change (i.e., count lower than cutoff)
    return (0)
  }
}


