#Conducting Monte Carlo Simulations to Generate and Analyze Single-Case Graphs 

#Function to create time series
create_time_series <- function(n, a, ct){
  
  #Create an empty vector to hold values
  time_series = c()
  
  #Compute first point (no autocorrelation possible)
  point1 = rnorm(1)
  
  #Add point1 to time series
  time_series = c(time_series, point1)
  
  #Repeat the process below for all subsequent point
  for (i in 2:n){
    
    #Compute autocorrelated point
    point = rnorm(1)+a*time_series[i-1]
    
    #Add autocorrelated point to time series
    time_series = c(time_series, point)
  }
  
  #Add constant to all points
  time_series = time_series + ct
  
  #Return the time series
  return(time_series)
}

#This function creates data for an alternating treatment graph with an 
#autocorrelation of a, a trend of tr (in degrees), a constant of ct, a minimum 
#of nb_points in each condition, and a standardized mean difference of smd. The 
#alternation scheme can be 'systematic', 'semi-random', or 'random' 

create_AT_data <- function(a, tr, ct, nb_points, smd, alternation){
  
  #If the alternation is systematic (e.g., ABABABABAB)
  if (alternation == 'systematic'){
    
    #Create labels
    labels = c(rep(c('A', 'B'), nb_points))
  }
  
  #If the alternation is semi-random (in blocks of 2)
  if (alternation == 'semi-random'){
    
    #Empty labels 
    labels = c()
    
    #Repeat process for each pair of points
    for (i in 1:nb_points){
      
      #Randomly select the order of the two conditions 
      conditions = sample(c('A','B'), 2, replace = FALSE)
      
      #Add conditions to labels
      labels = c(labels, conditions[1], conditions[2])
    }
  }
    
  #If the alternation is completely random 
  if (alternation == 'random'){
    
    #Empty labels 
    labels = c()
    
    #Run code until minimum number of points is reached for both Phases
    while ((sum(labels == 'A') < nb_points) | (sum(labels == 'B') < nb_points)){
      
      #Randomly select one condition 
      condition = sample(c('A', 'B'), 1)
      
      #Add to labels
      labels = c(labels, condition)
    }
  }
  
  #Create time series 
  time_series = create_time_series(length(labels), a, ct) 
  
  #Indices for values of B
  idxB = which(labels == 'B')
  
  #Add smd to values
  all_values = time_series
  all_values[idxB] = all_values[idxB] + smd

  #Identify middle point around which to pivot trend
  middle_point = median(1:length(all_values))
  
  #Apply trend to all points
  for (i in 1:length(all_values)){
    
    #Compute distance to middle point of each point
    distance = i - middle_point
    
    #Add trend to each point using trigonometry (tangent of radians)
    all_values[i] = all_values[i] + distance*tan(tr*pi/180)
  }
  
  #Combine abels and values in the same list
  AT_data = list(labels, all_values)
  
  #Return alternating-treatment data
  return(AT_data)
}

#Function to produce alternating-treatment graph

ATgraph <- function (AT_data){
  
  #Initialize graph
  par(mfrow = c(1,1))
  
  #Axis labels
  xlab = "Measurement Times"
  ylab = "Scores"

  #Identify points for conditions A and B
  idxA = which(AT_data[[1]] == 'A')
  idxB = which(AT_data[[1]] == 'B')
  
  #Plot points for Phase A
  plot(idxA, AT_data[[2]][idxA], xlab = xlab, ylab = ylab, type = 'o',
                          ylim = c(0, max(AT_data[[2]])*1.2), 
                          xlim = c(0, length(AT_data[[2]])), pch = 15,
                          axes = FALSE)
  
  #Plot points for Phase B 
  lines(idxB, AT_data[[2]][idxB], type = 'o', pch = 16)
  
  #Add x axis 
  axis(1, tck=-0.01)
  
  #Add y axis
  axis(2, tck=-0.01, label = NA)
  
  #Add legend
  legend('bottomright', legend = c("Condition A", "Condition B"), pch =c(15,16), 
         lty = 1)
  
}

#To test function, remove the hashtags from the two lines below
#AT_data = create_AT_data(0.1, 30, 10, 5, 10, 'semi-random')
#ATgraph(AT_data)
