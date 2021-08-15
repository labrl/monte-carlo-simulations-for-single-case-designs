#Conducting Monte Carlo Simulations to Generate and Analyze Single-Case Graphs

#Function to create time series
create_time_series <- function(n, a, ct){
  
  #Create an empty vector to hold values
  time_series = c()
  
  #Compute first point (no autocorrelation possible)
  point1 = rnorm(1)
  
  #Add point1 to time series
  time_series = c(time_series, point1)
  
  #Repeat the process below for all subsequent points
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


#This function creates data for an ABAB graph with an autocorrelation of a, a 
#trend of tr (in degrees), a constant of ct, nb_pointsA1 and nbpointsA2 in the 
#first and second Phase A, nb_pointsB1 and nb_pointsB2 in the first and second 
#Phase B, and a standardized mean difference of smd 

create_ABAB_data <- function(a, tr, ct, nb_pointsA1, nb_pointsB1,
                             nb_pointsA2, nb_pointsB2, smd){
  
  #Calculate total number of points in series
  total_points = nb_pointsA1+nb_pointsB1+nb_pointsA2+nb_pointsB2
  
  #Create time series 
  time_series = create_time_series(total_points, a, ct)
  
  #Extract the values for the first Phase A
  PhaseA1 = time_series[1:nb_pointsA1]
  
  #Extract the values and add smd for the first phase of Phase B
  PhaseB1 = time_series[(nb_pointsA1+1):(nb_pointsA1+nb_pointsB1)] + smd 
  
  #Extract the values for the second Phase A
  PhaseA2 = time_series[(nb_pointsA1+nb_pointsB1+1):(nb_pointsA1+nb_pointsB1+
                                                       nb_pointsA2)]
  
  #Extract the values and add smd for the second phase of Phase B
  PhaseB2 = time_series[(nb_pointsA1+nb_pointsB1+nb_pointsA2+1):
                    (nb_pointsA1+nb_pointsB1+nb_pointsA2+nb_pointsB2)] + smd   
  
  #Combine all values in a single vector
  all_values = c(PhaseA1, PhaseB1, PhaseA2, PhaseB2)
  
  #Identify middle point around which to pivot trend
  middle_point = median(1:length(all_values))
  
  #Apply trend to all points
  for (i in 1:length(all_values)){
    
    #Compute distance to middle point of each point
    distance = i - middle_point
    
    #Add trend to each point using trigonometry (tangent of radians)
    all_values[i] = all_values[i] + distance*tan(tr*pi/180)
  }
  
  #Create labels
  labels = c(rep('A1', nb_pointsA1), rep('B1', nb_pointsB1), 
             rep('A2', nb_pointsA2), rep('B2', nb_pointsB2))
  
  #Combine labels and values in the same list
  ABAB_data = list(labels, all_values)
  
  #Return ABAB data
  return(ABAB_data)
}


#Function to produce ABAB graph

#Adapted from Bulte, I., & Onghena, P. (2012). When the truth hits you between 
#the eyes: A software tool for the visual analysis of single-case experimental 
#data. Methodology, 8, 104-114.

ABABgraph <- function (ABAB_data){
  
  #Initialize graph
  par(mfrow = c(1,1))
  
  #Set axes titles
  xlab = "Measurement Times"
  ylab = "Behavior"
  
  #Session values
  x <- 1:length(ABAB_data[[2]])
  
  #Plot points
  plot(x, ABAB_data[[2]], xlab = xlab, ylab = ylab, 
                          ylim = c(0, max(as.numeric(ABAB_data[[2]]))*1.2), 
                          xlim = c(0, length(ABAB_data[[2]])),
                          pch = 16, axes = FALSE)
  
  #Connect points in Phase A1
  lines(x[ABAB_data[[1]] == "A1"], ABAB_data[[2]][ABAB_data[[1]] == "A1"])
  
  #Connect points in Phase B1
  lines(x[ABAB_data[[1]] == "B1"], ABAB_data[[2]][ABAB_data[[1]] == "B1"])
  
  #Connect points in Phase A2
  lines(x[ABAB_data[[1]] == "A2"], ABAB_data[[2]][ABAB_data[[1]] == "A2"])
  
  #Connect point in Phase B2
  lines(x[ABAB_data[[1]] == "B2"], ABAB_data[[2]][ABAB_data[[1]] == "B2"])
  
  #Draw first phase change line 
  lines(c(sum(ABAB_data[[1]] == "A1") + 0.5, sum(ABAB_data[[1]] == "A1") + 0.5), 
        c(0, max(ABAB_data[[2]])*1.2), lty = 2)

  #Draw second phase change line 
  lines(c(sum(ABAB_data[[1]] == "B1") + sum(ABAB_data[[1]] == "A1")+ 0.5, 
          sum(ABAB_data[[1]] == "B1") + sum(ABAB_data[[1]] == "A1")+ 0.5), 
          c(0, max(ABAB_data[[2]])*1.2), lty = 2)

  #Draw third phase change line 
  lines(c(sum(ABAB_data[[1]] == "A2") + sum(ABAB_data[[1]] == "B1") + 
        sum(ABAB_data[[1]] == "A1")+ 0.5, sum(ABAB_data[[1]] == "A2") + 
        sum(ABAB_data[[1]] == "B1") + sum(ABAB_data[[1]] == "A1")+ 0.5), 
        c(0, max(ABAB_data[[2]])*1.2), lty = 2)
  
  #Add x axis 
  axis(1, tck=-0.01)
  
  #Add y axis
  axis(2, tck=-0.01, label = NA)
  
  #Add label for Phase A1
  mtext('Phase A', side = 3, at = (sum(ABAB_data[[1]] == "A1")/2))
  
  #Add label for Phase B1
  mtext('Phase B', side = 3, at = ((sum(ABAB_data[[1]] == "A1")*2 + 1 +
                                     sum(ABAB_data[[1]] == "B1"))/2))
  
  #Add label for Phase A2
  mtext('Phase A', side = 3, at = ((sum(ABAB_data[[1]] == "A1")*2 +
          sum(ABAB_data[[1]] == "B1")*2 + sum(ABAB_data[[1]] == "A2")+1)/2))
  
  #Add label for Phase B2
  mtext('Phase B', side = 3, at = ((sum(ABAB_data[[1]] == "A1")*2 +
      sum(ABAB_data[[1]] == "B1")*2 + sum(ABAB_data[[1]] == "A2")*2 +
      sum(ABAB_data[[1]] == "B2")+1)/2))
}

#To test function, remove the hashtags from the two lines below
#ABAB_data = create_ABAB_data(0.2, 30, 10, 5, 6, 7, 8, 10)
#ABABgraph(ABAB_data)
