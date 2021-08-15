#Conducting Monte Carlo Simulations to Generate and Analyze Single-Case Graphs

#This function creates a time series with n points, an autocorrelation of a,
#and a constant of c
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

#This function creates data for a multiple baseline graphs with an 
#autocorrelation of a, a trend of tr (in degrees), a constant of ct, a minimum 
#of nb_pointsA in Phase A, a minimum of nb_pointsB in Phase B, stagger each 
#tiers by stagger_points, with nb_tiers of tiers, and a standardized mean 
#difference of smd

create_MB_data <- function(a, tr, ct, nb_pointsA, nb_pointsB,
                             stagger_points, nb_tiers, smd){

  #Create empty labels and values vectors
  labels = c()
  all_values = c()
  
  #Compute total number of points per tier
  total_points = nb_pointsA + (nb_tiers-1)*stagger_points + nb_pointsB
  
  #Repeat for each tier
  for (tier in 1:nb_tiers){
    
    #Create time series
    time_series = create_time_series(total_points, a, ct)
    
    #Number of points per phase for tier
    nb_pointsA_tier = nb_pointsA + (tier-1)*stagger_points
    nb_pointsB_tier = total_points - nb_pointsA_tier
    
    #Extract the values for Phase A
    PhaseA = time_series[1:nb_pointsA_tier]
    
    #Extract values and add smd to values for Phase B
    PhaseB = time_series[(nb_pointsA_tier+1):(nb_pointsA_tier+
                                                nb_pointsB_tier)] + smd 
    
    #Combine tier values in a single vector
    tier_values = c(PhaseA, PhaseB)
    
    #Identify middle point around which to pivot trend
    middle_point = median(1:length(tier_values))
    
    #Apply trend to all tier points
    for (i in 1:length(tier_values)){
      
      #Compute distance to middle point of each point
      distance = i - middle_point
      
      #Add trend to each point using trigonometry (tangent of radians)
      tier_values[i] = tier_values[i] + distance*tan(tr*pi/180)
    }
    
    #Produce labels for tier
    tier_labels = c(rep(paste0('A',tier), nb_pointsA_tier), 
                    rep(paste0('B',tier), nb_pointsB_tier))
    
    #Add labels for tier to the data series
    labels = c(labels, tier_labels)
    
    #Add values for tier to the data series
    all_values = c(all_values, tier_values)
    
  }
  
  #Combine labels and values in list
  MB_data = list(labels, all_values)
  
  #Return multiple baseline data
  return(MB_data)
}


#Function to produce multiple baseline graph

MBgraph <- function (MB_data){
  
  #Set axes titles
  xlab = "Measurement Times"
  ylab = "Behavior"
  
  #Extract number of tiers
  nb_tiers = max(as.numeric(gsub('\\D', '', MB_data[[1]])))
  
  #Extract points per tiers and x axis values
  total_points = length(MB_data[[2]])/nb_tiers
  x = 1:total_points
  
  #Create graphs in single column
  par(mfrow=c(nb_tiers,1))
  
  #Repeat for each tiers
  for (tier in 1:nb_tiers){
    
    #Identify indices of values for Phases A and B for this tier
    idxA_tier = which(MB_data[[1]] == paste0('A', tier))
    idxB_tier = which(MB_data[[1]] == paste0('B', tier))
    
    #Extract data for tier 
    MB_tier = list(MB_data[[1]][c(idxA_tier, idxB_tier)],
                   MB_data[[2]][c(idxA_tier, idxB_tier)])
    
    #Plot points
    plot(x, MB_tier[[2]], xlab = "", ylab = ylab,
                            ylim = c(0, max(as.numeric(MB_data[[2]]))*1.2), 
                            xlim = c(0, total_points),
                            pch = 16, axes = FALSE)
    
    #Connect points in Phase A
    lines(x[MB_tier[[1]] == paste0('A', tier)], 
          MB_tier[[2]][MB_tier[[1]] == paste0('A', tier)])
    
    #Connect point in Phase B
    lines(x[MB_tier[[1]] == paste0('B', tier)], 
          MB_tier[[2]][MB_tier[[1]] == paste0('B', tier)])
    
    #Draw phase change line 
    lines(c(sum(MB_tier[[1]] == paste0('A', tier)) + 0.5, 
            sum(MB_tier[[1]] == paste0('A', tier)) + 0.5), 
            c(0, max(MB_data[[2]])*1.2), lty = 2)
  
    #Add x axis 
    axis(1, tck=-0.01)
    
    #Add y axis
    axis(2, tck=-0.01, label = NA)
    
    #If first tier, add labels at the top
    if (tier == 1){
    
      #Add label for Phase A
      mtext('Phase A', side = 3, at = (sum(MB_data[[1]] == "A1")/2))
      
      #Add label for Phase B
      mtext('Phase B', side = 3, at = ((sum(MB_data[[1]] == "A1")*2 + 1 +
                                            sum(MB_data[[1]] == "B1"))/2))
    }
  }
  #Add label to x axis
  title(xlab = xlab)
}

#To test function, remove the hashtags from the two lines below
#MB_data = create_MB_data(0.2, 15, 10, 5, 9, 3, 3, 5)
#MBgraph(MB_data)
