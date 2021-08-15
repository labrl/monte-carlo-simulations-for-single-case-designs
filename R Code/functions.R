set.seed(48151)
create_time_series = function(n, a, ct){
  time_series = c()
  point1 = rnorm(1)
  time_series = c(time_series, point1)
  for (i in 2:n){
    point = a*time_series[i-1]+rnorm(1)
    time_series = c(time_series, point)
  }
  time_series = time_series + ct
  return(time_series)
}

create_AB_data <- function(time_series, nb_pointsA, nb_pointsB, smd){
  total_points = nb_pointsA + nb_pointsB
  PhaseA = time_series[1:nb_pointsA]
  PhaseB = time_series[(nb_pointsA+1):total_points] + smd 
  labels = c(rep('A', nb_pointsA), rep('B', nb_pointsB))
  AB_data = list(labels, c(PhaseA, PhaseB))
  return(AB_data)
}

add_trend <- function(AB_data, tr){
  middle_point = median(1:length(AB_data[[2]]))
  for (i in 1:length(AB_data[[2]])){
    distance = i - middle_point
    AB_data[[2]][i] = AB_data[[2]][i] + distance*tan(tr*pi/180)
  }
  return (AB_data)
}

ABgraph <- function (AB_data){
  xlab = "Measurement Times"
  ylab = "Behavior"
  x <- 1:length(AB_data[[2]])
  plot(x, AB_data[[2]], xlab = xlab, ylab = ylab, 
       ylim = c(0, max(AB_data[[2]])*1.2), 
       xlim = c(0, length(AB_data[[2]])),
       pch = 16, axes = FALSE)
  lines(x[AB_data[[1]] == "A"], AB_data[[2]][AB_data[[1]] == "A"])
  lines(x[AB_data[[1]] == "B"], AB_data[[2]][AB_data[[1]] == "B"])
  lines(c(sum(AB_data[[1]] == "A") + 0.5, sum(AB_data[[1]] == 
       "A") + 0.5), c(0, max(AB_data[[2]])*1.2),  lty = 2)
  axis(1, tck=-0.01)
  axis(2, tck=-0.01, label = NA)
  mtext('Phase A', side = 3, at = (sum(AB_data[[1]] == "A") + 
                                     1)/2)
  mtext('Phase B', side = 3, at = (sum(AB_data[[1]] == "A") + 
                                     (sum(AB_data[[1]] == "B") + 1)/2))
}

Fisheretal = c(NA,NA,3,4,5,6,6,7,8,8,9,9,10,11,12,12,12,13,13,13,14,14,15)
CDC_method <- function(AB_data){
  idx_A = which(AB_data[[1]]=='A')
  idx_B = which(AB_data[[1]]=='B')
  mean_line = mean(AB_data[[2]][idx_A]) + 0.25*sd(AB_data[[2]][idx_A])
  lr_A = lm(AB_data[[2]][idx_A] ~ idx_A)
  trend_line = idx_B*lr_A$coefficients[2]+lr_A$coefficients[1] + 
    0.25*sd(AB_data[[2]][idx_A])
  count= sum(AB_data[[2]][idx_B] > trend_line & AB_data[[2]][idx_B] > mean_line)
  if (count >= Fisheretal[length(idx_B)]){
    return (1)
  } else { 
    return (0)
  }
}
