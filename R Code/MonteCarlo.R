source('functions.R')

nb_pointsA_list = c(3,5)
nb_pointsB_list = c(5,10)
a_list = c(0, 0.2, 0.4)
tr_list = c(0, 15, 30)
smd_list = c(0,0,0,1,2,3)
ct = 10

all_AB_data = c()
true_values = c()
trend_values = c()
for (nb_pointsA in nb_pointsA_list){
  for (nb_pointsB in nb_pointsB_list){
    for (a in a_list){
      for (tr in tr_list){
        for (smd in smd_list){
          time_series = create_time_series(nb_pointsA+nb_pointsB, a, ct)
          AB_data = create_AB_data(time_series, nb_pointsA, nb_pointsB, smd)
          AB_data = add_trend(AB_data, tr)
          all_AB_data = c(all_AB_data, AB_data)
          if (smd == 0){
            true_values = c(true_values, 0)
          } else {
            true_values = c(true_values, 1)
          }
          trend_values = c(trend_values, tr)
        }
      }
    }
  }
}

pdf("ABgraphs.pdf", paper="USr", height = 300, width=400)
for (i in seq(1,length(all_AB_data), 2)){
  ABgraph(all_AB_data[i:(i+1)])
}
dev.off()

cdc_results = c()
for (i in seq(1,length(all_AB_data), 2)){
  cdc_results = c(cdc_results, CDC_method(all_AB_data[i:(i+1)]))
}

agreement_CDC = sum(cdc_results == true_values)/length(true_values)
idx_0 = which(true_values == 0)
typeI_error_CDC = sum(cdc_results[idx_0])/length(idx_0)
idx_1 = which(true_values == 1)
power_CDC = sum(cdc_results[idx_1])/length(idx_1)

expert_data = read.csv('Expert_data.csv', header = FALSE)[1]
agreement_VI = sum(expert_data[1] == true_values)/length(true_values)
typeI_error_VI = sum(expert_data[idx_0,1])/length(idx_0)
power_VI = sum(expert_data[idx_1,1])/length(idx_1)

error_by_trend = c()
for (tr in tr_list){
  idx = intersect(which(trend_values == tr), which(true_values == 0))
  error_by_trend = c(error_by_trend, sum(cdc_results[idx])/length(idx)) 
}
names(error_by_trend) = tr_list

power_by_trend = c()
for (tr in tr_list){
  idx = intersect(which(trend_values == tr), which(true_values == 1))
  power_by_trend = c(power_by_trend, sum(cdc_results[idx])/length(idx)) 
}
names(power_by_trend) = tr_list
