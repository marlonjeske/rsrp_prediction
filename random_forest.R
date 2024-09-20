library(tidyverse)
library(caret)
library(randomForest)
library(Metrics)


dados <- read_csv("features.csv")

######################### TRAIN AND TEST DATASET ###############################

##Split train and test data in 80/20
train_index<-createDataPartition(dados$rssi, p=0.80, list=FALSE) 
train_index

treino <- dados[train_index,]
teste <- dados[-train_index,] 

dim(treino)
dim(teste)

features_treino <- treino %>% dplyr::select(-rssi)
target_treino <- treino$rssi

features_teste <- teste %>% dplyr::select(-rssi)
target_teste <- teste$rssi


############################ TUNING ############################ 

##Tuning parameters mtry and ntree
set.seed(58427)

bestmtry <- tuneRF(features_treino, target_treino, mtryStart = 4, ntreeTry = 100,
                   stepFactor = 4, trace = T, plot = T)


############################ RANDOM FOREST #####################################

##Train Random Forest for all features (include in function importance = T)
fit_rf <- randomForest(x = features_treino, y = target_treino, mtry = 16, ntree = 300, type = "regression", importance = T)


######### Feature importance ##########

summary(fit_rf)
varImpPlot(fit_rf)
fit_rf$importance

a <- as.data.frame(fit_rf$importance)
a <- a %>% arrange(-`%IncMSE`)
a


############################ RANDOM FOREST - TOP FEATURES #####################################


#Only coordinates and tech params
features_treino_top6 <- features_treino %>% select(downtilt, azimuth, tx_effective_height, tx_height, terrain_mean, tx_power)


#coordinates and tech params + terrain and los
features_treino_top10 <- features_treino %>% select(downtilt, azimuth, tx_effective_height, tx_height, terrain_mean, tx_power,
                                                    diff_tx_effective_height_max_terrain, tx_elevation, 
                                                    diff_tx_rx_effective_height, diff_tx_effective_height_max_clutter)
  
#coordinates and tech params + terrain and los  
features_treino_top15 <- features_treino %>% select(downtilt, azimuth, tx_effective_height, tx_height, terrain_mean, tx_power,
                                                    diff_tx_effective_height_max_terrain, tx_elevation, 
                                                    diff_tx_rx_effective_height, diff_tx_effective_height_max_clutter,
                                                    clutter_max, angle_difference_tx_rx, los_distance, fresnel_radii,
                                                    obstruction_fresnel)

#coordinates and tech params + terrain + los + Fresnel features  
features_treino_top20 <- features_treino %>% select(downtilt, azimuth, tx_effective_height, tx_height, terrain_mean, tx_power,
                                                    diff_tx_effective_height_max_terrain, tx_elevation, 
                                                    diff_tx_rx_effective_height, diff_tx_effective_height_max_clutter,
                                                    clutter_max, angle_difference_tx_rx, los_distance, fresnel_radii,
                                                    obstruction_fresnel, clutter_sd, diff_rx_effective_height_max_clutter,
                                                    distance, distance_tx_first_obs_terrain, clutter_mean)


#coordinates and tech params + terrain + los + Fresnel features  
features_treino_top25 <- features_treino %>% select(downtilt, azimuth, tx_effective_height, tx_height, terrain_mean, tx_power,
                                                    diff_tx_effective_height_max_terrain, tx_elevation, 
                                                    diff_tx_rx_effective_height, diff_tx_effective_height_max_clutter,
                                                    clutter_max, angle_difference_tx_rx, los_distance, fresnel_radii,
                                                    obstruction_fresnel, clutter_sd, diff_rx_effective_height_max_clutter,
                                                    distance, distance_tx_first_obs_terrain, clutter_mean, terrain_max,
                                                    deltacoordY, terrain_sd, terrain_min, deltacoordX)

  

teste_top6 <- features_teste %>% select(downtilt, azimuth, tx_effective_height, tx_height, terrain_mean, tx_power)

teste_top10 <- features_teste %>% select(downtilt, azimuth, tx_effective_height, tx_height, terrain_mean, tx_power,
                                         diff_tx_effective_height_max_terrain, tx_elevation, 
                                         diff_tx_rx_effective_height, diff_tx_effective_height_max_clutter)
  
teste_top15 <- features_teste %>% select(downtilt, azimuth, tx_effective_height, tx_height, terrain_mean, tx_power,
                                         diff_tx_effective_height_max_terrain, tx_elevation, 
                                         diff_tx_rx_effective_height, diff_tx_effective_height_max_clutter,
                                         clutter_max, angle_difference_tx_rx, los_distance, fresnel_radii,
                                         obstruction_fresnel)
  
teste_top20 <- features_teste %>% select(downtilt, azimuth, tx_effective_height, tx_height, terrain_mean, tx_power,
                                         diff_tx_effective_height_max_terrain, tx_elevation, 
                                         diff_tx_rx_effective_height, diff_tx_effective_height_max_clutter,
                                         clutter_max, angle_difference_tx_rx, los_distance, fresnel_radii,
                                         obstruction_fresnel, clutter_sd, diff_rx_effective_height_max_clutter,
                                         distance, distance_tx_first_obs_terrain, clutter_mean)


teste_top25 <- features_teste %>% select(downtilt, azimuth, tx_effective_height, tx_height, terrain_mean, tx_power,
                                         diff_tx_effective_height_max_terrain, tx_elevation, 
                                         diff_tx_rx_effective_height, diff_tx_effective_height_max_clutter,
                                         clutter_max, angle_difference_tx_rx, los_distance, fresnel_radii,
                                         obstruction_fresnel, clutter_sd, diff_rx_effective_height_max_clutter,
                                         distance, distance_tx_first_obs_terrain, clutter_mean, terrain_max,
                                         deltacoordY, terrain_sd, terrain_min, deltacoordX)



#RandomForest with best features
fit_rf_top6 <- randomForest(x = features_treino_top6, y = target_treino, ntree = 300, type = "regression")
fit_rf_top10 <- randomForest(x = features_treino_top10, y = target_treino, ntree =  300, type = "regression")
fit_rf_top15 <- randomForest(x = features_treino_top15, y = target_treino, ntree =  300, type = "regression")
fit_rf_top20 <- randomForest(x = features_treino_top20, y = target_treino, ntree =  300, type = "regression")
fit_rf_top25 <- randomForest(x = features_treino_top25, y = target_treino, ntree =  300, type = "regression")



##################### MODELS VALIDATION ##############


##RSRP by Random Forest

#All features
preditos_teste <- predict(fit_rf, teste)

#Top features
preditos_top6 <- predict(fit_rf_top6, teste_top6)
preditos_top10 <- predict(fit_rf_top10, teste_top10)
preditos_top15 <- predict(fit_rf_top15, teste_top15)
preditos_top20 <- predict(fit_rf_top20, teste_top20)
preditos_top25 <- predict(fit_rf_top25, teste_top25)



## Error metrics


##Teste
#Pearson correlation
cor_teste <- cor(target_teste,preditos_teste)
cor_teste
#Mean Square Error
mse_teste <- mse(target_teste,preditos_teste)
mse_teste
#Root Mean Square Error
rmse_teste <- rmse(target_teste,preditos_teste)
rmse_teste
#Mean Absolut Percentage Error
mape_teste <- mape(target_teste,preditos_teste)
mape_teste
#Summary
sumario_teste <- summary(preditos_teste)
sumario_teste



##Top Features
rmse(target_teste,preditos_top6)
mape(target_teste,preditos_top6)

rmse(target_teste,preditos_top10)
mape(target_teste,preditos_top10)

rmse(target_teste,preditos_top15)
mape(target_teste,preditos_top15)

rmse(target_teste,preditos_top20)
mape(target_teste,preditos_top20)

rmse(target_teste,preditos_top25)
mape(target_teste,preditos_top25)




####Save models

save(fit_rf, treino, teste, bestmtry, file = "fit_rf_all64_features.RData")

save(fit_rf_top6, file = "fit_rf_top6_features.RData")
save(fit_rf_top10, file = "fit_rf_top10_features.RData")
save(fit_rf_top15, file = "fit_rf_top15_features.RData")
save(fit_rf_top20, file = "fit_rf_top20_features.RData")
save(fit_rf_top25, file = "fit_rf_top25_features.RData")



