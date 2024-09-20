library(tidyverse)
library(raster)
library(e1071)


#Cell or base station raw data
cell_file <- "cell_data.csv"

#Filename to save features
filename_destino <- "features.csv"



dados = read_csv(cell_file, guess_max = 5000)

dados = dados[complete.cases(dados),]
dados$LinkID = 1:nrow(dados)



#############


#TX position
ponto_tx = dados %>% dplyr::select(`Cell X`, `Cell Y`, `Cell Altitude`, Height, `Cell Building Height`, `Cell Clutter Index`) %>% head(1)
coord_long_tx = ponto_tx$`Cell X`
coord_lat_tx = ponto_tx$`Cell Y`
altura_tx = ponto_tx$`Cell Altitude` + ponto_tx$Height + ponto_tx$`Cell Building Height`


#Profiles
mapa_terrain = dados %>% dplyr::select(X, Y, Altitude) %>% 
  add_row(X = ponto_tx$`Cell X`, Y = ponto_tx$`Cell Y`, Altitude = ponto_tx$`Cell Altitude`) %>% 
  distinct(X, Y, .keep_all = T)

mapa_clutter = dados %>% dplyr::select(X, Y, `Building Height`) %>% 
  add_row(X = ponto_tx$`Cell X`, Y = ponto_tx$`Cell Y`, `Building Height` = ponto_tx$`Cell Building Height`) %>% 
  distinct(X, Y, .keep_all = T)

mapa_clutterindex = dados %>% dplyr::select(X, Y, `Clutter Index`) %>% 
  add_row(X = ponto_tx$`Cell X`, Y = ponto_tx$`Cell Y`, `Clutter Index` = ponto_tx$`Cell Clutter Index`) %>% 
  distinct(X, Y, .keep_all = T)


#Raster
projecao = "+proj=utm +zone=49 +datum=WGS84  +units=m"
raster_terrain = rasterFromXYZ(mapa_terrain, crs = projecao)
raster_clutter = rasterFromXYZ(mapa_clutter, crs = projecao)
raster_clutterindex = rasterFromXYZ(mapa_clutterindex, crs = projecao)



#Initialize the features dataframe
data_features <- data.frame(
  
  #ID
  "network_id" = "a",
  "tx_tag" = "a",
  "rx_tag" = "a",
  
  #Radio and Link
  "freq" = 0,
  "tx_power" = 0,
  "tx_height" = 0,
  "rx_height" = 0,
  "tx_elevation" = 0,
  "rx_elevation" = 0,
  "tx_latitude" = 0,
  "tx_longitude" = 0,
  "rx_latitude" = 0,
  "rx_longitude" = 0,
  "tx_effective_height" = 0,
  "rx_effective_height" = 0,
  "distance" = 0,
  "los_distance" = 0,
  "angle_difference_tx_rx" = 0,
  "diff_tx_rx_effective_height" = 0,
  "downtilt" = 0,
  "azimuth" = 0,
  "tca" = 0,
  "bca" = 0,
  
  
  ##Terrain and Clutter Statics
  
  #Terrain variation
  "terrain_mean" = 0,
  "terrain_median" = 0,
  "terrain_max" = 0,
  "terrain_min" = 0,
  "terrain_sd" = 0,
  "terrain_1q" = 0,
  "terrain_3q" = 0,
  "terrain_skewness" = 0,
  "terrain_kurtosis" = 0,
  
  
  #Buildings variation
  "clutter_mean" = 0,
  "clutter_median" = 0,
  "clutter_max" = 0,
  "clutter_min" = 0,
  "clutter_sd" = 0,
  "clutter_1q" = 0,
  "clutter_3q" = 0,
  "clutter_skewness" = 0,
  "clutter_kurtosis" = 0,
  
  ##Obstructions
  "distance_tx_first_obs_terrain" = 0,
  "distance_rx_first_obs_terrain" = 0,
  "distance_tx_first_obs_clutter" = 0,
  "distance_rx_first_obs_clutter" = 0,
  "diff_tx_effective_height_max_terrain" = 0,
  "diff_rx_effective_height_max_terrain" = 0,
  "diff_tx_effective_height_max_clutter" = 0,
  "diff_rx_effective_height_max_clutter" = 0,
  "num_obstructions_los" = 0,
  "ptb" = 0,
  "ptt" = 0,
  "ptfs" = 0,
  "los_indicator" = 0,
  
  #Fresnel Zone
  "obstruction_fresnel" = 0,
  "fresnel_radii" = 0,
  
  #Diffraction Loss - CKE
  "diffraction_loss" = 0,
  
  
  #Portion Through Clutter / Clutter index
  "clutter_index_tx" = 0,
  "clutter_index_rx" = 0,
  "ptc_1" = 0,
  "ptc_2" = 0,
  "ptc_3" = 0,
  "ptc_4" = 0,
  "ptc_5" = 0,
  "ptc_6" = 0,
  "ptc_7" = 0,
  "ptc_8" = 0,
  "ptc_9" = 0,
  "ptc_10" = 0,
  "ptc_11" = 0,
  "ptc_12" = 0,
  "ptc_13" = 0,
  "ptc_14" = 0,
  "ptc_15" = 0,
  "ptc_16" = 0,
  "ptc_17" = 0,
  "ptc_18" = 0,
  "ptc_19" = 0,
  "ptc_20" = 0,
  
  "rssi" = 0
)

#Delete the first row with empty values
data_features <- data_features[-1,]

#Escreve o nome das colunas
write_csv(data_features, filename_destino)




#Extract elevation profiles from links
for(l in 1:nrow(dados)) {
  
  dados_link = dados[l,]
  
  
  ########################## ELEVATION PROFILE ##########################
  
  coord_long_rx = as.integer(dados_link$X)
  coord_lat_rx = as.integer(dados_link$Y)
  ground_height = 0.00001 #measurement at ground level
  altura_rx = as.integer(dados_link$Altitude) + ground_height
  freq = as.integer(dados_link$`Frequency Band`)
  
  coord_tx = c(coord_long_tx, coord_lat_tx)
  coord_rx = c(coord_long_rx, coord_lat_rx)
  
  distancia = sqrt((coord_long_tx-coord_long_rx)^2 + (coord_lat_tx-coord_lat_rx)^2)
  
  #Skip link smaller than 5m distance
  if(distancia < 5){
    next
  }
  
  recorte <- SpatialLines(list(Lines(Line(rbind(coord_tx,coord_rx)), ID="a")), proj4string = raster_terrain@crs)
  
  perfil_terrain <- raster::extract(raster_terrain, recorte, exact = T, along = T, df = T)
  
  #skip links without terrain profile
  if(nrow(perfil_terrain) < 3){
    next
  }
  
  perfil_clutter <- raster::extract(raster_clutter, recorte, exact = T, along = T, df = T)
  
  #skip links without clutter profile
  if(nrow(perfil_clutter) < 3){
    next
  }
  
  perfil_clutterindex <- raster::extract(raster_clutterindex, recorte, exact = T, along = T, df = T)
  
  #skip links without clutter index profile
  if(nrow(perfil_clutterindex) < 3){
    next
  }

  perfil_elevacao_df = data.frame(terrain = perfil_terrain$Altitude, 
                                  clutter = perfil_terrain$Altitude + perfil_clutter$`Building.Height`,
                                  clutterindex = as.factor(perfil_clutterindex$`Clutter.Index`) )
  
  #Skip link with missing terrain values
  if(sum(is.na(perfil_elevacao_df$terrain)) > 0){
    next
  }
  
  
  #Add distance
  perfil_elevacao_df$distancia = seq(0, distancia, length.out = nrow(perfil_elevacao_df))
  
  
  ############ LOS  ############
  
  linha_visada <- data.frame(distancia = c(0, distancia), altura = c(altura_tx, altura_rx))
  
  angulo_visada <- (linha_visada[2,2] - linha_visada[1,2]) / (linha_visada[2,1] - linha_visada[1,1])
  perfil_elevacao_df$altura_visada <- (angulo_visada * perfil_elevacao_df$distancia) + linha_visada[1,2]
  
  ############ First Fresnel zone ############
  
  d1 <- perfil_elevacao_df$distancia
  d2 <- distancia - d1
  
  c <- 3*(10^8)
  lambda <- (c / (freq * 1000000))
  
  fresnel = sqrt((1 * d1 * d2 * lambda)/(d1 + d2))
  
  perfil_elevacao_df$altura_fresnel <- perfil_elevacao_df$altura_visada - fresnel
  perfil_elevacao_df$altura_fresnel_top <- perfil_elevacao_df$altura_visada + fresnel
  
  perfil_elevacao_df$raio_fresnel <- fresnel
  
  #Identify the link
  perfil_elevacao_df$`Cell Index` = dados_link$`Cell Index`
  perfil_elevacao_df$LinkID = dados_link$LinkID
  
  #Save it
  #dados_links_completos = rbind(dados_links_completos, dados_link)
  #dados_perfil_elevacao = rbind(dados_perfil_elevacao, perfil_elevacao_df)
  
  
  
  
  
  ########################## FEATURE EXTRACTION ##########################
  
  #Node tag X-Y
  network_id = dados_link$`Cell Index`
  tx_tag = paste0(dados_link$`Cell X`, '-', dados_link$`Cell Y`)
  rx_tag = paste0(dados_link$`X`, '-', dados_link$`Y`)
  
  #RSSI (dBm)
  rssi <- dados_link$RSRP
  
  #Frequency (Mhz)
  freq <- dados_link$`Frequency Band`
  
  #Tx power (dBm)
  tx_power <-  dados_link$`RS Power`
  
  #Tx and Rx coordinates (lat,long)
  tx_longitude <- dados_link$`Cell X`
  tx_latitude <- dados_link$`Cell Y`
  rx_longitude <- dados_link$X
  rx_latitude <- dados_link$Y
  
  
  #Rx measurement at ground level
  ground_height = 0.00001
  
  #Distance Tx-Rx (m)
  distance_m <- perfil_elevacao_df$distancia %>% tail(1)
  
  #Tx and Rx antenna heights (m)
  tx_height <- dados_link$Height + dados_link$`Cell Building Height`
  rx_height <- ground_height
  
  #Tx and Rx elevation (m)
  tx_elevation <- dados_link$`Cell Altitude`
  rx_elevation <- dados_link$Altitude
  
  #Tx and Rx effective heights (m)
  tx_effective_height <- tx_height + tx_elevation
  rx_effective_height <- rx_height + rx_elevation
  
  #Effective Height difference between Tx and Rx
  diff_tx_rx_effective_height <- tx_effective_height - rx_effective_height
  
  
  
  
  
  #Downtilt angle (deg)
  downtilt = dados_link$`Electrical Downtilt` + dados_link$`Mechanical Downtilt`
  
  #Azimuth
  azimuth = dados_link$Azimuth
  
  
  #Get vector "Distance" (m)
  distance_path <- perfil_elevacao_df$distancia
  
  #Get vector "Terrain" (m)
  terrain_profile <- perfil_elevacao_df$terrain
  
  #Get vector "Clutter" (m) 
  clutter_profile <- perfil_elevacao_df$clutter - terrain_profile
  
  #Get vector "Clutter index" (code) 
  clutter_index_profile <- perfil_elevacao_df$clutterindex
  
  #Total height (terrain + clutter)
  total_heights <- terrain_profile + clutter_profile
  
  #First Fresnel Zone
  fresnel <- perfil_elevacao_df$raio_fresnel * (-1)
  
  
  ################ CALCULAR FEATURES #################
  
  #Terrain variation (mean, median, max, min, sd)
  terrain_mean <- mean(terrain_profile)
  terrain_median <- median(terrain_profile)
  terrain_max <- max(terrain_profile)
  terrain_min <- min(terrain_profile)
  terrain_sd <- sd(terrain_profile)
  quant_terrain <- quantile(terrain_profile)
  terrain_1q <- quant_terrain[2]  
  terrain_3q <- quant_terrain[4] 
  terrain_skewness <- skewness(terrain_profile)
  terrain_kurtosis <- kurtosis(terrain_profile)  
  
  #Obstruction (clutter) variation (mean, median, max, min, sd) total height (terrain + clutter height)
  clutter_mean <- mean(total_heights)
  clutter_median <- median(total_heights)
  clutter_max <- max(total_heights)
  clutter_min <- min(total_heights)
  clutter_sd <- sd(total_heights)
  quant_clutter <- quantile(total_heights)
  clutter_1q <- quant_clutter[2]
  clutter_3q <- quant_clutter[4]
  clutter_skewness <- skewness(total_heights)
  clutter_kurtosis <- kurtosis(total_heights)  
  
  #Default values for skewness (0) and kurtosis (1)
  if(is.nan(terrain_skewness)){
    terrain_skewness <- 0
  }
  if(is.nan(terrain_kurtosis)){
    terrain_kurtosis <- 1
  }
  if(is.nan(clutter_skewness)){
    clutter_skewness <- 0
  }
  if(is.nan(clutter_kurtosis)){
    clutter_kurtosis <- 1
  }
  
  
  #Length of Line-of-Sight (LOS)
  los_distance <- sqrt((diff_tx_rx_effective_height^2) + (distance_m^2))
  
  #Slope and equation of a line
  los_angle <- (rx_effective_height - tx_effective_height) / distance_m
  los_heights <- (los_angle * distance_path) + tx_effective_height
  
  
  #Angular difference between Tx and Rx
  angle_difference_tx_rx <- asin(distance_m/los_distance)
  angle_difference_tx_rx <- (angle_difference_tx_rx * (180/pi))
  
  #Height difference between Tx and the maximum obstruction point (if NEGATIVE then Tx is greater than max obstruction)
  diff_tx_effective_height_max_terrain <- terrain_max - tx_effective_height
  diff_tx_effective_height_max_clutter <- clutter_max - tx_effective_height
  
  #Height difference between Tx and the maximum obstruction point (if NEGATIVE then Rx > max obstruction)
  diff_rx_effective_height_max_terrain <- terrain_max - rx_effective_height
  diff_rx_effective_height_max_clutter <- clutter_max - rx_effective_height
  
  
  
  #Number of obstructions by terrain and clutter/buildings

  
  #Obstructions in Terrain
  diff_los_terrain <- los_heights - terrain_profile
  index_terrain_obs <- which(diff_los_terrain < 0)
  obstruction_init <- NA
  obstruction_final <- NA
  distance_terrain_obs <- 0
  num_obstructions_terrain <- 0
  obstruction_segments_terrain <- c()
  
  if(length(index_terrain_obs) > 0){
    for(i in 1:length(index_terrain_obs)){
      
      if(is.na(obstruction_init)){
        obstruction_init <- index_terrain_obs[i]
      }
      
      #not consecutive obstructions OR end of obs
      if( (index_terrain_obs[i+1] - index_terrain_obs[i] > 1 && i < length(index_terrain_obs) ) ||
          (i == length(index_terrain_obs)) ){
        
        #not consecutive obstructions
        if(index_terrain_obs[i+1] - index_terrain_obs[i] > 1 && i < length(index_terrain_obs)){
          obstruction_final = index_terrain_obs[i]
        }
        
        #end of obstruction
        if(i == length(index_terrain_obs)){
          obstruction_final = index_terrain_obs[i]
        }
        
        #1 point obstacle
        if(obstruction_init == obstruction_final && obstruction_init > 1){
          obstruction_init <- obstruction_init-1
        }
        
        distance_terrain_obs <- distance_terrain_obs + (distance_path[obstruction_final] - distance_path[obstruction_init])
        obstruction_segments_terrain <- c(obstruction_segments_terrain, paste(obstruction_init, obstruction_final, sep="-"))
        obstruction_init <- NA
        num_obstructions_terrain <- num_obstructions_terrain + 1
      }
    }
  }
  
  
  #Obstructions in Clutter
  total_heights_rx_height <- total_heights
  total_heights_rx_height[length(total_heights)] <- terrain_profile[length(terrain_profile)]
  diff_los_clutter <- los_heights - total_heights_rx_height
  index_clutter_obs1 <- which(diff_los_clutter < 0)
  index_clutter_obs <- setdiff(index_clutter_obs1, index_terrain_obs)
  obstruction_init <- NA
  obstruction_final <- NA
  distance_clutter_obs <- 0
  num_obstructions_clutter <- 0
  obstruction_segments_clutter <- c()
  
  if(length(index_clutter_obs) > 0){
    for(i in 1:length(index_clutter_obs)){
      
      if(is.na(obstruction_init)){
        obstruction_init <- index_clutter_obs[i]
      }
      
      #not consecutive obstructions OR end of obs
      if( (index_clutter_obs[i+1] - index_clutter_obs[i] > 1 && i < length(index_clutter_obs) ) ||
          (i == length(index_clutter_obs)) ){
        
        #not consecutive obstructions
        if(index_clutter_obs[i+1] - index_clutter_obs[i] > 1 && i < length(index_clutter_obs)){
          obstruction_final = index_clutter_obs[i]
        }
        
        #end of obstruction
        if(i == length(index_clutter_obs)){
          obstruction_final = index_clutter_obs[i]
        }
        
        #1 point obstacle
        if(obstruction_init == obstruction_final && obstruction_init > 1){
          obstruction_init <- obstruction_init-1
        }
        
        distance_clutter_obs <- distance_clutter_obs + (distance_path[obstruction_final] - distance_path[obstruction_init])
        obstruction_segments_clutter <- c(obstruction_segments_clutter, paste(obstruction_init, obstruction_final, sep="-"))
        obstruction_init <- NA
        num_obstructions_clutter <- num_obstructions_clutter + 1
      }
    }
  }
  
  
  #Total number of obstructions in the path
  num_obstructions_los <- num_obstructions_terrain + num_obstructions_clutter
  
  #Portion Trough Terrain (PTT)
  ptt <- distance_terrain_obs / distance_m
  
  #Portion Trough Building(PTB)
  ptb <- distance_clutter_obs / distance_m
  
  #Portion Through Free Space (PTFS)
  ptfs <- (1 - (ptt + ptb))
  
  #LOS / NLOS state (0 or 1)
  if (num_obstructions_los == 0){
    los_indicator <- 0
  }else{
    los_indicator <- 1
  }
  
  
  #Distance TX/RX to Obstruction - Terrain
  ##Set default values when there is no obstruction
  if(length(index_terrain_obs) > 0){
    distance_tx_first_obs_terrain <- distance_path[index_terrain_obs[1]]
    distance_rx_first_obs_terrain <- distance_m - distance_path[index_terrain_obs[length(index_terrain_obs)]]
  }else{
    distance_tx_first_obs_terrain <- distance_m
    distance_rx_first_obs_terrain <- distance_m
  }
  
  #Distance TX/RX to Obstruction - Clutter
  if(length(index_clutter_obs) > 0){
    distance_tx_first_obs_clutter <- distance_path[index_clutter_obs[1]]
    distance_rx_first_obs_clutter <- distance_m - distance_path[index_clutter_obs[length(index_clutter_obs)]]
  }else{
    distance_tx_first_obs_clutter <- distance_m
    distance_rx_first_obs_clutter <- distance_m
  }
  
  
  
  ##FRESNEL ZONES
  
  #Radius of the first Fresnel Zone in middle point
  d1 <- (distance_m / 2) 
  d2 <- d1
  
  c <- 3*(10^8)
  lambda <- (c / (freq * 1000000))
  
  fresnel_radii = sqrt((1 * d1 * d2 * lambda)/(d1 + d2))
  
  
  #Obstruction percentage of First Fresnel Zone regards to clutter
  diametro_fresnel <- 2 * perfil_elevacao_df$raio_fresnel
  
  diametro_fresnel <- diametro_fresnel + 0.00000000000001
  
  modifica <- total_heights 
  
  modifica[length(modifica)] <- rx_effective_height
  
  diff_clutter_fresnel <- modifica - perfil_elevacao_df$altura_fresnel
  
  portion_clutter_fresnel <- diff_clutter_fresnel / diametro_fresnel
  
  obstruction_fresnel <- max(portion_clutter_fresnel)
  
  if(obstruction_fresnel < 0){
    obstruction_fresnel <- 0
  }
  if(obstruction_fresnel > 1){
    obstruction_fresnel <- 1
  }
  
  
  
  ##Terrain Clearance Angle (TCA) Rx
  index_max_profile <- which.max(terrain_profile)
  oposto <- terrain_profile[index_max_profile] - rx_elevation
  adjacente <-  distance_path[index_max_profile]
  
  if(oposto == 0 || adjacente == 0){
    tca = 0
  }else{
    radiano <- atan(oposto/adjacente)
    tca <- radiano * (180/pi)
  }
  
  
  ##Building Clearance Angle (BCA) Rx
  index_max_profile <- which.max(total_heights)
  oposto <- total_heights[index_max_profile] - rx_elevation
  adjacente <-  distance_path[index_max_profile]
  
  if(oposto == 0 || adjacente == 0){
    bca = 0
  }else{
    radiano <- atan(oposto/adjacente)
    bca <- radiano * (180/pi)
  }
  
  
  ##CKE Diffraction Loss
  
  #Initialize jotas
  jota_v1 <- 0
  jota_v2 <- 0
  jota_v3 <- 0
  
  
  #Calcula as derivadas de fresnel e a perda por difra??o jota
  calcular_jota <- function(h, d1, d2){
    
    #Equation 26 from ITU-R P.526-15 recommendation
    v <- h * sqrt((2/lambda) * ((1/d1) + (1/d2)))
    
    integrando1 <- function(x) {cos(((pi * x^2)/2))}
    integrando2 <- function(x) {sin(((pi * x^2)/2))}
    
    c_v <-  integrate(integrando1, lower = 0, upper = v, stop.on.error = F, subdivisions = 20000, rel.tol = .Machine$double.eps^.05)
    c_v <- c_v$value
    
    s_v <-  integrate(integrando2, lower = 0, upper = v, stop.on.error = F, subdivisions = 20000, rel.tol = .Machine$double.eps^.05)
    s_v <- s_v$value
    
    jota_v <- -20*log(((sqrt((1 - c_v - s_v)^2 + (c_v - s_v)^2)) / 2))
    
    return(jota_v)
  }
  
  
  ########################## PROFILE 1 ##########################
  
  if(length(total_heights) > 2){
    
    total_heights_reduzido <- total_heights[2:(length(total_heights)-1)]
    index_max_reduzido <- which.max(total_heights_reduzido)
    index_max_profile <- (index_max_reduzido + 1)
    altura_max <- total_heights[index_max_profile]
    
    oposto1 <- abs(altura_max - tx_effective_height)
    base1 <- distance_path[index_max_profile] 
    
    oposto2 <- abs(altura_max - rx_effective_height)
    base2 <- distance_m - distance_path[index_max_profile]
    
    d1 <- sqrt(base1^2 + oposto1^2)
    d2 <- sqrt(base2^2 + oposto2^2)
    
    h <- altura_max - los_heights[index_max_profile]
    
    if (d1 == 0 || d2 == 0){
      jota_v1 <- 0
    }else{
      jota_v1 <- calcular_jota(h, d1, d2)
    }
    
    
    ########################## PROFILE 2 ##########################
    
    if(index_max_profile > 2){
      
      total_heights_reduzido2 <- total_heights[2:(index_max_profile-1)]
      index_max_reduzido2 <- which.max(total_heights_reduzido2)
      index_max_profile2 <- (index_max_reduzido2 + 1)
      altura_max2 <- total_heights[index_max_profile2]
      
      oposto1 <- abs(altura_max2 - tx_effective_height)
      base1 <- distance_path[index_max_profile2]
      
      oposto2 <- abs(altura_max2 - altura_max)
      base2 <- distance_path[index_max_profile] - distance_path[index_max_profile2]
      
      d1 <- sqrt(base1^2 + oposto1^2)
      d2 <- sqrt(base2^2 + oposto2^2)
      
      h <- altura_max2 - los_heights[index_max_profile2]
      
      if (d1 == 0 || d2 == 0){
        jota_v2 <- 0
      }else{
        jota_v2 <- calcular_jota(h, d1, d2)
      }
    }
    
    
    ########################## PROFILE 3 ##########################
    
    if((index_max_profile + 1) < (length(total_heights)-1)){
      
      total_heights_reduzido3 <- total_heights[(index_max_profile+1):(length(total_heights)-1)]
      index_max_reduzido3 <- which.max(total_heights_reduzido3)
      index_max_profile3 <- (index_max_reduzido3 + index_max_profile)
      altura_max3 <- total_heights[index_max_profile3]
      
      oposto1 <- abs(altura_max3 - altura_max)
      base1 <- distance_path[index_max_profile3] -  distance_path[index_max_profile] 
      
      oposto2 <- abs(altura_max3 - rx_effective_height)
      base2 <- distance_m - distance_path[index_max_profile3]
      
      d1 <- sqrt(base1^2 + oposto1^2)
      d2 <- sqrt(base2^2 + oposto2^2)
      
      h <- altura_max3 - los_heights[index_max_profile3]
      
      if (d1 == 0 || d2 == 0){
        jota_v3 <- 0
      }else{
        jota_v3 <- calcular_jota(h, d1, d2)
      }
    }
    
    
  }#End first if for jota 1
  
  
  #Valor do CKE / Diffraction Loss
  diffraction_loss <- jota_v1 + jota_v2 + jota_v3
  
  
  ##Clutter index
  
  #Clutter index at Tx and Rx
  clutter_index_tx = dados_link$`Cell Clutter Index`
  clutter_index_rx = dados_link$`Clutter Index`
  
  #Percentage of each clutter index along the path
  
  get_distance_clutter_index = function(index){
    #Quantity of steps equals index
    clutter_index_qtd = perfil_elevacao_df %>% filter(clutterindex == index) %>% count() %>% as.integer()
    #Percentage of this index regards the number of steps
    clutter_index_perc =  (clutter_index_qtd / nrow(perfil_elevacao_df)) 
    return(clutter_index_perc)
  }
  
  ptc_1 = get_distance_clutter_index(1)
  ptc_2 = get_distance_clutter_index(2)
  ptc_3 = get_distance_clutter_index(3)
  ptc_4 = get_distance_clutter_index(4)
  ptc_5 = get_distance_clutter_index(5)
  ptc_6 = get_distance_clutter_index(6)
  ptc_7 = get_distance_clutter_index(7)
  ptc_8 = get_distance_clutter_index(8)
  ptc_9 = get_distance_clutter_index(9)
  ptc_10 = get_distance_clutter_index(10)
  ptc_11 = get_distance_clutter_index(11)
  ptc_12 = get_distance_clutter_index(12)
  ptc_13 = get_distance_clutter_index(13)
  ptc_14 = get_distance_clutter_index(14)
  ptc_15 = get_distance_clutter_index(15)
  ptc_16 = get_distance_clutter_index(16)
  ptc_17 = get_distance_clutter_index(17)
  ptc_18 = get_distance_clutter_index(18)
  ptc_19 = get_distance_clutter_index(19)
  ptc_20 = get_distance_clutter_index(20)
  
  
  #Add new row in the features dataset
  data_features <- data.frame(
    network_id = network_id,
    tx_tag = tx_tag,
    rx_tag = rx_tag,
    freq = freq,
    tx_power = tx_power,
    tx_height = tx_height,
    rx_height = rx_height,
    tx_elevation = tx_elevation,
    rx_elevation = rx_elevation,
    tx_latitude = tx_latitude,
    tx_longitude = tx_longitude,
    rx_latitude = rx_latitude,
    rx_longitude = rx_longitude,
    tx_effective_height = tx_effective_height,
    rx_effective_height = rx_effective_height,
    distance = distance_m,
    los_distance = los_distance,
    angle_difference_tx_rx = angle_difference_tx_rx,
    diff_tx_rx_effective_height = diff_tx_rx_effective_height,
    downtilt = downtilt,
    azimuth = azimuth,
    tca = tca,
    bca = bca,
    terrain_mean = terrain_mean,
    terrain_median = terrain_median,
    terrain_max = terrain_max,
    terrain_min = terrain_min,
    terrain_sd = terrain_sd,
    terrain_1q = terrain_1q,
    terrain_3q = terrain_3q,
    terrain_skewness = terrain_skewness,
    terrain_kurtosis = terrain_kurtosis,
    clutter_mean = clutter_mean,
    clutter_median = clutter_median,
    clutter_max = clutter_max,
    clutter_min = clutter_min,
    clutter_sd = clutter_sd,
    clutter_1q = clutter_1q,
    clutter_3q = clutter_3q,
    clutter_skewness = clutter_skewness,
    clutter_kurtosis = clutter_kurtosis,
    distance_tx_first_obs_terrain = distance_tx_first_obs_terrain,
    distance_rx_first_obs_terrain = distance_rx_first_obs_terrain,
    distance_tx_first_obs_clutter = distance_tx_first_obs_clutter,
    distance_rx_first_obs_clutter = distance_rx_first_obs_clutter,
    diff_tx_effective_height_max_terrain = diff_tx_effective_height_max_terrain,
    diff_rx_effective_height_max_terrain = diff_rx_effective_height_max_terrain,
    diff_tx_effective_height_max_clutter = diff_tx_effective_height_max_clutter,
    diff_rx_effective_height_max_clutter = diff_rx_effective_height_max_clutter,
    num_obstructions_los = num_obstructions_los,
    ptb = ptb,
    ptt = ptt,
    ptfs = ptfs,
    los_indicator = los_indicator,
    obstruction_fresnel = obstruction_fresnel,
    fresnel_radii = fresnel_radii,
    diffraction_loss = diffraction_loss,
    clutter_index_tx = clutter_index_tx,
    clutter_index_rx = clutter_index_rx,
    ptc_1 = ptc_1,
    ptc_2 = ptc_2,
    ptc_3 = ptc_3,
    ptc_4 = ptc_4,
    ptc_5 = ptc_5,
    ptc_6 = ptc_6,
    ptc_7 = ptc_7,
    ptc_8 = ptc_8,
    ptc_9 = ptc_9,
    ptc_10 = ptc_10,
    ptc_11 = ptc_11,
    ptc_12 = ptc_12,
    ptc_13 = ptc_13,
    ptc_14 = ptc_14,
    ptc_15 = ptc_15,
    ptc_16 = ptc_16,
    ptc_17 = ptc_17,
    ptc_18 = ptc_18,
    ptc_19 = ptc_19,
    ptc_20 = ptc_20,
    rssi = rssi
  )
  
  #Save link features
  write_csv(data_features, filename_destino, append = T, col_names = F)
  
  
} #End links


