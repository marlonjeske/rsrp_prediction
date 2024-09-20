[![DOI](https://zenodo.org/badge/860171037.svg)](https://zenodo.org/doi/10.5281/zenodo.13800566)

# Enhancing Reference Signal Received Power Prediction Accuracy in Wireless Outdoor Settings: A Comprehensive Feature Importance Study

Source code for RSRP prediction using machine learning. This code includes feature extraction, training and validation using Random Forest.

## Cite

To cite the contents of this repository, please cite this paper and this repo.

M. Jeske and B. Sansó and D. Aloise and M. C. V. Nascimento (2024) Enhancing Reference Signal Received Power Prediction Accuracy in Wireless Outdoor Settings: A Comprehensive Feature Importance Study. Submitted to IEEE Transactions on Antennas and Propagation.

Marlon Jeske. Enhancing Reference Signal Received Power Prediction Accuracy in Wireless Outdoor Settings: A Comprehensive Feature Importance Study. https://github.com/marlonjeske/rsrp_prediction, 2024. GitHub repository.



## Source code

### Step 1 - Feature extraction

This process extracts the elevation profiles from base station measurements performed on 5x5 meters grids. After, model features are extracted from each generated link. 

File: feature_extraction.R

**Input:** cell_data.csv

**Required columns:** Cell Index, Cell X, Cell Y, Height, Azimuth, Electrical Downtilt, Mechanical Downtilt, Frequency Band, RS Power, Cell Altitude, Cell Building Height, Cell Clutter Index, X, Y, Altitude, Building Height, Clutter Index, RSRP

This input data is available at IEEE Dataport (https://dx.doi.org/10.21227/4ba2-tg21).

**Output:** features.csv


### Step 2 - Train the model

Using the features file, train the Random Forest model. It includes tunning, training, feature importance, and reduction of model complexity into 6 to 25 features sets.


File: random_forest.R

**Input:** features.csv

**Required columns:** freq, tx_power, tx_height, tx_elevation, rx_elevation, tx_effective_height, rx_effective_height, distance, los_distance, angle_difference_tx_rx, diff_tx_rx_effective_height, downtilt, azimuth, tca, bca, terrain_mean, terrain_median, terrain_max, terrain_min, terrain_sd, terrain_1q, terrain_3q, terrain_skewness, terrain_kurtosis, clutter_mean, clutter_median, clutter_max, clutter_min, clutter_sd, clutter_1q, clutter_3q, clutter_skewness, clutter_kurtosis, distance_tx_first_obs_terrain, distance_rx_first_obs_terrain, distance_tx_first_obs_clutter, distance_rx_first_obs_clutter, diff_tx_effective_height_max_terrain, diff_rx_effective_height_max_terrain, diff_tx_effective_height_max_clutter, diff_rx_effective_height_max_clutter, num_obstructions_los, ptb, ptt, ptfs, los_indicator, obstruction_fresnel, fresnel_radii, diffraction_loss, ptc_2, ptc_5, ptc_6, ptc_7, ptc_8, ptc_10, ptc_11, ptc_12, ptc_13, ptc_14, ptc_15, ptc_16, ptc_17, rssi, deltacoordX, deltacoordY


**Output:** saved models in RData for future usage.


## To run the R code

1. **Prepare Your Data**: Ensure you have a CSV file with the required columns as described above. This file should be ready and saved on your computer.

2. **Open RStudio**: Launch RStudio on your computer. Open the R file.

3. **Set Your Working Directory**: In RStudio, set your working directory to the folder where your CSV file is located. You can do this by clicking on the `Session` menu, selecting `Set Working Directory`, and then `Choose Directory...`. Navigate to the folder containing your CSV file and select it.

4. **Install Required Packages**: Ensure you have the necessary packages installed by running:
    ```R
    install.packages(c("tidyverse", "raster", "e1071", "Metrics", "caret", "randomForest"))
    ```
    
5. **Run the R Code**: On the RStudio, click on the `Code` menu, then `Run Region`, and click on `Run All`. Or press `Ctrl + Alt + R`.

6. **View Results**: Check the console pane (bottom-left) to see the output.

