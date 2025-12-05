library(dplyr)
library(MASS)
library(ggplot2

#NOTE: The master dataset "grid_xy" has been split into two subsets for modeling.
# Both subsets are available in the folder: "CSV_data" : 
#
#   1. model_1_input 
#        - All grid cells with predictor variables 
#        - Used for Binomial GLM (Sampling Effort)
#   2. model_2_input 
#        - Grid cells with occurrences > 0 and extracted predictors
#        - Used for Negative Binomial GLM (Sampling Intensity)
#



# 1. Create binary sampling-effort variable (0/1)


grid_xy$sampling_effort <- ifelse(grid_xy$num_occurrences == 0, 0, 1)



# 2. Prepare model datasets


# A. Sampling effort model : all grid cells
model_1_input <- grid_xy

# B. Sampling intensity model : only grid cells where occurrences > 0
model_2_input <- grid_xy %>%
  filter(num_occurrences > 0)




# 3. remove outliers 


model_1_input <- model_1_input %>%
  filter(
    RoadDensity < 1.5,
    topographic_roughness < 250,
    mean_pop < 1500
  )

model_2_input <- model_2_input %>%
  filter(
    RoadDensity < 1.5,
    topographic_roughness < 250,
    mean_pop < 1500
  )



# 4. Fit Binomial GLM (sampling effort)


model_bin_clean <- glm(
  sampling_effort ~ Elevation + topographic_roughness +
    mean_pop + TravelTime + GDP + region,
  data = model_1_input,
  family = binomial
)



# 5. Fit Negative Binomial GLM (sampling intensity)


model_nb_clean <- glm.nb(
  num_occurrences ~ pa_ratio + topographic_roughness +
    mean_pop + TravelTime + region + GDP,
  data = model_2_input
)


# 6. Diagnostics


par(mfrow = c(1, 2))

plot(
  fitted(model_bin_clean),
  resid(model_bin_clean, type = "deviance"),
  xlab = "Fitted", ylab = "Residuals", main = "Binomial Residuals"
)

plot(
  fitted(model_nb_clean),
  resid(model_nb_clean, type = "deviance"),
  xlab = "Fitted", ylab = "Residuals", main = "NB Residuals"
)
