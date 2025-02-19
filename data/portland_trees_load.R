library(pdxTrees)
set.seed(080546)

pdx <- get_pdxTrees_parks()
mu <- mean(pdx$Carbon_Storage_lb, na.rm = TRUE)
sigma <- round(sd(pdx$Carbon_Storage_lb, na.rm = TRUE),2)
portland_park_trees <- pdx  |> 
  dplyr::select(Common_Name, 
         Condition, 
         Tree_Height, 
         Structural_Value, 
         Carbon_Storage_lb, 
         Pollution_Removal_value,
         Carbon_Sequestration_value,
         Carbon_Sequestration_lb) |>  
  drop_na() |> 
  dplyr::slice_sample(n = 500) 

save(portland_park_trees, file = "./data/portland_park_trees.Rdata")
  
              
