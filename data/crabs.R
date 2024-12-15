# adding crabs.Rdata to datasets to eliminate dependence on asbio.
# asbio does not install in current versions of R and R Studio

library(asbio)
data(crabs)
crabs <-  crabs |> 
  mutate(color = fct_recode(color,
                            "light medium" = "1",
                            "medium" = "2",
                            "dark medium" = "3",
                            "dark" = "4")) |> 
  mutate(spine = fct_recode(spine,
                            "both good" = "1",
                            "one worn or broken" = "2",
                            "both worn or broken" = "3" )) 

save(crabs, file = "data/crabs.Rdata")

# check match with table 3 in Brockman
