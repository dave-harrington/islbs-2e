# loading crabs data directly from Agresti site
# On his site, color == 1 => light, bu there were no light
# crabs in Brockman's data. AA used color - 1 in his text;
# I have used original coding


crabs_aa <- read.table("crabs_aa.txt", header = TRUE)
crabs <-  crabs_aa |> 
  mutate(color = case_when(
    color == 2 ~ "light medium",
    color == 3 ~ "medium",
    color == 4 ~ "dark medium",
    color == 5 ~ "dark")
  ) |> 
  mutate(color = factor(color,
                        levels = c("light medium",
                                   "medium",
                                   "dark medium",
                                   "dark"),
                        ordered = TRUE)) |> 
  mutate(spine = case_when(
    spine == 1 ~ "both good",
    spine == 2 ~ "one worn or broken",
    spine == 3 ~ "both worn or broken")
  ) |> 
  mutate(spine = factor(spine,
                        levels =  c("both good",
                                    "one worn or broken",
                                    "both worn or broken"),
                        ordered = TRUE)) 

table(crabs$color)
table(crabs$spine)
summary(crabs$weight)

save(crabs, file = "crabs.Rdata")

# check match with table 3 in Brockman
# 
# 
df <- crabs
df$y <- (crabs$satell > 0)
table(df$y)
#counts match table 3
#
tapply(df$weight, df$y, mean) # close; 
tapply(df$width, df$y, mean) # close; 
# suspect the difference is because AA has updated data

