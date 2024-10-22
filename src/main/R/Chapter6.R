library(dplyr) # A Grammar of Data Manipulation
library(evd) # Functions for Extreme Value Distributions
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics 
library(kableExtra) # Construct Complex Table with 'kable' and Pipe Syntax 
library(mlogit) # Multinomial Logit Models
library(tidyr) # Tidy Messy Data

data("Heating")

Proportion <- Heating %>%
  # Group the rows by the value of `depvar`
  group_by(depvar) %>%
  # Count the number of cases of each outcome in `depvar` 
  summarise(no_rows = n())
df <- data.frame(System = c("Gas Central",
                            "Gas Room", 
                            "Electric Central", 
                            "Electric Room", 
                            "Heat Pump"),
                 Installation = c(median(Heating$ic.gc), 
                                  median(Heating$ic.gr), 
                                  median(Heating$ic.ec), 
                                  median(Heating$ic.er), 
                                  median(Heating$ic.hp)),
                 Operation = c(median(Heating$oc.gc), 
                               median(Heating$oc.gr), 
                               median(Heating$oc.ec), 
                               median(Heating$oc.er), 
                               median(Heating$oc.hp)),
                 Proportion = Proportion$no_rows/900)

df %>% 
  kable() %>%
  kable_styling()

Heating %>%
  # Select columns 13 to 16
  select(13:16) %>% 
  summary()

H <- Heating %>% 
  mlogit.data(shape = "wide",
              # Name of column with the choices
              choice = "depvar",
              # Numbers of columns with attributes that vary by alternative 
              varying = c(3:12))

# model1 with only installation cost (without operation cost) and electric central heating system ec
# as reference level = ASC or beta_ec = 0
model1 <- mlogit(depvar ~ ic,
                 Heating,
                 shape = "wide", 
                 choice = "depvar", 
                 reflevel = "ec", 
                 varying = c(3:7))
stargazer::stargazer(model1,
                     type = "latex",
                     header = FALSE,
                     single.row = TRUE,
                     title = "Estimation results: Model 1 (wide data as input)")

ic_min <- Heating[1,] 
ic_mean <- Heating[1,] 
ic_max <- Heating[1,]

min_cost <- Heating %>% 
  select(starts_with("ic")) %>%
  summarise(across(.cols = everything(),
                   min)) 
mean_cost <- Heating %>%
  select(starts_with("ic"))%>%
  summarise(across(.cols = everything(),
                   mean)) 
max_cost <- Heating %>%
  select(starts_with("ic"))%>% 
  summarise(across(.cols = everything(),
                   max))

ic_min[3:7] <- min_cost 
ic_mean[3:7] <- mean_cost 
ic_max[3:7] <- max_cost

head(ic_max)

p_model1_ic_min <- predict(model1,
                           newdata = mlogit.data(ic_min,
                                                 shape = "wide", 
                                                 choice = "depvar", 
                                                 varying = 3:7))
p_model1_ic_mean <- predict(model1,
                            newdata = mlogit.data(ic_mean,
                                                  shape = "wide", 
                                                  choice = "depvar", 
                                                  varying = 3:7))
p_model1_ic_max <- predict(model1,
                           newdata = mlogit.data(ic_max,
                                                 shape = "wide", 
                                                 choice = "depvar", 
                                                 varying = 3:7))

min_cost
p_model1_ic_min

data.frame(System = c("Electric Central",
                      "Electric Room", 
                      "Gas Central", 
                      "Gas Room", 
                      "Heat Pump"),
           ic_min = ic_min %>%
             # Select installation costs in the same order as the probabilities
             select(ic.ec, ic.er, ic.gc, ic.gr, ic.hp) %>% 
             t() %>%
             as.numeric() %>%
             round(1),
           p_min = p_model1_ic_min %>%
             as.numeric() %>% 
             round(3),
           ic_mean = ic_mean %>%
             # Select installation costs in the same order as the probabilities
             select(ic.ec, ic.er, ic.gc, ic.gr, ic.hp) %>% 
             t() %>%
             as.numeric() %>%
             round(1),
           p_mean = p_model1_ic_mean %>%
             as.numeric() %>% 
             round(3),
           ic_max = ic_max %>%
             # Select installation costs in the same order as the probabilities
             select(ic.ec, ic.er, ic.gc, ic.gr, ic.hp) %>% 
             t() %>%
             as.numeric() %>%
             round(1),
           p_max = p_model1_ic_max %>%
             as.numeric() %>% 
             round(3)) %>%
  kable(col.names = c("System",
                      "Cost", 
                      "Probability", 
                      "Cost", 
                      "Probability", 
                      "Cost", 
                      "Probability"),
        digits = 3) %>% 
  kable_styling() %>%
  add_header_above(c(" " = 1,
                     "Minimum Cost" = 2,
                     "Mean Cost" = 2, 
                     "Maximum Cost" = 2))

gc_direct_marginal_effect_min <- -0.00168108 * (1 - 0.617)
gc_direct_marginal_effect_mean <- -0.00168108 * (1 - 0.639)
gc_direct_marginal_effect_max <- -0.00168108 * (1 - 0.672)

# The values above indicate that the probabilities of choosing a gas central system 
# would decrease by approximately 0.064%, 0.061%, 0.055% if the installation cost 
# increased by one dollar from the minimum, mean, and maximum installation cost, 
# respectively.

gc_cross_marginal_effect_min <- -(-0.00168108 * 0.617)
gc_cross_marginal_effect_mean <- -(-0.00168108 * 0.639)
gc_cross_marginal_effect_max <- -(-0.00168108 * 0.672)

# The values above indicate that the probabilities of choosing an electric central heating 
# system would increase by approximately 0.103%, 0.107%, 0.113% if the cost of 
# installing a gas central heating system increased by one dollar from the minimum, 
# mean, and maximum installation cost, respectively.

gc_direct_point_elasticity_min <- -0.00168108 * 431.8 * (1 - 0.617)
gc_direct_point_elasticity_mean <- -0.00168108 * 776.8 * (1 - 0.639)
gc_direct_point_elasticity_max <- -0.00168108 * 1158.9 * (1 - 0.672)

# These values indicate that the probability of choosing the gas central system with 
# the lowest installation cost declines by approximately 0.278% when the installation 
# cost increases by 1%. When the installation cost is the mean and the maximum, the 
# probability of selecting the electric central system declines by approximately 0.471% 
# and 0.639% respectively when the cost of installation increases by 1%.

gc_cross_point_elasticity_min <- -(-0.00168108 * 431.8 * 0.617)
gc_cross_point_elasticity_mean <- -(-0.00168108 * 776.8 * 0.639)
gc_cross_point_elasticity_max <- -(-0.00168108 * 1158.9 * 0.672)

# In other words, the probability of choosing a system other than gas central system 
# increases by approximately 0.448%, 0.834%, 1.309% when the cost of installing a 
# gas central system goes up by 1% from the minimum, mean, and maximum base 
# installation costs.

#################################### the above effects / elasticities can also be calculated as follows #################################################
# values on diagonal: direct marg effects, off diagonal: cross marginal effects
# marg effects min
effects(model1,
        # Calculate the marginal effects with respect to attribute "ic"
        covariate = "ic",
        # Type of effects to compute: relative for probability, absolute for attribute 
        type = "ra",
        data = mlogit.data(ic_min,
                           shape = "wide", 
                           choice = "depvar", 
                           varying = 3:7))

# marg effects mean
effects(model1,
        covariate = "ic",
        type = "ra",
        data = mlogit.data(ic_mean,
                           shape = "wide", 
                           choice = "depvar", 
                           varying = 3:7))

# marg effects max
effects(model1,
        covariate = "ic",
        type = "ra",
        data = mlogit.data(ic_max,
                           shape = "wide", 
                           choice = "depvar", 
                           varying = 3:7))

# values on diagonal: direct point elasticities, off diagonal: cross point elasticities
# elasticities min
effects(model1,
        covariate = "ic",
        type = "rr",
        data = mlogit.data(ic_min,
                           shape = "wide", 
                           choice = "depvar", 
                           varying = 3:7))
# elasticities mean
effects(model1,
        covariate = "ic",
        type = "rr",
        data = mlogit.data(ic_mean,
                           shape = "wide", 
                           choice = "depvar", 
                           varying = 3:7))
# elasticities max
effects(model1,
        covariate = "ic",
        type = "rr",
        data = mlogit.data(ic_max,
                           shape = "wide", 
                           choice = "depvar", 
                           varying = 3:7))

summary(Heating$region)

model2 <- mlogit(depvar ~ ic | region,
                 Heating,
                 shape = "wide", 
                 choice = "depvar", 
                 reflevel = "ec", 
                 varying = c(3:7))
stargazer::stargazer(model2,
                     header = FALSE,
                     single.row = TRUE,
                     title = "Estimation results: Model 2")

ic_mean_region <- ic_mean %>%
  mutate(count = 4) %>% 
  uncount(count)
ic_mean_region$region <- c("valley", 
                           "scostl", 
                           "mountn", 
                           "ncostl")
head(ic_mean_region)

p_region_ic_mean <- data.frame(Region = c("valley", 
                                          "scostl", 
                                          "mountn", 
                                          "ncostl"),
                               predict(model2,
                                       newdata = mlogit.data(ic_mean_region,
                                                             shape = "wide", 
                                                             choice = "depvar", 
                                                             varying = 3:7),
                                       outcome = FALSE))
p_region_ic_mean

data.frame (Effect = c("valley to scostl", 
                       "valley to mountn", 
                       "valley to ncostl"),
            rbind (p_region_ic_mean[2, 2:6] - p_region_ic_mean[1, 2:6], 
                   p_region_ic_mean[3, 2:6] - p_region_ic_mean[1, 2:6], 
                   p_region_ic_mean[4, 2:6] - p_region_ic_mean[1, 2:6]))

model3 <- mlogit(depvar ~ ic + oc,
                 Heating,
                 shape = "wide", 
                 choice = "depvar", 
                 reflevel = "ec", 
                 varying = c(3:12))
stargazer::stargazer(model3,
                     header = FALSE,
                     single.row = TRUE,
                     title = "Estimation results: Model 3")


H_rebate <- Heating %>% 
  mutate(ic.hp = 0.85 * ic.hp)

data.frame(Policy = c("Do nothing", "15% rebate"),
           rbind(apply(predict(model3,
                               newdata = mlogit.data(Heating,
                                                     shape = "wide", 
                                                     choice = "depvar", 
                                                     varying = c(3:12))),
                       2, 
                       mean),
                 apply(predict(model3,
                               newdata = mlogit.data(H_rebate,
                                                     shape = "wide", 
                                                     choice = "depvar", 
                                                     varying = c(3:12))),
                       2, 
                       mean)))

X <- model.matrix(model3) 
head(X)

alt <- index(H)$alt 
Xn <- X[alt == "ec",] 
head(Xn)

Xn[, "ic"] <- Xn[, "ic"] + 200 
Xn[, "oc"] <- Xn[, "oc"] * 0.75 
head(Xn)

chid <- index(H)$chid 
head(chid, 12)

unchid <- unique(index(H)$chid) 
head(unchid, 12)

rownames(Xn) <- paste(unchid, 'new', sep = ".") 
chidb <- c(chid, unchid)
head(Xn)

X <- rbind(X, Xn)
X <- X[order(chidb), ] 
head(X,15)

exp_Xb <- as.numeric(exp(X %*% coef(model3))) # vectors 
head(exp_Xb)

# tapply does the sum of th exp_Xb for each chidb 
sum_exp_Xb <- as.numeric(tapply(exp_Xb,
                                sort(chidb), 
                                sum))
P <- exp_Xb / sum_exp_Xb[sort(chidb)]

P <- data.frame(matrix(P,
                       ncol = 6,
                       byrow = TRUE))
P <- transmute(P,
               ec = P[, 1],
               er = P[, 2], 
               gc = P[, 3], 
               gr = P[, 4], 
               hp = P[, 5], 
               new = P[, 6])
summary(rowSums(P))

apply(P, 2, mean)

apply(fitted(model3,
             outcome = FALSE),
      2, 
      mean)

p_model3 <- fitted(model3,
                   outcome = FALSE) %>%
  data.frame()
apply(p_model3,
      2, 
      mean)

o_model3 <- p_model3 %>%
  # Group by row
  rowwise() %>%
  # Find the maximum value by row
  mutate(max_p = max(c(ec, er, gc, gr, hp))) %>% 
  ungroup() %>%
  # Find the column that matches the highest probability 
  transmute(outcome = case_when(max_p == ec ~ "ec",
                                max_p == er ~ "er", 
                                max_p == gc ~ "gc", 
                                max_p == gr ~ "gr", 
                                max_p == hp ~ "hp"))
table(o_model3)

#################### exercises #############################################################
# 1. What is the difference between a marginal effect and an elasticity?
# elasticity is relative to the dependent variable y and independent variable x_k
# elasticity has no unit

# 2. Why is it inappropriate to calculate the elasticity of a dummy variable?
# because it does not make sense to do it. You cannot say what the change of 1% of the name of somebody is..

# 3. Use Model 3 in this chapter and calculate the marginal effects and the elasticities for operating cost at the mean of all variables.
model3 <- mlogit(depvar ~ ic + oc,
                 Heating,
                 shape = "wide", 
                 choice = "depvar", 
                 reflevel = "ec", 
                 varying = c(3:12))

oc_mean_3 <- Heating[1,]

mean_cost_3 <- Heating %>%
  select(starts_with("oc"))%>%
  summarise(across(.cols = everything(),
                   mean))

oc_mean_3[3:12] <- mean_cost 

effects(model3,
        # Calculate the marginal effects with respect to attribute "oc"
        covariate = "oc",
        # Type of effects to compute: relative for probability, absolute for attribute 
        type = "ra",
        data = mlogit.data(oc_mean_3,
                           shape = "wide", 
                           choice = "depvar", 
                           varying = 3:12))


# 4. Use Model 3 in this chapter to calculate the rebate needed to reach a 10% penetration rate of heat pumps.

# Estimate a new model that extends Model 3 by introducing the age of the household head. Use the electric room system (“er”) as the reference level.
# 5. Use the likelihood ratio test to compare your new model to Model 3. Discuss the results.
# 6. Is the ratio of the coefﬁcient of installation (or operation) cost to the coefﬁcient of age of household head meaningful? Explain.
