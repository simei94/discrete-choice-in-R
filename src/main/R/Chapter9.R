# install version 1.1-3.3 of gmnl
# remotes::install_github("mauricio1986/gmnl")

library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics 
library(gmnl) # Multinomial Logit Models with Random Parameters
library(gridExtra) # Miscellaneous Functions for "Grid" Graphics 
library(kableExtra) # Construct Complex Table with 'kable' and Pipe Syntax 
library(mlogit) # Multinomial Logit Models
library(stargazer) # Well-Formatted Regression and Summary Statistics Tables 
library(tibble) # Simple Data Frames
library(tidyr) # Tidy Messy Data

data("RiskyTransport", 
     package = "mlogit")

all_available <- RiskyTransport %>%
  group_by(chid) %>%
  summarise(no_rows = length(chid), .groups = 'drop') %>% 
  filter(no_rows == 4) %>% select(chid)

RT <- inner_join(RiskyTransport,
                 all_available, 
                 by = "chid") %>%
  drop_na()

df <- RT %>% 
  group_by(mode) %>%
  summarize(proportion = sum(choice), 
            `min (cost)` = min(cost), 
            `mean (cost)` = mean(cost),
            `max(cost)` = max(cost), 
            `min (risk)` = min(risk), 
            `mean (risk)` = mean(risk), 
            `max (risk)` = max(risk), 
            .groups = 'drop') %>%
  mutate(proportion = proportion/sum(proportion)) %>% 
  column_to_rownames(var = "mode")

stargazer::stargazer(df,
                     type ="latex",
                     rownames = TRUE, 
                     summary = FALSE, 
                     digits =2,
                     header = FALSE,
                     label = "tab:descriptive-statistics",
                     title = "Summary Statistics: Cost and Risk in Risky Transport Data Set")

RT <- RT %>%
  mutate(`cost:dwage` = cost * dwage,
         `risk:dwage` = risk * dwage, 
         dwage2 = dwage^2)

RT <- mlogit.data(RT,
                  choice = "choice",
                  shape = "long", 
                  alt.var = "mode", 
                  id.var = "id", 
                  chid.var = "chid")

mnl.rt0 <- mlogit(choice ~ cost + risk | 0,
                  data = RT)
stargazer::stargazer(mnl.rt0,
                     header = FALSE,
                     single.row = TRUE,
                     title = "Estimation results: Base Multinomial Logit Model")

packageVersion("gmnl")

lc2 <- gmnl(choice ~ cost + risk | 0 | 0 | 0 | 1,
            data = RT,
            model = 'lc',
            Q = 2,
            panel = TRUE, 
            method = "bhhh")

lc3 <- gmnl(choice ~ cost + risk | 0 | 0 | 0 | 1,
            data = RT,
            model = 'lc',
            Q = 3,
            panel = TRUE, 
            method = "bhhh")

# AIC of base MNL model
2 * length(coef(mnl.rt0)) - 2 * mnl.rt0$logLik

# AIC of LC2
2 * length(coef(lc2)) - 2 * lc2$logLik$maximum

# AIC of LC3
2 * length(coef(lc3)) - 2 * lc3$logLik$maximum

# willingness to pay cost averse class
as.numeric(lc3$coefficients[2]) / as.numeric(lc3$coefficients[1])

# willingness to pay risk averse class
as.numeric(lc3$coefficients[4]) / as.numeric(lc3$coefficients[3])

# willingness to pay class 3
as.numeric(lc3$coefficients[6]) / as.numeric(lc3$coefficients[5])

# membership in each of the 3 classes is equally likely
1/3 * as.numeric(lc3$coefficients[2]) / as.numeric(lc3$coefficients[1]) + 
  1/3 * as.numeric(lc3$coefficients[4]) / as.numeric(lc3$coefficients[3]) +
  1/3 * as.numeric(lc3$coefficients[6]) / as.numeric(lc3$coefficients[5])

mnl.cov <- mlogit(choice ~ cost + risk | dwage + 0,
                  data = RT)
mnl.exp <- mlogit(choice ~ cost + cost:dwage +
                    risk + risk:dwage | 0, 
                  data = RT)

# AIC MNL COV
as.numeric(2 * length(coef(mnl.cov)) - 2 * mnl.cov$logLik)

# AIC MNL EXP
as.numeric(2 * length(coef(mnl.exp)) - 2 * mnl.exp$logLik)

as.numeric(exp(((2 * length(coef(lc3)) - 2 * lc3$logLik$maximum) -
                  (2 * length(coef(mnl.cov)) - 2 * mnl.cov$logLik))/2))


lc2.cov <- gmnl(choice ~ cost + risk | 0 | 0 | 0 | dwage,
                data = RT,
                model = 'lc',
                Q = 2,
                panel = TRUE, 
                method = "nm", 
                iterlim = 1200)

summary(lc2.cov)

# AIC lc2 cov
as.numeric(2 * length(coef(lc2.cov)) - 2 * lc2.cov$logLik$maximum)

# AIC LC3 (again for comparison)
as.numeric(2 * length(coef(lc3)) - 2 * lc3$logLik$maximum)

# model LC2-COV has about a 37% probability of capturing at least as much information as LC3:
as.numeric(exp(((2 * length(coef(lc3)) - 2 * lc3$logLik$maximum) -
                  (2 * length(coef(lc2.cov)) - 2 * lc2.cov$logLik$maximum))/2))


#Create a data frame for plotting:
df <- data.frame(dwage = seq(min(RT$dwage),
                             to = max(RT$dwage),
                             by = (max(RT$dwage) - min(RT$dwage))/100))

# Use the class-membership model to calculate the membership probabilities 
df <- df %>%
  mutate(p_1 = 1 -
           exp(coef(lc2.cov)["(class)2"] + coef(lc2.cov)["dwage:class2"] * dwage)/
           (1 + exp(coef(lc2.cov)["(class)2"] + coef(lc2.cov)["dwage:class2"] * dwage)), 
         p_2 = exp(coef(lc2.cov)["(class)2"] + coef(lc2.cov)["dwage:class2"] * dwage)/
           (1 + exp(coef(lc2.cov)["(class)2"] + coef(lc2.cov)["dwage:class2"] * dwage))) %>% 
  # Pivot longer to put all probabilities in a single column, and label by class 
  pivot_longer(cols = -dwage,
               names_to = "Class", 
               values_to = "p") %>%
  mutate(Class = ifelse(Class == "p_1",
                        "Class 1", 
                        "Class 2"))

# Plot
ggplot(df, aes(x = dwage)) +
  geom_line(aes(x = dwage,
                y = p,
                color = Class))
