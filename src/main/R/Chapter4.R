library(dplyr) # A Grammar of Data Manipulation
library(evd) # Functions for Extreme Value Distributions
library(ggplot2)

# Define parameters for the distribution 
# Location: moves the distribution on the x axis
mu <- 0
# Scale: scales the distribution
sigma <- 1

######################################### EV Type 1 Distr ##########################################

# Create a data frame for plotting; 
df <- data.frame(x =seq(from = -5,
                        to = 5,
                        by = 0.01)) %>%
  # The function `dgumbel()` is the EV Type I distribution 
  mutate(f = dgumbel(x,
                     # Location parameter 
                     loc = mu,
                     # Scale parameter 
                     scale = sigma))
# Plot 
ggplot(data = df,
       aes(x = x,
           y = f)) +
  geom_area(fill = "orange",
            alpha = 0.5) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) + 
  ylab("f(x)")

################################### normal vs logistic distr ##############################

# Create a data frame for plotting 
df <- data.frame(x =seq(from = -5,
                        to = 5,
                        by = 0.01)) %>%
  # Add columns with the values of the
  # `dlogis()` and `dnorm()` distributions 
  mutate(logistic = dlogis(x,
                           # Location parameter 
                           location = mu,
                           # Scale parameter 
                           scale = sigma),
         normal = dnorm(x,
                        # The location parameter of the normal
                        # distribution is the mean 
                        mean = mu,
                        # The scale parameter of the normal
                        # distribution is the standard deviation 
                        sd = sigma))

# Plot 
ggplot() +
  # Add geometric object of type area to plot the 
  # logistic distribution
  geom_area(data = df,
            aes(x = x,
                y = logistic),
            # The fill color of the logistic distribution 
            fill = "blue",
            alpha = 0.5) +
  # Add geometric object of type area to plot the 
  # normal distribution
  geom_area(data = df,
            aes(x = x,
                y = normal),
            # The fill color of the normal distribution 
            fill = "black",
            alpha = 0.5) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  ylab("f(x)") # Label the y axis

################################## logit probability ########################################
# Define an upper limit for calculating the probability;
# This equivalent to V_i - V_j.
# Negative values represent V_i < V_j, and positive values are V_j > V_i; 
# when V_j = V_i, then X = 0:
x <- -2
# Create data frames for plotting
df <- data.frame(x =seq(from = -6 + mu,
                        to = 6 + mu,
                        by = 0.01)) %>%
  mutate(y = dlogis(x,
                    location = mu,
                    scale = sigma)) 
df_p <- data.frame(x =seq(from = -6,
                          to = x,
                          by = 0.01)) %>%
  mutate(y = dlogis(x,
                    location = mu,
                    scale = sigma))
# Plot distribution function and the area under the curve 
ggplot(data = df,
       aes(x, y)) + 
  geom_area(fill = "orange",
            alpha = 0.5) + 
  geom_area(data = df_p,
            fill = "orange", 
            alpha = 1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  xlab(expression(paste(V[i] - V[j] - mu))) +
  ylab("f(x)")

############################### cumulative distr function ######################################
# Create a data frame for plotting
df <- data.frame(x =seq(from = -5 + mu,
                        to = 5 + mu,
                        by = 0.01)) %>%
  mutate(f = plogis(x,
                    location = mu,
                    scale = 1))
# Plot the cumulative distribution function 
logit_plot <- ggplot(data = df,
                     aes(x = x,
                         y = f)) +
  geom_line(color = "orange") + 
  ylim(c(0, 1)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)
logit_plot +
  xlab(expression(paste(V[i] - V[j] - mu))) +
  ylab(expression(paste(P[i])))

logit_plot +
  xlab(expression(paste(V[transit] - V[car] - mu))) +
  ylab(expression(paste(P[transit]))) + 
  annotate("segment",
           x = -3.75, xend = -2.5,
           y = 0.024, yend = 0.024, 
           colour = "blue", 
           linetype = "solid") +
  annotate("segment",
           x = -2.5, xend = -2.5,
           y = 0.024, yend = 0.075, 
           colour = "blue", 
           linetype = "solid") +
  annotate("segment",
           x = 0, xend = 1.25,
           y = 0.5, yend = 0.5, 
           colour = "red", 
           linetype = "dashed") +
  annotate("segment",
           x = 1.25, xend = 1.25,
           y = 0.5, yend = 0.77, 
           colour = "red", 
           linetype = "dashed")

######################### reference function #############################################
V_j <- -4 
V_k <- 8 
theta <- 0.8
theta * V_j - theta * V_k


#################### exercises #############################################################
# 1. What do we mean when we say that the logit probability has a closed form?
# Wahrscheinlichkeit, die mit expliziter mathematischer Formel mit endlicherAnzahl an Operatoren

# 2. Why is it that we can set the dispersion parameter in the logit probabilities to one?
# Weil der parameter nie das vorzeichen von 2 Nutzenvariablen ändern wird, er ändert beide Variablen
# gleichermaßen und kann somit auf 1 gesetzt / weggelassen werden.

# 3. Suppose that a choice set consists of two alternatives, travel by car (c) and travel
# by blue bus (bb). The utilities of these two modes are the same, that is
# Vc = Vbb
# What are the probabilities of choosing these two modes?
# sie müssen zusammen 1 ergeben also jeweils 0.5

#  Suppose that the transit operator of the blue buses in Question 3 decides to intro-
# duce a new service, namely a red bus. This red bus is identical to the blue bus 
# in every respect except the color. Under these new conditions, what are the logit 
# probabilities of choosing these modes?
# Attribute von Vc ändern sich nicht, also prob = 0.5 weiterhin.
# Prob für bb un rb sind gleich, also prob bb / prob rb = 1
# also 1 = prob c + 2* prob bb -> prob bb = 0.25




