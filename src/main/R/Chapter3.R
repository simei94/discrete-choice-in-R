library(tidyverse)
# functions for extreme value distributions
library(evd)

# Chapter 3: Fundamental concepts
#   Utility is dependent of attrbiutes related to the choice maker i and the alternative(choice) o, e.g.: V_o,i = V(cost_o, speed_o, income_i)
#   Analysts may fail to observe every single aspect of the decision making process, therefore a random component epsilon_o is needed:
#   U_o = V_o + epsilon_o

#  example on probability distribution functions (PDFs) -> random term in discrete choice analysis
# Define a function to return a value of one if 
# -L > x <= L and zero otherwise
uniform <- function(x, L){
  # Logical condition: x greater than minus L and less than L 
  ifelse(x > -L & x <= L,
         # Value if true 
         1/(2 * L),
         # Value if false 
         0)
}
# Define parameter L for the distribution 
L <- 2
# Create a data frame for plotting
# The function `seq()` is used to create a sequence of 
# values starting at `from` and ending at `to` with a
# step increase of `by`
df <- data.frame(x =seq(from = -(L+1),
                        to = L+1,
                        by = 0.01)) %>%
  # Mutate the data frame to add a new column with the 
  # value of the function
  mutate(f = uniform(x, L))
# Plot 
ggplot(data = df,
       # Map column x in the data frame to the x-axis 
       # and column f to the y-axis
       aes(x = x,
           y = f)) + 
  geom_area(fill = "orange",
            alpha = 0.5) +
  # Set the limits of the y axis
  ylim(c(0, 1/(2 * L) + 0.2 * 1/(2 * L))) + 
  # Draw a horizontal line for the x axis 
  geom_hline(yintercept = 0) +
  # Draw a vertical line for the y axis 
  geom_vline(xintercept = 0) +
  ylab("f(x)")

# the above function is a form of the uniform probability distribution:

# Define an upper limit for calculating the probability 
X <- 2
# Create a data frame for plotting the full distribution 
df <- data.frame(x =seq(from = -(L + 1),
                        to = L + 1,
                        by = 0.01)) %>%
  # Mutate the data frame to add a new column with the 
  # value of the function
  mutate(f = uniform(x, L))
# Create a data frame for plotting the portio of the 
# distribution that is less than X
df_p <- data.frame(x =seq(from = -(L + 1),
                          to = X,
                          by = 0.01)) %>%
  mutate(f = uniform(x, L))
# Plot 
ggplot() +
  # Use data frame `df` to plot the full distribution 
  geom_area(data = df,
            # Map column x in the data frame to the x-axis 
            # and column f to the y-axis
            aes(x = x,
                y = f),
            fill = "orange",
            alpha = 0.5) +
  # Use data frame `df_p` to plot area under the curve to X
  geom_area(data = df_p,
            # Map column x in the data frame to the x-axis
            # and column f to the y-axis 
            aes(x = x,
                y = f),
            fill = "orange",
            alpha = 1) +
  # Set the limits of the y axis
  ylim(c(0,
         1/(2 * L) + 0.2 * 1/(2 * L))) +
  # Draw a horizontal line for the x axis 
  geom_hline(yintercept = 0) +
  # Draw a horizontal line for the y axis 
  geom_vline(xintercept = 0) + 
  ylab("f(x)")

# with a given PDF a CDF can be formulated (cumulative distribution function) -> mapping of how does probability change if we change the interval.
# F(x) = P(x <= X)


# Define the cumulative distribution function 
punif <- function(x, L){
  # Logical statement: x less or equal to -L 
  ifelse(x <= -L,
         # Value if true
         0,
         # If false, check whether x is between -L and L 
         ifelse(x > -L & x <= L,
                # Value if true 
                (x + L)/(2 * L), 
                # Value if false 
                1))
}

# Create a data frame for plotting the cumulative distribution function 
df <- data.frame(x =seq(from = -(L+1),
                        to = L+1,
                        by = 0.01)) %>%
  mutate(f = punif(x, L))
# Plot 
ggplot(data = df,
       # Map column x in the data frame to the x-axis 
       # and column f to the y-axis
       aes(x = x,
           y = f)) +
  # Add geometric object of type step to the plot to
  # plot cumulative distribution function 
  geom_step(color = "orange") +
  ylim(c(0, 1)) + # Set the limits of the y axis 
  geom_hline(yintercept = 0) + # Add y axis 
  geom_vline(xintercept = 0) + # Add x axis 
  ylab("F(x)") # Label the y axis

# key takeaway: given a valid probability distribution function the probability that a random variable x <= X (uopper bound) is the area under the
# curve in the interval -eternity to X.

# Exercises
# 1. Deﬁne utility.
#   The sum of an alternatives systematic utility + random term dependent on the choice maker and the alternative's attributes
# 2. Describe in your own words the behavior described by utility maximization.
#   Quantifying of the single attributes of each alternative + attributes of the decision maker
# 3. What conditions are necessary for a function to be a valid probability distribution function?
#   f(x) must always be positive and the area under the curve (f(x)) must equal 1

# Consider the function shown in Fig. 3.6. This is called the triangle or tent function.
# 4. Show that the triangle function in the ﬁgure is a valid probability distribution function.
#   for B = 2: f(-2) = 0, f(2) = 0, f(1/2) = 0.5 => f(x) always positive
#   area unter curve = b*h/2 = 4 * 0.5 / 2 = 1

# WRONG, update here:
#(1) f(x)= 0 if x <= -B
# (2) f(x)= 1/B^2*x + 1/B if -B<x<=0
#(3) f(x)= -1/B^2*x + 1/B if 0<x<B
#(4) f(x)=0 if x >=B

# 5. Next, consider the following utility functions for two alternatives, namely i and
# j:
#   Ui = Vi + eps_i 
# Uj = Vj + eps_j
# Assume that the difference between the error terms below follows the triangle 
# distribution:
#   eps_q = eps_i − eps_j
# 
# Parting from the assumption above, derive a binary choice model for the probability 
# of selecting alternative j.
#   Pj = P(eps_i - eps_j < Vj - Vi)
#  ???????????????

# WRONG, we need to integrate f(x)
#(1) F(x)=0
#(2) F(x)= 1/B^2 * x^2/2 + 1/B * x + 1/2
# (3) F(x)= -1/B^2 * x^2/2 + 1/B * x + 1/2
# (4) F(x)= 1

