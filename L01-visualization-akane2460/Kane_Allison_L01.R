# L01-visualization ----
# Stat 301-1

# load packages and data ----
library(tidyverse)
library(skimr)

## load data

tinder_data <- read_csv("data/tinder_data.csv")

# clean tinder_data to remove NA

tinder_data <- na.omit(tinder_data)

# Exercises ----


### Ex 1 ----

# free response: in qmd

### Ex 2 ----

# exploring hwy vs. city scatterplot
ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point()

# exploring hwy vs. city scatterplot (jittered)
ggplot(data = mpg, aes(x = cty, y = hwy)) +
geom_jitter()

### Ex 3 ----

# color set to drv for hwy vs. city scatterplot
ggplot(data = mpg, aes(x = cty, y = hwy, color = drv)) +
  geom_point()

# build plot with facet_wrap based on "drv"
ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point(color = "#573ec7") +
  facet_wrap("drv", ncol = 1, scales = "fixed")

# short answer question  (see qmd)

### EX 4 ----

# mapping engine displacement to color
ggplot(data = mpg, aes(x = cty, y = hwy, color = displ)) +
  geom_point()

# mapping engine displacement to size
ggplot(data = mpg, aes(x = cty, y = hwy, size = displ)) +
  geom_point()

# mapping type of transmission to shape
ggplot(data = mpg, aes(x = cty, y = hwy, shape = trans)) +
  geom_point()

# short answer question (see qmd)

### EX 5 ----

# histogram
diamonds |>  
  ggplot(aes(carat)) +
    geom_histogram(bins = 60, color = "white") +
    labs(
      title = "Carat Distributions of 50,000 round cut Diamonds",
      y = NULL,
      x = "Carat",
      caption = "Source: ggplot2 package"
    )

# NOTE: command shift m is the shortcut for pipe


### EX 6 ----

# facet wrapped histogram
ggplot(data = diamonds, aes(x = carat)) +
  geom_histogram(stat = "bin", binwidth = .1) +
  facet_wrap(~ cut) +
  labs(
    title = "Carat Distributions of Diamonds",
    y = NULL,
    x = "Carat",
    caption = "Source: ggplot2 package"
  )

# see qmd for description

### EX 7 ----

# barplot
ggplot(data = diamonds, aes(x = cut)) +
  geom_bar(aes(fill = carat > .7)) +
  labs(
    title = "Proportion of Diamonds >.7 Carats for Different Cuts",
    y = NULL,
    x = "Cut",
    caption = "Source: ggplot2 package"
  )

### EX 8 ----

# see qmd

### Case Study ----

## univariate analysis ----

## summary stats ----
tinder_data |> 
  skim_without_charts(messages_received, messages_sent, user_gender)

# messages_sent ----
messages_sent_plot <-
  ggplot(data = tinder_data, aes(messages_sent)) +
    geom_boxplot()

# look at boxplot closer (not showing outlier values)
messages_sent_boxplot_clean <- 
  ggplot(data = tinder_data, aes(messages_sent)) +
    geom_boxplot() +
    scale_x_continuous(limits = c(0, 1486)) + # upper limit = the third quartile found in skim
    labs(
      title = "Distribution of Messages Sent from 1208 Tinder Users",
      x = "Messages Sent",
      caption = "Source: tinder_data"
    )

# save clean boxplot for messages_sent
ggsave(
  filename = "image/messages_sent_boxplot_clean.png",
  plot = messages_sent_boxplot_clean,
  width = 6,
  units = "in"
)

# look at a zoomed in density plot 
messages_sent_densityplot_clean <-
  ggplot(data = tinder_data, aes(messages_sent)) +
  geom_density(fill = "#9168d9") +
  scale_x_continuous(limits = c(0, 1486)) + # upper limit = the third quartile found in skim
  geom_vline(xintercept = 352) +
  annotate("text",
           label = "Median Messages Sent (352)",
           x = 375,
           y = .0015,
           angle = 90,
           size = 3, 
           color = "black") +
  labs(
    title = "Distribution of Messages Sent from 1208 Tinder Users",
    x = "Messages Sent",
    y = NULL,
    caption = "Source: tinder_data"
  )

# save clean density plot for messages_sent
ggsave(
  filename = "image/messages_sent_densityplot_clean.png",
  plot = messages_sent_densityplot_clean,
  width = 6,
  units = "in"
)

## messages_received----
ggplot(data = tinder_data, aes(messages_received)) +
  geom_boxplot()

# look at boxplot closer (not showing outlier values)
messages_received_boxplot_clean <- 
  ggplot(data = tinder_data, aes(messages_received)) +
  geom_boxplot() +
  scale_x_continuous(limits = c(0, 1426)) + # upper limit = the third quartile found in skim
  labs(
    title = "Distribution of Messages Recevied from 1208 Tinder Users",
    x = "Messages Received",
    caption = "Source: tinder_data"
  )

# save clean boxplot for messages_sent
ggsave(
  filename = "image/messages_received_boxplot_clean.png",
  plot = messages_received_boxplot_clean,
  width = 6,
  units = "in"
)

# look at a zoomed in density plot 
messages_received_density_plot_clean <-
  ggplot(data = tinder_data, aes(messages_received)) +
    geom_density(fill = "#9168d9") +
    scale_x_continuous(limits = c(0, 1426)) + # upper limit = the third quartile found in skim
    geom_vline(xintercept = 324) +
    annotate("text",
             label = "Median Messages Received (324)",
             x = 350,
             y = .0015,
             angle = 90,
             size = 3, 
             color = "black") +
    labs(
      title = "Distribution of Messages Received from 1208 Tinder Users",
      x = "Messages Sent",
      y = NULL,
      caption = "Source: tinder_data"
    ) 

# save clean density plot for messages_sent
ggsave(
  filename = "image/messages_received_density_plot_clean.png",
  plot = messages_received_density_plot_clean,
  width = 6,
  units = "in"
)

# gender of users----
# barplot of gender
gender_barplot <- 
  ggplot(data = tinder_data, aes(x = user_gender, 
                             fill = user_gender, 
                             color = user_gender)) +
    geom_bar() + 
    labs(
      title = "Gender of Tinder Users",
      x = "Gender",
      y = "Count",
      caption = "Source: tinder_data"
      )
# saving gender_barplot
ggsave(
  filename = "image/genderbarplot.png",
  plot = gender_barplot,
  width = 6,
  units = "in"
  )

## bivariate analysis ----

# objectives: who is sending more messages? who is receiving more? 
# is there a relationship between messages sent and messages received?

# scatterplot between messages sent and received

received_vs_sent <- 
  ggplot(tinder_data, aes(x = messages_sent, y = messages_received)) +
    geom_jitter(alpha = .4) +
    geom_smooth(se = FALSE, color = "#602e99") +
    labs(
      title = "Relationship Between Number of Messages Sent and Messages Received for 1208 users on Tinder",
      x = "Messages Sent",
      y = "Messages Received",
      caption = "Source: tinder data"
    )

# save received_vs_sent

ggsave(
  filename = "image/received_vs_sent.png",
  plot = received_vs_sent,
  width = 9,
  units = "in"
)

## multivariate analysis ----

received_vs_sent_m_f <-
  ggplot(tinder_data, aes(x = messages_sent, y = messages_received, color = user_gender)) +
    geom_jitter(alpha = .4) +
    geom_smooth(se = FALSE, color = "#602e99") +
    labs(
      title = "Relationship Between Number of Messages Sent and Messages Received for 1208 users on Tinder",
      x = "Messages Sent",
      y = "Messages Received",
      caption = "Source: tinder data",
      color = "User Gender"
    )

ggsave(
  filename = "image/received_vs_sent_m_f.png",
  plot = received_vs_sent_m_f,
  width = 9,
  units = "in"
)

# looking only at women
tinder_data_f <- filter(tinder_data, user_gender == "F")

received_vs_sent_f <-
  ggplot(tinder_data_f, aes(x = messages_sent, y = messages_received)) +
    geom_jitter(alpha = .5, color = "coral") +
    geom_smooth(se = FALSE, color = "#602e99") +
    labs(
      title = "Relationship Between Number of Messages Sent and Messages Received for 150 Female Users on Tinder",
      x = "Messages Sent",
      y = "Messages Received",
      caption = "Source: tinder data"
    )

ggsave(
  filename = "image/received_vs_sent_f.png",
  plot = received_vs_sent_f,
  width = 10,
  units = "in"
)


# looking only at men
tinder_data_m <- filter(tinder_data, user_gender == "M")

received_vs_sent_m <-
  ggplot(tinder_data_m, aes(x = messages_sent, y = messages_received, color = user_gender)) +
  geom_jitter(alpha = .5, color = "#51cbdb") +
  geom_smooth(se = FALSE, color = "#602e99") +
  labs(
    title = "Relationship Between Number of Messages Sent and Messages Received for 1058 Male Users on Tinder",
    x = "Messages Sent",
    y = "Messages Received",
    caption = "Source: tinder data",
    color = "User Gender"
  )

ggsave(
  filename = "image/received_vs_sent_m.png",
  plot = received_vs_sent_m,
  width = 10,
  units = "in"
)

# examining LGBT+ users messaging habits
tinder_data_lgbt <- filter(tinder_data, 
                           user_interested_in == "M and F" | user_interested_in == user_gender)
received_vs_sent_lgbt <-
  ggplot(tinder_data_lgbt, aes(x = messages_sent, y = messages_received, color = user_gender)) +
  geom_jitter(alpha = .5) +
  geom_smooth(se = FALSE, color = "#602e99") +
  labs(
    title = "Relationship Between Number of Messages Sent and Messages Received for 98 LGBT Users on Tinder",
    x = "Messages Sent",
    y = "Messages Received",
    caption = "Source: tinder data",
    color = "User Gender"
  )

ggsave(
  filename = "image/received_vs_sent_lgbt.png",
  plot = received_vs_sent_lgbt,
  width = 10,
  units = "in"
)

# examining messaging habits for LGBT+ women
tinder_data_lgbt_f <- filter(tinder_data_lgbt, user_gender == "F")

received_vs_sent_lgbt_f <-
  ggplot(tinder_data_lgbt_f, aes(x = messages_sent, y = messages_received, color = user_interested_in)) +
  geom_jitter(alpha = .5) +
  labs(
    title = "Relationship Between Number of Messages Sent and Messages Received for 47 Female LGBT+ Users on Tinder",
    x = "Messages Sent",
    y = "Messages Received",
    caption = "Source: tinder data",
    color = "Gender Interested In"
  )

ggsave(
  filename = "image/received_vs_sent_lgbt_f.png",
  plot = received_vs_sent_lgbt_f,
  width = 10,
  units = "in"
)

# examining messaging habits for LGBT+ men
tinder_data_lgbt_m <- filter(tinder_data_lgbt, user_gender == "M")

received_vs_sent_lgbt_m <-
  ggplot(tinder_data_lgbt_m, aes(x = messages_sent, y = messages_received, color = user_interested_in)) +
  geom_jitter(alpha = .5) +
  labs(
    title = "Relationship Between Number of Messages Sent and Messages Received for 51 Male LGBT+ Users on Tinder",
    x = "Messages Sent",
    y = "Messages Received",
    caption = "Source: tinder data",
    color = "Gender Interested In"
  )

ggsave(
  filename = "image/received_vs_sent_lgbt_m.png",
  plot = received_vs_sent_lgbt_m,
  width = 10,
  units = "in"
)





