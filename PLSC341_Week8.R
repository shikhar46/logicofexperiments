library(tidyverse)
library(ggrepel)
library(ggthemes)
library(patchwork)
library(randomizr)
library(texreg)
library(estimatr)

# Download the dataset
dat <-
  read_csv("w8_tea_coffee_experiment.csv")

# Create new columns for the analysis
# T1 = Coffee, T2 = Tea, T3 = Control

dat <- dat %>%
  mutate(
    Z = c(
      "T1",
      "T1",
      "T2",
      "T1",
      "T1",
      "T2",
      "T3",
      "T2",
      "T3",
      "T3",
      "T3",
      "T1",
      "T2",
      "T1",
      "T2",
      "T2",
      "T2",
      "T1",
      "T2",
      "T3",
      "T3",
      "T1",
      "T3",
      "T2",
      "T1",
      "T1",
      "T1",
      "T3",
      "T3",
      "T2",
      "T3",
      "T1",
      "T2",
      "T2",
      "T1",
      "T2",
      "T3",
      "T3",
      "T2",
      "T3",
      "T2",
      "T1",
      "T1",
      "T1",
      "T3",
      "T3",
      "T3",
      "T2"
    ),
    coffee = case_when(Z == "T1" ~ 1, Z != "T1" ~ 0),
    tea = case_when(Z == "T2" ~ 1, Z != "T2" ~ 0),
    Y = (Coder1 + Coder2 + Coder3 + Coder4 + Coder5) / 5,
    block = complete_ra(N = 48)
  )

##### Summaries #######

# Group Means 

group_means <- dat %>%
  group_by(Z) %>%
  summarise(
    tidy(lm_robust(Y ~ 1, data = cur_data()))
  )

# Group Means by Block
# There are two blocks X=1 and X=0

block_group_means <- dat %>%
  group_by(Z, block) %>%
  summarise(
    tidy(lm_robust(Y ~ 1, data = cur_data()))
  )

# Difference in Means (DIMs)

dim_coffee <- dat %>%
  filter(Z %in% c("T1","T3")) %>%
  summarise(
    tidy(difference_in_means(Y ~ coffee, data = cur_data()))
  )

dim_tea <- dat %>%
  filter(Z %in% c("T2","T3")) %>%
  summarise(
    tidy(difference_in_means(Y ~ tea, data = cur_data()))
  )

dims = bind_rows(dim_coffee, dim_tea)



# Conditional Average Treatment Effects
# (DIMs for block X=1 and block X=0)


cates_coffee <- dat %>%
  filter(Z %in% c("T1", "T3")) %>%
  group_by(block) %>% # this is the crucial step where DIMs are calculated separately for each block
  summarise(
    tidy(difference_in_means(Y ~ coffee, data = cur_data()))
  )

cates_tea <- dat %>%
  filter(Z %in% c("T2", "T3")) %>%
  group_by(block) %>%
  summarise(
    tidy(difference_in_means(Y ~ tea, data = cur_data()))
  )

cates = bind_rows(cates_coffee, cates_tea)


####### Visualizing Data ###########

# Template 1: Scatterplot with summary overlays


fig_scatterplot <- ggplot(data = dat,
                          aes(x = Z, y = Y)) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  geom_point(data = group_means,
             aes(x = Z, y = estimate),
             color = "red",
             size = 3) +
  geom_linerange(
    data = group_means,
    aes(
      x = Z,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high
    ),
    color = "red"
  ) +
  ylim(c(1, 7)) +
  xlab("Treatment Groups") +
  theme_bw()

# Template 2: Coefficient plots

fig_coefplot <- ggplot(data = dims,
                       aes(x = term, y = estimate)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  ylim(c(0, 5)) +
  xlab("Treatment-Control Comparison") +
  ylab("Difference in Means") +
  theme_bw()


# Combining the two figures using ggpatchwork

fig_results <- fig_scatterplot + fig_coefplot

# Template 3: Coefficient plots with block-level ATEs

ggplot(data = cates, # we use cates, not dims
       aes(x = term, y = estimate)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  ylim(c(0, 5)) +
  xlab("Treatment-Control Comparison") +
  ylab("Difference in Means") +
  facet_wrap( ~ block, nrow = 1) + # This is the crucial step where we create sep. coef plots for each block
  theme_bw()


# Template 4: Bar charts 

fig_barchart <- ggplot(data = block_group_means,
                       aes(x = Z, y = estimate)) +
  geom_bar(stat = "identity", position = position_dodge(0.5)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(0.5)) +
  facet_wrap(~block, nrow = 1) +
  xlab("Treatment Groups") +
  ylab("Average Outcome") +
  theme_bw()


####### Making Regression Tables #######

# Specify models using lm_robust or difference_in_means in estimatr package

m_water_v_coffee <-
  lm_robust(Y ~ coffee, data = dat, subset = Z %in% c("T1", "T3"))

m_water_v_tea <-
  lm_robust(Y ~ tea, data = dat, subset = Z %in% c("T2", "T3"))

m_coffee_v_tea <-
  lm_robust(Y ~ coffee, data = dat, subset = Z %in% c("T1", "T2"))


# Template 1: Make the table using screenreg, screenshot the R output

screenreg(list(m_water_v_coffee, m_water_v_tea, m_coffee_v_tea), 
          include.ci = FALSE,
          custom.model.names = c("Coffee v. Water", "Tea v. Water", "Coffee v. Tea"),
          caption = "Difference in Means",
          digits = 3)


# Template 2: Make the table using wordreg, and save it as a word document
# You can format the table in word as any other table

wordreg(
  list(m_water_v_coffee, m_water_v_tea, m_coffee_v_tea),
  include.ci = FALSE,
  custom.model.names = c("Coffee v. Water", "Tea v. Water", "Coffee v. Tea"),
  caption = "Difference in Means",
  caption.above = TRUE,
  digits = 3,
  file = "Regression_Tables.doc" # name of the word document where the table will be saved
)

# Template 3: Make the table using texreg, copy-paste the latex code into your Tex word processor


texreg(
  list(m_water_v_coffee, m_water_v_tea, m_coffee_v_tea),
  include.ci = FALSE,
  custom.model.names = c("Coffee v. Water", "Tea v. Water", "Coffee v. Tea"),
  caption = "Difference in Means",
  caption.above = TRUE,
  booktabs = TRUE,
  digits = 3
)

