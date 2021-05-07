library("tidyverse")
library("kableExtra")
library("ordinal")
library("broom")

df_tidy<- read.csv("~/Desktop/finalds4ling/Data_tidy_raw/data_tidy1.csv")
knitr::kable(head(df_tidy), caption = 'Table 1') %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

# Descriptive statistics

d1 <- df_tidy %>%
  group_by(Blocking) %>%
  summarise(mean = formatC(mean(rating)),
            SD = formatC(sd(rating)),
            N = n())
d2 <- df_tidy %>%
  group_by(Identity) %>%
  summarise(mean = formatC(mean(rating)),
            SD = formatC(sd(rating)),
            N = n()) 
d3 <- df_tidy %>%
  group_by(dia) %>%
  summarise(mean = formatC(mean(rating)),
            SD = formatC(sd(rating)),
            N = n())
knitr::kable(list(d1, d2, d3)) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

# Violin plots

df_tidy %>%
  ggplot(., aes(x = Blocking, y= rating)) + # set up varibles 
  geom_violin() + # make a plot 
  stat_summary(mapping = aes(x = Blocking, y = rating),fun.min = min, fun.max = max, fun = median) +
  labs(x = "Blocking", y = "Ratings", title = "") +
  theme_grey(base_family = "Times", base_size = 10)

df_tidy %>%
  ggplot(., aes(x = Identity, y= rating)) + # set up varibles 
  geom_violin() + # make a plot
  stat_summary(mapping = aes(x = Identity, y = rating),fun.min = min, fun.max = max, fun = median) +
  labs(x = "Identity", y = "Ratings") +
  theme_grey(base_family = "Times", base_size = 10)

df_tidy %>%
  ggplot(., aes(x = dia, y= rating)) + # set up varibles 
  geom_violin() + 
  stat_summary(mapping = aes(x = dia, y = rating),fun.min = min, fun.max = max, fun = median) + # make a plot
  labs(x = "Dialectual background", y = "Ratings") +
  theme_grey(base_family = "Times", base_size = 10)

# boxplots

df_tidy %>%
  ggplot(aes(x = id, y = rating, fill = id)) +
  geom_boxplot() +
  labs(x = "Paticipants", y = "Ratings") +
  theme_grey(base_family = "Times", base_size = 10) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = -0.2))

# Cumulative link mixed model

fm1 <- clm(as.factor(rating) ~ 1, data = df_tidy)
fm2 <- clm(as.factor(rating) ~ Blocking, data = df_tidy)
fm3 <- clm(as.factor(rating) ~ Blocking + Identity , data = df_tidy)
fm4 <- clm(as.factor(rating) ~ Blocking + Identity + dia, data = df_tidy)
fm5 <- clmm(as.factor(rating) ~ Blocking + Identity + dia + (1|id), data = df_tidy)
knitr::kable(anova(fm1,fm2,fm3,fm4,fm5))

tidy(fm4) %>%
  knitr::kable()



