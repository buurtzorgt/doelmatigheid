library(ggplot2)
library(tidyverse)
library(lme4)
theme_set(theme_classic())
rbind(
  data.frame(zorggebruik = c(rep(10, 5),rep(5, 5)),
         tijd = c(1:10),
         client = "A"),

  data.frame(zorggebruik = c(rep(5, 4), rep(8, 6)),
         tijd = c(1:10),
         client = "B")) %>% 
  ggplot(aes(x = tijd, y = zorggebruik, linetype = client)) + geom_line() + 
  coord_cartesian(ylim = c(0, 12)) + 
  scale_x_continuous(breaks=seq(0, 10, 3)) + 
  scale_y_continuous(breaks=seq(0, 12, 2)) + 
  labs(x = "Maanden", y = "Zorggebruik in uren per maand", title = "De kracht van longitudinale modellen") + 
  geom_segment(aes(x = 2, y = 5 + 0.2, xend = 2 , yend = 10 - 0.2, colour = "segment")) +
  geom_segment(aes(x = 2, y = 3, xend = 8 , yend = 3, colour = "segment"), arrow = arrow()) +
  annotate("text", x = 3, y = 7.5, label = "Intra-behandeling \n variabiliteit") + 
  annotate("text", x = 5, y = 2, label = "Inter-behandeling \n variabiliteit") + 
  annotate("text", x = 7, y = 10, label = "Belangrijke gebeurtenissen \n tijdens de behandeling") + 
  geom_segment(aes(x = 6, y = 10, xend = 5.1 , yend = 10), arrow = arrow()) +
  geom_segment(aes(x = 6, y = 10, xend = 4.1 , yend = 5), arrow = arrow()) +
  theme(legend.position = "none", text = element_text(size=15))




library(gridExtra)

time = 1:10

plot_evols = function(time, intercept, letter, seed){
  set.seed(123456  + seed)
  slope = rnorm(1, sd =0.2)
  ict = rnorm(1, mean = 5, sd = 1)
  outcome = ict + time*slope + rnorm(10, sd = 0.2)
  
  client = paste("Cliënt", letter)
  data.frame(time = time, outcome = outcome, client = client)
}

clients = LETTERS[seq( from = 1, to = 9)]
data = list()
for (i in c(1:9)){
  data[[i]] = plot_evols(time = time, intercept = i*1, letter = clients[i], seed = i)
}
  
data_example = do.call("rbind", data)


lmmodel = lmer(outcome ~ time + (time|client), data = data_example)
data_example$mixed_fitted = predict(lmmodel, data_example)

line_data = data.frame(outcome_mixed = c(lmmodel@beta[1] + lmmodel@beta[2]*time),
                       time = 1:10)

ggplot() + 
  geom_point(data = data_example, aes(x = time, y = outcome, group = client, color = client)) +
  labs(y = "Uitkomst", x = "Tijd", color = NULL, titl = "Longitudinaal model") + 
  theme(legend.position = "none") 

ggplot() + 
  geom_point(data = data_example, aes(x = time, y = outcome, group = client, color = client)) +
  geom_line(data=line_data, aes(x = time, y = outcome_mixed), size = 2) + 
  geom_line(data=data_example, aes(x = time, y = mixed_fitted, group = client)) + 
  labs(y = "Uitkomst", x = "Tijd", color = NULL, title = "Longitudinaal model") + 
  theme(legend.position = "none")


data_example %>% filter(time > 9) %>%
  ggplot() + 
  geom_point(aes(x = time, y = outcome, group = client, color = client)) +
  labs(y = "Uitkomst", x = "Tijd", color = NULL, title = "Dwarsdoorsnede") + 
  theme(legend.position = "none") + 
  coord_cartesian(xlim = c(0, 10))

data_example %>% 
  ggplot(aes(x = time, y = outcome)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(.~client, ncol = 3) + 
  labs(y = "Uitkomst", x = "Tijd", color = NULL, title = "Level 1")
  











