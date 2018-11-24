library(ggplot2)
hg <- read.csv("data/hgNov_23_2018.csv") %>% as_tibble() %>% 
  arrange(desc(accuracy)) %>%
  dplyr::select(-X, -balanced_accuracy) %>%
  filter(dataset == "g")

ggplot(hg, aes(x = folded_aspect_type, y=accuracy)) +
  geom_boxplot()
ggplot(hg, aes(x = folded_aspect_type, y=sensitivity)) +
  geom_boxplot()
ggplot(hg, aes(x = folded_aspect_type, y=specificity)) +
  geom_boxplot()

ggplot(hg, aes(x = elevation, y=accuracy)) +
  geom_boxplot()
ggplot(hg, aes(x = elevation, y=specificity)) +
  geom_boxplot()
ggplot(hg, aes(x = elevation, y=sensitivity)) +
  geom_boxplot()
ggplot(hg, aes(x = dataset, y=accuracy)) +
  geom_boxplot()

ggplot(hg, aes(x = as.factor(nodesize), y=accuracy)) +
  geom_boxplot()

ggplot(hg, aes(x = as.factor(mtry), y=accuracy)) +
  geom_boxplot()
ggplot(hg, aes(x = as.factor(sc), y=accuracy)) +
  geom_boxplot()

ggplot(hg, aes(x = elevation, y=accuracy, color= sc)) +
  geom_point(alpha=0.5)

