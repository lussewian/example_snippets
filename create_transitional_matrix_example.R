##' Title: Multiple disadvantage Markov chain
##' Author: Wian Lusse
##' Peer review: Simon Anastasiadis
##' 
##' Creates a matrix of transitional probabilities.

library(dplyr)
library(diagram)
library(markovchain)

## Markov chain - Overall transition probabilities.

df <- tbl %>%
  select(snz_uid, age, crisis_cat) %>%
  arrange(snz_uid, as.numeric(age), crisis_cat) %>%
  group_by(snz_uid) %>%
  mutate(next_age = lead(age),
         next_state = lead(crisis_cat)) %>%
  filter(!is.na(next_age)) %>%
  ungroup()

probs <- df %>%
  count(crisis_cat, next_state, name = "n") %>%
  group_by(crisis_cat) %>%
  mutate(p = n / sum(n))

mat_prob <- xtabs(p ~ crisis_cat + next_state, data = probs)
mat_prob <- matrix(mat_prob, nrow = 3, ncol = 3, byrow = FALSE)

plotmat(t(mat_prob),
        pos = c(1,2),
        lwd = 1,
        box.lwd = 2,
        cex.txt = 0.8,
        box.size = 0.09,
        box.type = "circle",
        box.prop = 0.5,
        box.col = "light yellow",
        shadow.size = 0,
        arr.length = 0.1,
        arr.width = 0.1,
        self.cex = 0.4,
        self.shifty = -0.01,
        self.shiftx = 0.1279
        )

states <- as.character(sort(unique(df$crisis_cat)))
crisis_chain <- new("markovchain", states = states, transitionMatrix = mat_prob, name = "crisis chain")






























