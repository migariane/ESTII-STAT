num = 100000
n = round((0.5+6*runif(num)),0)
table(n/num)

num = 100000
n = round((0.5+6*runif(num)),0)
m = which(n==6)
s = c(m[1], diff(m))
mean(s)

library(ggplot2)

# number of times to repeat the experiment
iterration <-  10000 

# defining the doors
doors <- c("cabra","cabra","coche")

# initialize dataframe to store the result per iteration

monte_hall <- function(iteration){
  
  contestant_door <-  sample(doors, size = iteration, replace = TRUE)
  i=1:iteration
  
  #stick_win which is equal to 1 if the contestant_door in current i is coche, 0 cabra
  #switch_win which is equal to 0 if the contestant_door is equal to coche, 1 cabra
  stick_win  <- ifelse(contestant_door == 'coche',1,0)
  switch_win <- ifelse(contestant_door == 'coche',0,1)
  
  stick_prob  <- cumsum(stick_win)/i
  switch_prob <- cumsum(switch_win)/i
  
  #store result in a dataframe
  results <- data.frame(i=i, contestant_door=contestant_door, 
                        stick_win=stick_win,  switch_win=switch_win,
                        stick_prob=stick_prob, switch_prob=switch_prob)
  
  return(results)
}

monte_hall_results <- monte_hall(iter)

summary <- table(monte_hall_results$contestant_door)

df_summary <- data.frame(label = names(summary), count = matrix(summary))

print(df_summary)
head(monte_hall_results)
print(paste("Chance of winning if we always stick from our first choice: ", sum(monte_hall_results$stick_win)/iter))
print(paste("Chance of winning if we switch from our first choice: ", sum(monte_hall_results$switch_win)/iter))

g <- ggplot(monte_hall_results, mapping = aes(x=i, y=stick_prob))
g + geom_line(color="#3333ff") +
  geom_hline(yintercept = 1/3, color = '#8080ff') +
  geom_line(aes(y=switch_prob), color= "#ff751a") +
  geom_hline(yintercept = 2/3, color = '#ff944d') +
  ylab('Est.Probability')+xlab('Iteration') +
  geom_label(data = data.frame(label = c('switch', 'stick'), i = c(10000,10000), stick_prob = c(0.75,0.25)), 
             aes(label = label, color=label),
             show.legend = FALSE
  )+
  scale_color_manual(values = c("#3333ff", "#ff751a"))+
  ggtitle("Estimated Probability of Winning")

library(tidyverse)
classic_monty <- function() {
  # Assign the prize
  prize <- sample(1:3,1)
  # Pick a door
  choice <- sample(1:3,1)
  # Monty picks a door
  monty <- sample(c(1:3)[-c(choice,prize)], 1)
  return(ifelse(prize!=choice, "Switch", "Stick"))
}

n <- 2^(1:16)
runs <- data.frame(n=numeric(), switch=numeric())
for (trials in n) {
  run <- table(replicate(trials, classic_monty()))
  runs <- runs %>%  add_row(n=trials, switch=(sum(run["Switch"]))/trials)
}
runs[is.na(runs)]<-0
runs

# Bayesian perspective 
car=sample(3,10,replace=T)
car
# Which door is chosen:
door=sample(3,10,replace=T)
door
3 3 2 2 1 3 3 1 1 2
# Switch and win: the car is not behind the chosen door.
switchwin=(door!=car)
switchwin
sum(switchwin)/10
# Not switch and win: the car is behind the chosen door.
noswitchwin=(door==car)
noswitchwin
sum(noswitchwin)/10