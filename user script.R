# pre-reqs
library("deepnet")
source("DataFunctions.R")
source("DeepLearning.R")
source("gameLogic.R")
load("card.info")

# init a nn model and save it to disk
load("all_states_1_a")
all.states <- adjust.money.supply(all.states)
init.nn(model.name = "two_layers",dat = all.states)

# go through all game state data and update model
for(i in sample(1:160)){
  for(s in sample(1:9)){
    gc()
    load(paste0("all_states_",i,"_",letters[s]))
    all.states <- adjust.money.supply(all.states)
    cat("Learning from",paste0("all_states_",i,"_",letters[s]),", oh wow!",sum(complete.cases(all.states)),"turns to learn from.\n")
    update.nn("two_layers", all.states)
    gc()
  }
}

# load base model
load("two_layers")
# play a random game with cards from base set and intrigue!
game<-run.game(do.auto = T,buy.sampled = F)

