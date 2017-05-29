# pre-reqs
library("deepnet")
source("DataFunctions.R")
source("DeepLearning.R")
source("gameLogic.R")
load("card.info")

# init a nn model and save it to disk
load("all_states_1_a")
all.states <- adjust.money.supply(all.states)
init.nn(model.name = "big_model_2",dat = all.states)

# go through all game state data and update model
for(i in 2:3){
  for(s in sample(1:9, 1)){
    load(paste0("all_states_",i,"_",letters[s]))
    all.states <- adjust.money.supply(all.states)
    cat("Learning from",paste0("all_states_",i,"_",letters[s]),"oh wow",sum(complete.cases(all.states)),"turns to learn from.\n")
    update.nn("big_model_2", all.states)
    load("big_model_2")
    stopifnot(!is.nan(model$L))
    rm(model)
    gc()
  }
}

# load base model
load("big_model_2")
# play a random game with cards from base set and intrigue!
run.game()

