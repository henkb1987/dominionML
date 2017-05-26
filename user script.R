# pre-reqs
library("deepnet")
source("DataFunctions.R")
source("DeepLearning.R")
source("gameLogic.R")
load("card.info")

# init a nn model and save it to disk
#load("all_states_1_a")
#init.nn(model.name = "nn_model",dat = all.states)

# go through all game state data and update model
#for(i in 1:160){
#  for(s in 1:9){
#    load(paste0("all_states_",i,"_",letters[s]))
#    cat("Learning from",paste0("all_states_",i,"_",letters[s]),"oh wow",sum(complete.cases(all.states)),"turns to learn from.\n")
#    update.nn("nn_model", all.states)
#    gc()
#  }
#}

# load base model
load("nn_model")
# play a random game with cards from base set and intrigue!
run.game()

