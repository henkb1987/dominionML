# pre-reqs
library("deepnet")
source("DataFunctions.R")
source("DeepLearning.R")
load("card.info")

# obtain game state data from games
for(i in 1:160){
  cat("-------------------",i,"--------------------\n")
  load(paste0("games_",i))
  starts <- round(seq(1,length(tmp),length.out = 10))
  for(s in 2:length(starts)){
    if(!file.exists(paste0("all_states_",i,"_",letters[s - 1]))){
      cat("part",paste0("all_states_",i,"_",letters[s - 1]),"\n")
      all.states <- get.game.states.inoutput(tmp[starts[(s-1)]:starts[s]],do.parallel = T) 
      save(all.states, file=paste0("all_states_",i,"_",letters[s - 1]))
      rm(all.states)
      gc() 
    }
  }
}

# build a nn model and save it to disk
init.nn(model.name = "nn_model",dat = dat)

# load new data and update nn
load("games_2")
dat <- get.game.states.inoutput(tmp)
update.nn("nn_model", dat)