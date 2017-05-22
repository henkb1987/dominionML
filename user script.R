# pre-reqs
library("deepnet")
source("DataFunctions.R")
source("DeepLearning.R")
load("card.info")

# load game data
load("games_1")

# obtain game state data from games
dat <- get.game.states.inoutput(tmp)

# build a nn model and save it to disk
init.nn(model.name = "nn_model",dat = dat)

# load new data and update nn
load("games_2")
dat <- get.game.states.inoutput(tmp)
update.nn("nn_model", dat)