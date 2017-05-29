# game logic
# central data is the game data which corresponds to 
# an input row in the all.states datasets
# a buy is represented with a name that is matched (case insensitive)
# a kingdom is a vector of names of cards
source("DataFunctions.R")

random.kingdom <- function(expansions="Dominion|Intrigue"){
  # all: "Dominion|Alchemy|Seaside|Cornucopia|Prosperity|Intrigue|Promotional|Hinterlands"
  load("card.info")
  eligible.cards <- card.info$Singular[grepl(expansions, card.info$Expansion, T)]
  return(as.character(sample(eligible.cards, 10)))
}
paint.kingdom <- function(game){
  cat("CARD: DECK/OPPONENT/SUPPLY\n")
  for(k in 1:length(game$kingdom)){
    s <- game$game.data[, paste0("s_",game$kingdom[k])]
    d <- game$game.data[, paste0("d_",game$kingdom[k])]
    o <- game$game.data[, paste0("o_",game$kingdom[k])]
    cat(game$kingdom[k],":",d,"/",o,"/",s,"\n")
  }
}
init.game <- function(kingdom = random.kingdom(), players = c("me","opponent")){
  game <-  list(
    game.data = init.game.data(kingdom, length(players)),
    players = players,
    kingdom = c("Copper", "Silver", "Gold", "Platinum", "Estate", "Duchy", "Province", "Silver", kingdom)
  )
  return(game)
}

init.game.data <- function(kingdom, n.players){
  game.data <- matrix(0, 1, 519)
  colnames(game.data) <- c(paste0("s_",card.info$Singular),paste0("d_",card.info$Singular),paste0("o_",card.info$Singular))
  # action cards
  for(card in 1:length(kingdom)){
    game.data[1, which(colnames(game.data) == paste0("s_",kingdom[card]))] <- 10
  }
  # money cards
  game.data[1, get.std.cards()[c(2,5,6,8)]] <- rep(1,4)
  # vp cards
  game.data[1, get.std.cards()[c(1,3,4,7)]] <- rep(c(-1,8,12,12)[n.players],4)
  # player starting hands
  game.data[1, "d_Copper"] <- 7
  game.data[1, "d_Estate"] <- 3
  game.data[1, "o_Copper"] <- 7
  game.data[1, "o_Estate"] <- 3
  return(game.data)
}
# buys is a vector, e.g. c("copper", "copper", "duke")
add.my.buy <- function(game.data, buys){
  for(b in 1:length(buys)){
    if(!grepl("s_Copper|s_Silver|s_Gold|s_Platinum",paste0("s_",buys[b]),t)){
      game.data[1, paste0("s_",buys[b])] <- game.data[1, paste0("s_",buys[b])] - 1
    }
    game.data[1, paste0("d_",buys[b])] <- game.data[1, paste0("d_",buys[b])] + 1
  }
  return(game.data)
}
add.opp.buy <- function(game.data, buys){
  for(b in 1:length(buys)){
    if(!grepl("s_Copper|s_Silver|s_Gold|s_Platinum",paste0("s_",buys[b]),t)){
      game.data[1, paste0("s_",buys[b])] <- game.data[1, paste0("s_",buys[b])] - 1
    }
    game.data[1, paste0("o_",buys[b])] <- game.data[1, paste0("o_",buys[b])] + 1 / (length(game$players) - 1)
  }
  return(game.data)
}
run.game <- function(game = init.game(), buy.sampled=F){
  cat("Starting game with this kingdom:",paste(game$kingdom, collapse = ", "),"\n")
  do.stop <- FALSE
  turn <- 1
  player <- 1
  #game.key <- paste0(sample(letters,50,T), collapse = "")
  if(!exists("model")){stop("No decision model found. Please load one.")}
  game.buy.options <- game$kingdom
  while(!do.stop){
    #save(game, file = game.key)    
    # backup game
    cat("Turn",turn,"...\n")
    # my turn: after knowing money, predict my buy
    if(player == 1){
      paint.kingdom(game)
      cat("My turn! I expect in opening hand $",expected.money(game$game.data),"\n")
      my.money=as.numeric(readline("Please do your actions. How much money do you have?\n"))
      my.options <- buy.strategy(model, game$game.data, my.money)
      if(buy.sampled){
        my.buy <- as.character(sample(my.options$card, 1, T, my.options$priority))
      } else {
        my.buy <- as.character(my.options$card[1]) 
      }
      game$game.data <- add.my.buy(game$game.data, my.buy)
      cat("These were my options:\n")
      print(my.options)
      cat("I will buy: ",my.buy,"!\n\n")
      player <- player + 1
    } else {
      cat("It is the turn of",game$players[player],".\n")
      cat("What did that person buy? Insert a number:\n",paste0(1:length(game.buy.options),":",game.buy.options))
      opp.buy <- as.numeric(readline("?\n"))
      game$game.data <- add.opp.buy(game$game.data, game.buy.options[opp.buy])
      player <- player + 1
    }
    if(player > length(game$players)){
      player <- 1
      turn <- turn + 1
    }
  }
}
expected.money <- function(game.state, me = T){
  if(me){
    n.cards <- sum(game.state[1, grepl("d_", colnames(game.state), T)])
    total.value <- game.state[1, "d_Copper"] + 2 * game.state[1, "d_Silver"] + 3 * game.state[1, "d_Gold"] + 5 * game.state[1, "d_Platinum"]
    return(5 * total.value / n.cards)
  } else {
    n.cards <- sum(game.state[1, grepl("o_", colnames(game.state), T)])
    total.value <- game.state[1, "o_Copper"] + 2 * game.state[1, "o_Silver"] + 3 * game.state[1, "o_Gold"] + 5 * game.state[1, "o_Platinum"]
    return(5 * total.value / n.cards)  
  }
}