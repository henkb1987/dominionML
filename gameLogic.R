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
    kingdom = c("Copper", "Silver", "Gold", "Platinum", "Estate", "Duchy", "Province", kingdom)
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
    if(game.data[1, paste0("s_",buys[b])] > 0){
      game.data[1, paste0("d_",buys[b])] <- game.data[1, paste0("d_",buys[b])] + 1
    }
  }
  return(game.data)
}
add.opp.buy <- function(game.data, buys, n.players){
  for(b in 1:length(buys)){
    if(!grepl("s_Copper|s_Silver|s_Gold|s_Platinum",paste0("s_",buys[b]),t)){
      game.data[1, paste0("s_",buys[b])] <- game.data[1, paste0("s_",buys[b])] - 1
    }
    if(game.data[1, paste0("s_",buys[b])] > 0){
      game.data[1, paste0("o_",buys[b])] <- game.data[1, paste0("o_",buys[b])] + 1 / (n.players - 1)
    }
  }
  return(game.data)
}
run.game <- function(game = init.game(), buy.sampled=T, do.auto=T){
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
      if(!do.auto){
        paint.kingdom(game)
      }
      cat("My turn! I expect in opening hand $", get.money(game$game.data),",")
      if(do.auto){
        my.money <- get.money(game$game.data, T)
        cat("got",my.money,".\n")
      } else {
        my.money=as.numeric(readline("Please do your actions. How much money do you have?\n"))
      }
      my.options <- buy.strategy(model, game$game.data, my.money)
      if(buy.sampled){
        my.buy <- as.character(sample(my.options$card, 1, T, my.options$priority))
      } else {
        my.buy <- as.character(my.options$card[1]) 
      }
      game$game.data <- add.my.buy(game$game.data, my.buy)
      if(!do.auto){
        cat("These were my options:\n")
        print(my.options)
      }
      cat("I will buy: ",my.buy," in turn",turn,"\n")
      player <- player + 1
    } else {
      cat("It is the turn of",game$players[player],".\n")
      if(do.auto){
        opp.buy <- big.money.opponent(game$game.data)
      } else {
        cat("What did that person buy? Insert a number:\n",paste0(1:length(game.buy.options),":",game.buy.options))
        opp.buy <- as.numeric(readline("?\n"))
      }
      game$game.data <- add.opp.buy(game$game.data, game.buy.options[opp.buy], length(game$players))
      player <- player + 1
    }
    if(player > length(game$players)){
      player <- 1
      turn <- turn + 1
    }
    # end conditions (currently only based on provinces)
    if(game$game.data[, "s_Province"] <= 0 | turn >= 20){
      do.stop <- T
      paint.kingdom(game)
      print.points.per.player(game$game.data)
      cat("GAME END.\n")
      return(game)
    }
  }
}
get.money <- function(game.state, real=F){
  if(!real){
    n.cards <- sum(game.state[1, grepl("d_", colnames(game.state), T)])
    total.value <- game.state[1, "d_Copper"] + 2 * game.state[1, "d_Silver"] + 3 * game.state[1, "d_Gold"] + 5 * game.state[1, "d_Platinum"]
    return(5 * total.value / n.cards)
  } else {
  cards.in.deck <- sum(game.state[, grepl("d_", colnames(game.state), T)])
  money.cards <- sum(game.state[, grepl("d_Copper|d_Silver|d_Gold|d_Platinum", colnames(game.state), T)])
  money <- sum(
    sample(
      c(
        rep(0, cards.in.deck - money.cards),
        rep(1, game.state[, "d_Copper"]),
        rep(2, game.state[, "d_Silver"]),
        rep(3, game.state[, "d_Gold"]),
        rep(5, game.state[, "d_Platinum"])
      ),
      5,
      T
    )
  )
  }
  
}
evaluate.end.deck <- function(game.state){
  my.cards <- game.state[1, grepl("d_", colnames(game.state), T)]
  colnames(my.cards) <- gsub("d_", "", colnames(my.cards))
  out <- list(points=0, actions=0, cards=0, buys=0, money=0, budget=0)
}
print.points.per.player <- function(game.state){
  my.points <- game.state[, "d_Estate"] + game.state[, "d_Duchy"] * 3 + game.state[, "d_Province"] * 6
  opp.points <- game.state[, "o_Estate"] + game.state[, "o_Duchy"] * 3 + game.state[, "o_Province"] * 6
  cat("My points:",my.points,", Mean opponent points:",opp.points,"\n")
}
big.money.opponent <- function(game.state){
  # draw a hand and get money. Since we don't simulate
  # deck cycling, just do with replacement
  # c("Copper", "Silver", "Gold", "Platinum", "Estate", "Duchy", "Province"
  money <- sum(
    sample(
      c(
        rep(0, game.state[, "o_Estate"] + game.state[, "o_Duchy"] + game.state[, "o_Province"]),
        rep(1, game.state[, "o_Copper"]),
        rep(2, game.state[, "o_Silver"]),
        rep(3, game.state[, "o_Gold"])
      ),
      5,
      T
    )
  )
  if(money >= 8){return(7)}
  else if(money >= 6){return(3)}
  else if(money >= 3){return(2)}
  else{return(1)}
}