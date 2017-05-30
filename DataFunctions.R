library("plyr")
library(doSNOW)
library("snow")
get.supply <- function(game.data, include.std=T){
  if(!exists("card.info")){load("card.info")}
  supply <- regmatches(game.data, gregexpr("S:\\[.*?\\]", game.data))[[1]]
  supply <- c(as.numeric(strsplit(supply,"\\[|\\]|,")[[1]])[2:11]) + 1
  if(include.std){
    supply <- c(supply, get.std.cards())
  }
  return(supply)
}
get.std.cards <- function(){
  c(23,26,36,42,58,115,119,134)
}

get.decks.per.player <- function(game.data){
  p.count.start <- unname(gregexpr("::", game.data)[[1]])
  deck.pattern <- gregexpr("[0-9]{1,3}:[0-9]{1,3}", game.data)
  all.decks <- data.frame(player.id = NA,
                          deck.location = deck.pattern[[1]],
                          deck.text = regmatches(game.data, deck.pattern)[[1]],
                          card.number = NA,
                          card.amount = NA,
                          stringsAsFactors = F
  )
  # player id's
  for(p in 1:length(p.count.start)){
    all.decks$player.id[all.decks$deck.location > p.count.start[p]] <- p
  }
  # find supply
  supply <- get.supply(game.data)
  
  for(d in 1:nrow(all.decks)){
    dt <- as.numeric(strsplit(all.decks$deck.text[d],":")[[1]])
    decks.matrix[all.decks$player.id[d], (dt[1] + 1)] <- dt[2]
    decks.matrix[all.decks$player.id[d], 173 + (supply + 1)] <- 1
  }
  return(decks.matrix)
}
get.points.per.player <- function(game.data, normalize = T){
  points.at <- which(strsplit(game.data,"")[[1]]=="*")
  for(pl in 1:length(points.at)){
    points <- paste0(strsplit(game.data,"")[[1]][(points.at[pl]+2):(points.at[pl]+4)], collapse = "")
    points.at[pl] <- as.numeric(strsplit(points, ",")[[1]])
  }
  # normalize the points
  if(normalize){
    points.at <- points.at / max(points.at) 
  }
  points.at[is.na(points.at)]=0
  return(points.at)
}
winner <- function(game.data){
  which.max(get.points.per.player(game.data))
}
get.n.players <- function(game.data){
  return(sum(strsplit(game.data,"")[[1]]=="*"))
}
get.buy.orders <- function(game.data){
  player.at <- which(strsplit(game.data,"")[[1]]=="*")
  n.players <- length(player.at)
  
  buy.pattern <- gregexpr("b\\:\\[(([0-9]){1,3},?){1,100}\\]", game.data)
  buys <- regmatches(game.data, buy.pattern)[[1]]
  buys <- paste0(buys, collapse = "")
  buys <- strsplit(buys,"b\\:")[[1]]
  buys <- gsub("\\[|\\]","",buys)[2:length(buys)]
  
  buy.orders <- data.frame(player=0, turn=0, buys=buys, buy.location=buy.pattern[[1]], stringsAsFactors = F)
  for(p in 1:n.players){
    my.buys <- which(buy.orders$buy.location > player.at[p])
    buy.orders$player[my.buys] <- p
    buy.orders$turn[my.buys] <- 1:length(my.buys)
  }
  buy.matrix <- array(0, c(max(buy.orders$turn), 173, n.players))
  for(p in 1:n.players){
    if(sum(buy.orders$player == p) > 0)
      for(t in 1:max(buy.orders$turn[buy.orders$player == p])){
        my.buys <- as.numeric(strsplit(buy.orders$buys[buy.orders$player == p & buy.orders$turn == t], ",")[[1]])
        for(b in 1:length(my.buys)){
          buy.matrix[t, my.buys[b] + 1, p] <- buy.matrix[t, my.buys[b] + 1, p] + 1
        }
      }
  }
  return(list(buy.orders=buy.orders, buy.matrix=buy.matrix))
}
game.data.maker <- function(g){
  cbind(get.decks.per.player(tmp[g]), get.points.per.player(tmp[g]))
}
game.to.data.row <- function(game){
  # get the supply and the end deck for the winner
  return(tryNULL(get.decks.per.player(tmp[game])[which.max(get.points.per.player(tmp[game], F)), ]))
}
get.game.states <- function(game.data, real=F){
  if(!real){
    return(try_default(get.game.states(game.data,T),NULL,T))
  } else{
    n.players <- get.n.players(game.data)
    buys <- get.buy.orders(game.data)
    buy.orders <- buys$buy.orders
    buy.matrix <- buys$buy.matrix
    perspective <- winner(game.data)
    n.turns <- max(buy.orders$turn[buy.orders$player == perspective])
    last.turns <- rep(0, n.players)
    for(p in 1:n.players){
      last.turns[p] <- max(buy.orders$turn[buy.orders$player == p])
    }
    game.states <- array(0, dim = c(n.turns, 173, 4)) # current supply, my inv, mean opp inv, my.buys
    dimnames(game.states) <- list(
      paste("turn",1:(n.turns)),
      card.info$Singular,
      c("supply","my.inventory","opp.inventory","my.buys")
    )
    # init the supply at the start of the game
    # action cards
    game.states[, get.supply(game.data,F), "supply"] <- 10
    game.states[, get.std.cards()[c(2,5,6,8)], "supply"] <- rep(15,4)
    game.states[, get.std.cards()[c(1,3,4,7)], "supply"] <- rep(c(-1,8,12,12)[n.players],4)
    
    # walk through the game
    for(p in 1:n.players){
      # all players: adjust supply
      game.states[2:n.turns,,"supply"] <- game.states[2:n.turns,,"supply"] - apply(buy.matrix[1:(n.turns - 1),,p], 2, cumsum)
      # only opponent players: adjust opponent inventory
      if(p != perspective){
        game.states[2:n.turns,,"opp.inventory"] <- game.states[2:n.turns,,"opp.inventory"] + apply(buy.matrix[1:(n.turns - 1),,p], 2, cumsum)
      } else {
        # only perspective player: adjust my inventory
        game.states[2:n.turns,,"my.inventory"] <- apply(buy.matrix[1:(n.turns - 1),,p], 2, cumsum)
        # only perspective player: denote my buys
        game.states[1:n.turns,,"my.buys"] <- buy.matrix[1:n.turns,,p]
      }
    }
    # transform into matrix
    game.states <- matrix(game.states, ncol = 692)
    colnames(game.states) <- paste0(rep(c("s_","d_","o_","b_"),each=173),card.info$Singular)
    return(game.states)
  }
}
get.game.states.inoutput <- function(games, do.parallel=F){
  load("card.info")
  cat("Extracting game states ... ")
  
  if(do.parallel){
    cat("in parallel.\n")
    cl <- makeCluster(8, type = "SOCK")
    registerDoSNOW(cl)
    clusterExport(cl,"card.info")
    clusterCall(cl, source, file="DataFunctions.R")
    all.states <- llply(.data = games, .fun = get.game.states, .parallel = T, .progress = progress_time())
    stopCluster(cl)
  } else {
    cat("using standard plyr.\n")
    all.states <- llply(.data = games, .fun = get.game.states, .parallel = F, .progress = progress_time())
  }
  
  cat("Binding game states ...\n")
  all.states <- do.call(rbind, all.states)
  colnames(all.states) <- paste0(rep(c("s_", "d_", "o_", "b_"), each = 173), card.info$Singular)
  return(all.states)
}
adjust.money.supply <- function(all.states){
  all.states[, which(grepl("s_Copper|s_Silver|s_Gold|s_Platinum",colnames(all.states)))] <- runif(nrow(all.states), .95, 1.05)
  return(all.states)
}
clean.data <- function(dat){
  # clean data
  cat("Removing turns with NA data, ")
  dat <- dat[complete.cases(dat), ]
  cat("setting all money supplies to 1, ")
  dat <- adjust.money.supply(dat)
  cat("randomize row order,")
  dat <- dat[sample(1:nrow(dat)), ]
  cat("and normalize to 0-1 range.\n")
  for(i in which(grepl("s_|d_|o_",colnames(dat),T))){
    dat[, i] <-  dat[, i] / 10
  }
  return(dat)
}