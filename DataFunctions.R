get.supply <- function(game.data, include.std=T){
  if(!exists("card.info")){load("card.info")}
  supply <- regmatches(game.data, gregexpr("S:\\[.*?\\]", game.data))[[1]]
  supply <- c(as.numeric(strsplit(supply,"\\[|\\]|,")[[1]])[2:11])
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
  player.at <- which(strsplit(game.data,"")[[1]]=="N")
  return(length(player.at))
}
get.buy.orders <- function(game.data){
  player.at <- which(strsplit(game.data,"")[[1]]=="N")
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
  return(buy.orders)
}
game.data.maker <- function(g){
  cbind(get.decks.per.player(tmp[g]), get.points.per.player(tmp[g]))
}
game.to.data.row <- function(game){
  # get the supply and the end deck for the winner
  return(tryNULL(get.decks.per.player(tmp[game])[which.max(get.points.per.player(tmp[game], F)), ]))
}
