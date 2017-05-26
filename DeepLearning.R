init.nn <- function(model.name, dat){
  # clean data
  dat <- dat[complete.cases(dat), ]
  model <- dbn.dnn.train(
    x = dat[, !grepl("b_", colnames(dat))],
    y = dat[, grepl("b_", colnames(dat))],
    hidden = c(250),
    learningrate_scale = .9,
    learningrate = .7,
    numepochs = 1,
    #activationfun = "linear",
    #hidden_dropout = .01,
    output = "sigm"
  )
  save(model, file=model.name)
}
update.nn <- function(model.name, dat){
  # clean data
  dat <- dat[complete.cases(dat), ]
  load(model.name)
  model <- nn.train(
    x = dat[1:100, !grepl("b_", colnames(dat))],
    y = dat[1:100, grepl("b_", colnames(dat))],
    initW = model$W,
    initB = model$B,
    hidden = c(250),
    learningrate_scale = .8,
    learningrate = .7,
    numepochs = 10
  )
  save(model, file=model.name)
}
buy.strategy <- function(model, game.state, my.money=NA){
  game.state <- rbind(game.state, game.state)
  load("card.info")
  prediction <- nn.predict(model, game.state[, !grepl("b_", colnames(game.state))])[1,]
  names(prediction) <- gsub("b_","",names(prediction))
  legal.buys <- gsub("s_","",names(which(game.state[1,] > 0 & grepl("s_",colnames(game.state)))))
  prediction <- prediction[names(prediction) %in% legal.buys]
  prediction <- sort(prediction, T)
  turn.data <- data.frame(
    card = names(prediction),
    priority = unname(prediction),
    cost = -1
    )
  for(card in 1:nrow(turn.data)){
    turn.data$cost[card] <- as.numeric(as.character(card.info$Cost[as.character(card.info$Singular) == as.character(turn.data$card[card])]))
  }
  if(!is.na(my.money)){
    turn.data <- subset(turn.data, turn.data$cost <= my.money)
  }
  turn.data$priority <- 100 * round(turn.data$priority/sum(turn.data$priority),2)
  return(turn.data)
}