DescribePrediction <- function(i, t=.5){
  kingdom <- end.decks[i, 174:ncol(end.decks)]
  kingdom <- gsub("s_","",names(kingdom[kingdom==1]))
  prediction <- model.predict[i,]
  #prediction <- round(prediction[prediction > t],1)
  names(prediction) <- gsub("d_","",names(prediction))
  strategy <- data.frame(
    name=c(kingdom,std.cards),
    amount=0,
    stringsAsFactors = F
    )
  for(k in 1:length(kingdom)){
    if(sum(names(prediction) == kingdom[k]) > 0){
      strategy$amount[k] <- try(prediction[names(prediction) == kingdom[k]])
    }
  }
  for(k in 1:length(std.cards)){
    strategy$amount[length(kingdom) + k] <- try(prediction[names(prediction) == std.cards[k]])
  }
  cat("Kingdom:",kingdom,"\n\n")
  print(prediction)
  cat("\n")
  return(strategy)
}

DescribePrediction(5,0)

base.predict <- nn.predict(model, input.slices)
