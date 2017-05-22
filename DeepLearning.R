init.nn <- function(model.name, dat){
  inputs <- dat[[1]]
  outputs <- dat[[2]]
  model <- dbn.dnn.train(
    x = inputs,
    y = outputs,
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
  inputs <- dat[[1]]
  outputs <- dat[[2]]
  load(model.name)
  model <- nn.train(
    x = inputs,
    y = outputs,
    initW = model$W,
    initB = model$B,
    hidden = c(250),
    learningrate_scale = .9,
    learningrate = .7,
    numepochs = 1
  )
  save(model, file=model.name)
}