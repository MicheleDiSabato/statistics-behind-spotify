mse_func = function(pred, true){
  mean((true-pred)^2)
}

mae_func = function(pred, true){
  mean(abs(true-pred))
}

mape_func = function(pred, true){
  # warning: unstable when true[i] ~ 0
  mean(abs(true-pred)/true)
}