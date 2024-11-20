# **************************************************************
# filename: parameter_fns.R
# description: declares parameter functions
# author: Adrian Wiegman 
# revision date:  2020-05-15
# project: wetlandP
# repository: https://github.com/arhwiegman/wetlandP    
# notes:
# - need to add random selection and probability density functios
# - ___
# **************************************************************

fn_edit_parameter_values <- function(
  # this function allows for user interface to udpate parameter values
  ps = NULL
){
  if(is.null(ps)) ps <- parameters
  print(round(ps,3))
  require(svDialogs)
  a <- dlgInput("would you like to change a parameter value (y/n)? ", Sys.info()["user"])$res
  while(a == "y"){
    print(round(ps,3))
    b <- menu(names(ps),TRUE,title="select parameter")
    . <- paste("parameter name =",names(ps[b]),"\ncurrent value =",ps[b],"\nnew value =")
    c <- as.double((dlgInput(., Sys.info()["user"])$res))
    ps[b] <- c
    a <- dlgInput("would you like to change another parameter value (y/n)? ", Sys.info()["user"])$res
  }
  print("parameters updated...")
  return(ps)
}