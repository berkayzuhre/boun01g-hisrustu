# plumber.R

#* Echo back the input
#* @get /echo
function(msg="") {
  list(msg = paste0("IE48A-plumber message: This is a great tool for creating API's. -hisRüstü'", msg, "'"))
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
  as.numeric(a) + as.numeric(b)
}


