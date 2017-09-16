
eval = function(a = NULL, b = NULL, c = NULL) {
  print(a)
  print(b)
  print(c)
}

getLog = function(...) {
  eval(...)
}

x = getLog(c = 6)
