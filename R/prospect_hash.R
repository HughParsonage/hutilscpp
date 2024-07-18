
prospect_hash <- function(n) {
  .Call("C_prospect_hash", n, .Random.seed, PACKAGE = packageName())
}

