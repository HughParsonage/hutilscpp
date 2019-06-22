mark <- function(x) {
  if (missing(x)) {
    cat(as.character(Sys.time()), "\n")
  } else {
    cat(as.character(START <- Sys.time()), '\tSTART\t', deparse(substitute(x)), '\n')
    eval.parent(substitute(x))
    cat(as.character(END <- Sys.time()), '\tEND  \t', as.numeric(difftime(END, START, unit = "s")), "\n")
  }
}
mark()
N <- 1e8
x <- rep_len(TRUE, N)
y <- rep_len(1:10 %in% c(1, 5, 8, 9), N)
z <- rep_len(1:20 %in% c(1, 5, 8, 9), N)
#
library(hutilscpp)
mark(length(which(x & y & z)))
mark(length(which(and3(x, y, z))))
mark(length(which3(x, y, z)))
mark(length(which3(x, y, z, prepare = TRUE)))
mark(length(hutilscpp:::do_which3_mem(x, y, z)))

N <- 1e8
x <- rep_len(TRUE, N)
y <- rep_len(1:10 %in% c(1, 5, 8, 9), N)
z <- rep_len(1:20 %in% c(2, 5, 8, 9), N)

mark(length(which(x & y & z)))
# mark(length(intersect(which(x), intersect(which(y), which(z)))))
mark(length(which(and3(x, y, z))))
mark(length(which3(x, y, z)))
mark(length(which3(x, y, z, prepare = TRUE)))
mark(length(hutilscpp:::do_which3_mem(x, y, z)))

N <- 1e8
x <- rep_len(TRUE, N)
y <- rep_len(1:10 %in% c(1, 5, 8, 9), N)
z <- rep_len(1:21 %in% c(2, 5, 8, 9), N)

i = 19999999
oi = 99999989

mark(length(which(x & y & z)))
mark(length(which(and3(x, y, z))))
mark(length(which3(x, y, z)))
mark(length(which3(x, y, z, prepare = TRUE)))
mark(length(hutilscpp:::do_which3_mem(x, y, z)))


