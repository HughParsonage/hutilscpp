string_equals <- hutilscpp:::string_equals

x <- c("svGwr", "Jlvu2", "o8u2a", "ToOWY", "fXg9I", "iDc4K", "LKVok",
       "z12V4", "VYRWX", "A0rmW")
y <- c("svGwr", "Jlvu2", "o8u2a", "ToOWY", "fXg9I", "iDc4K", "LKVok",
       "z12V4", "VYRWX", "A0rmW")
zz <- c("svGwr", "Jlvu2", "o8u2a", "ToOWY", "fXg9I", "iDc4K", "LKVok",
        "z12V4", "VYRWX", "A0rmW", "more")
zw <- c("svGwr", "Jlvu2", "o8u2a", "ToOWY", "fXg9I", "iDc4K", "LKVok",
        "z12V4", "VYRWX", "A0rm_")

expect_true(string_equals("", ""))
expect_false(string_equals(character(2), c("", " a")))
expect_true(string_equals(rep("a", 10), "a"))
expect_false(string_equals(c(rep("a", 10), "b"), "a"))
expect_true(string_equals(x, y))
expect_false(string_equals(x, zz))
expect_false(string_equals(x, zw))

