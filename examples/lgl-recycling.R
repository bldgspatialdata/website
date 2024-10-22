# Logical vector recycling
# What do you expect? What do you get?

TRUE & c(FALSE, TRUE, FALSE)

TRUE && c(FALSE, TRUE, FALSE)

c(TRUE, TRUE, FALSE) && c(FALSE, TRUE, FALSE)

c(TRUE, TRUE, FALSE) & c(FALSE, TRUE, FALSE)

c(TRUE, TRUE, FALSE) & c(FALSE, TRUE)
