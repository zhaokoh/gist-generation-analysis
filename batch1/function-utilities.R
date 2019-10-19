## @knitr function-utilities
as.character.factor <- function(x) {as.character(levels(x))[x]}
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

