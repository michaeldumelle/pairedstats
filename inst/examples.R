n <- 60
n_var <- 3
n_group <- 5
group <- rep(1:n_group, each = n / n_group)
var <- rep(1:n_var, times = n / n_var)
val <- rnorm(n)
df <- data.frame(val, var, group)
head(df)

# defaults
test <- pairedstats(df, "var", "val", "group", .fun = "correlation")
summary(test)

# using ...
test <- pairedstats(df, "var", "val", "group", .fun = "correlation", method = "spearman")
summary(test)

# slim = FALSE
test <- pairedstats(df, "var", "val", "group", .fun = "correlation", .slim = FALSE)
summary(test)

# specify pair1
test <- pairedstats(df, "var", "val", "group", .fun = "correlation", .pair1 = c(1, 2))
summary(test)

# specify both
test <- pairedstats(df, "var", "val", "group", .fun = "correlation", .pair1 = c(1, 2), .pair2 = c(2, 3))
summary(test)

# .pairexpand = FALSE
test <- pairedstats(df, "var", "val", "group", .fun = "correlation", .pair1 = c(1, 2), .pair2 = c(2, 3), .pair_expand = FALSE)
summary(test)

# adjust p-value
test <- pairedstats(df, "var", "val", "group", .fun = "correlation")
summary(test, p.adjust = "bonferroni")

# different aggregate function
test <- pairedstats(df, "var", "val", "group", .fun = "correlation", .fun_aggregate = "min")
summary(test)
