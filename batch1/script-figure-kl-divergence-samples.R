P <- rnorm(10000, 3, .25)
Q <- rbeta(10000, 1, 2)

norm_P <- P/sum(P)
norm_Q <- Q/sum(Q)

kl_value_PQ <- KL(rbind(norm_P, norm_Q))
kl_value_QP <- KL(rbind(norm_Q, norm_P))

d <- data.table(cbind(norm_P, norm_Q))
melt_d <- melt(d, measure.vars = c("norm_P", "norm_Q"))

cols <- c("norm_P" = "red", "norm_Q" = "blue")

dp <- ggplot(melt_d, aes(x=value, color=variable)) + 
  geom_density() +
  xlab("") +
  ylab("") +
  scale_color_manual(name = "Distribution", values = cols, labels=c("P","Q")) +
  theme(axis.text = element_blank()) +
  ggtitle(sprintf("KL(P||Q) = %.4f, KL(Q||P) = %.4f", kl_value_PQ, kl_value_QP))


Q <- rnorm(10000, 3, .20)

norm_P <- P/sum(P)
norm_Q <- Q/sum(Q)

kl_value_PQ <- KL(rbind(norm_P, norm_Q))
kl_value_QP <- KL(rbind(norm_Q, norm_P))

d <- data.table(cbind(norm_P, norm_Q))
melt_d <- melt(d, measure.vars = c("norm_P", "norm_Q"))

cols <- c("norm_P" = "red", "norm_Q" = "blue")

dp2 <- ggplot(melt_d, aes(x=value, color=variable)) + 
  geom_density() +
  xlab("") +
  ylab("") +
  scale_color_manual(name = "Distribution", values = cols, labels=c("P","Q")) +
  theme(axis.text = element_blank()) +
  ggtitle(sprintf("KL(P||Q) = %.4f, KL(Q||P) = %.4f", kl_value_PQ, kl_value_QP))

p_grid <- plot_grid(dp2, dp, nrow=2, labels = c("A", "B"))
p_grid
ggsave("kl_divergence_examples.png")