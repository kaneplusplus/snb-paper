library(ggplot2)

stairs <- function(p, xstart, xend) {
  x <- c(xstart, rep((xstart+1):xend, each=2))
  y <- rep(0:(xend-xstart), each=2)
  y <- y[-length(y)]
  for (i in 1:(length(x)-1)) {
    p <- p + geom_segment(x=x[i], y=y[i], xend=x[i+1], yend=y[i+1], 
      color="green")
  }
  p
}

s <- 3
t <- 10
flips <- rbinom(2*t, 1, 0.3)
x <- c(0, cumsum( flips))
X <- data.frame(list(X=x, k=0:(length(x)-1)))
X1 <- X[X$X <= s,]

p1 <- qplot(k, X, data=X1, geom="line") +
  scale_x_continuous(breaks=0:t, limits=c(0, t)) +
  scale_y_continuous(breaks=0:s, limits=c(0, s)) +
  geom_segment(x=0, y=s, xend=t, yend=s, color="red") + 
  geom_segment(x=t, y=0, xend=t, yend=(s-1), color="green")

ggsave("BernoulliProcess.pdf", p1, width=7, height=5)

X$head <- c(0, cumsum(flips))
X$tail<- c(0, cumsum(!(flips)))
X$headEnd <- c(X$head[-1], NA)
X$tailEnd <- c(X$tail[-1], NA)

X2 <- X[1:(min(which(X$headEnd >= s), which(X$tailEnd >= t))),]

p2 <- ggplot(data=X2) + #right, up, data=X) + #, geom="segment") +
  scale_x_continuous(breaks=0:t, limits=c(0, t)) +
  scale_y_continuous(breaks=0:s, limits=c(0, s)) +
  geom_segment(mapping=aes(x=tail, y=head, xend=tailEnd,
    yend=headEnd), arrow=arrow()) +
  geom_segment(x=0, y=s, xend=t-1, yend=s, color="red") +
  geom_segment(x=t, y=0, xend=t, yend=s-1, color="green")

ggsave("ZeltermanPlot.pdf", p2, width=7, height=5)

X3 <- X[1:(min(which(X$X >= s), which(X$tail >= t))),]
p3 <- qplot(k, X, data=X3, geom="line") +
  scale_x_continuous(breaks=0:(t+s), limits=c(0, t+s)) +
  scale_y_continuous(breaks=0:s, limits=c(0, s)) +
  geom_segment(x=0, y=s, xend=(t+s-1), yend=s, color="red") 

p3 <- stairs(p3, 10, 12)

ggsave("KanePlot.pdf", p3, width=7, height=5)

s=2
t=11
trial = c(0, 0, 1, 0, 0, 0, 0, 0)
fit = fit_flips(trial, s=s, t=t)
# Use the mode for the 
phat = (fit['shape1'] - 1) / (fit['shape1'] + fit['shape2'] - 2)
total_range = s+t-1
conditional_range = (length(trial):total_range)
conditional_s = s - sum(trial)
conditional_t = t - sum(1-trial)
# Use the mode unless we don't have enough data.
p_hat = if(fit[1] < 1 || fit[2] < 1) {
          fit[1] / (fit[1] + fit[2]) 
        } else {
          (fit[1] - 1) / (fit[1] + fit[2] - 2)
        }

step_offset = length(trial)
p4 = dsnb_stack_plot(p_hat, conditional_s, conditional_t, offset=step_offset)
ggsave("conditional_snb.pdf", width=7, height=5)
