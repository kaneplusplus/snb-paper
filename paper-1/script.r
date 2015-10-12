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

stairs = function (p, xstart, xend) {
    x = c(xstart, rep((xstart + 1):xend, each = 2))
    y = rep(0:(xend - xstart), each = 2)
    y = y[-length(y)]
    for (i in 1:(length(x) - 1)) {
        p = p + geom_segment(x = x[i], y = y[i], xend = x[i + 
            1], yend = y[i + 1], color = "green", linetype=1)
    }
    p
}

kplot = function(flips, s, t) {
  if (!is.list(flips)) {
    d = flips_to_kplot_df(flips)
    p = qplot(k, path, data = d, geom = "line") + 
      scale_x_continuous(breaks = 0:(t + s), limits = c(0, t + s)) + 
      scale_y_continuous(breaks = 0:s, limits=c(0, s+0.15)) + 
      geom_segment(x=s, y=s, xend=(t+s-1), yend=s, color="green", linetype=1) +
      geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, col="red")
  } else {
    flip_set = lapply(flips, flips_to_kplot_df)
    for (i in 1:length(flip_set)) {
      flip_set[[i]]$num = as.factor(i)
      flip_set[[i]]$k = jitter(flip_set[[i]]$k)
      flip_set[[i]]$k[flip_set[[i]]$k < 0] = 0
    }
    d = Reduce(rbind, flip_set)[, -(4:5)]
    p = qplot(k, path, data = d, geom = "path", group = num) + 
        scale_x_continuous(breaks=0:(t+s), limits = c(0, t+s)) + 
        geom_segment(x = s, y = s, xend = (t + s - 1), yend = s, 
                     color = "green") + 
        geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, col="red")
    p
  }
  p
}

flips_to_kplot_df = function (flips) {
    d = data.frame(k = 0:length(flips))
    d$head = c(0, cumsum(flips))
    d$tail = c(0, cumsum(1 - flips))
    d$headEnd = c(d$head[-1], NA)
    d$tailEnd = c(d$tail[-1], NA)
    d$path = c(0, cumsum(flips))
    d$k = 0:(nrow(d) - 1)
    d
}




X$head <- c(0, cumsum(flips))
X$tail<- c(0, cumsum(!(flips)))
X$headEnd <- c(X$head[-1], NA)
X$tailEnd <- c(X$tail[-1], NA)

X2 <- X[1:(min(which(X$headEnd >= s), which(X$tailEnd >= t))),]

p2 <- ggplot(data=X2) + #right, up, data=X) + #, geom="segment") +
  scale_x_continuous(breaks=0:t, limits=c(0, t)) +
  scale_y_continuous(breaks=0:s, limits=c(0, s), labels=as.character(0:s)) +
  geom_segment(mapping=aes(x=tail, y=head, xend=tailEnd,
    yend=headEnd), arrow=arrow()) +
  geom_segment(x=0, y=s, xend=t-1, yend=s, color="red") +
  geom_segment(x=t, y=0, xend=t, yend=s-1, color="green")

ggsave("ZeltermanPlot.pdf", p2, width=7, height=5)


p3 = kplot( c(rep(0, 3), 1, rep(0, 6), 1), s=2, t=11) + 
  xlab("Number of Patients Enrolled") + ylab("Number of Responders") +
  geom_text(data=NULL, x=7, y=2.1, label="Success Boundary") +
  geom_text(data=NULL, x=12, y=0.5, label="Failure Boundary", 
            angle=80)



ggsave("KanePlot.pdf", p3, width=7, height=5)

s=2
t=11
trial = c(0, 0, 1, 0, 0, 0, 0, 0)
p4 = cdsnb_stack_plot(trial, s, t)
ggsave("conditional_snb.pdf", p4, width=7, height=5)

cdsnb(trial, s, t)


approx_plots_start = list(
  dsnb_stack_plot(0.2, 15, 75) + scale_x_discrete(breaks=seq(15, 95, by=10)) + 
    labs(x="", y="", title="bimodal example (15, 75, .2)"),
  dsnb_stack_plot(0.35, 50, 50) + scale_x_discrete(breaks=seq(50, 105, by=10))+
    labs(x="", y="", title="approximate normal (50, 50, .35)"),
  dsnb_stack_plot(0.06, 10, 10) + scale_x_discrete(breaks=seq(10, 20, by=2))+
    labs(x="", y="", title="geometric (10, 10, .06)"),
  dsnb_stack_plot(0.06, 3, 175) + scale_x_discrete(breaks=seq(3, 180, by=30))+ 
    labs(x="", y="", title="gamma approximation (3, 175, .06)"),
  dsnb_stack_plot(0.5, 25, 25) + scale_x_discrete(breaks=seq(25, 50, by=5))+
    labs(x="", y="", title="lower half normal (25, 25, .5)"),
  dsnb_stack_plot(0.98, 175, 2) + scale_x_discrete(breaks=seq(2, 180, by=20))+
    labs(x="", y="", title="local mode at top of range (175, 2, .98)"))

library(foreach)
library(grid)

approx_plots = foreach(p=approx_plots_start) %do% {
  p + theme(legend.position="none") 
}

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

pdf("shapes.pdf", width=10, height=11)
grid.newpage()
p = pushViewport(viewport(layout = grid.layout(3, 2)))

k=1
for (i in 1:3) {
  for (j in 1:2) {
    print(approx_plots[[k]], vp=vplayout(i, j))
    k = k+1
  }
}
dev.off()


