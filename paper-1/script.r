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

#p2 <- ggplot(data=X2) + #right, up, data=X) + #, geom="segment") +
#  scale_x_continuous(breaks=0:t, limits=c(0, t)) +
#  scale_y_continuous(breaks=0:s, limits=c(0, s), labels=as.character(0:s)) +
#  geom_segment(mapping=aes(x=tail, y=head, xend=tailEnd,
#    yend=headEnd), arrow=arrow()) +
#  geom_segment(x=0, y=s, xend=t-1, yend=s, color="red") +
#  geom_segment(x=t, y=0, xend=t, yend=s-1, color="green")

#ggsave("ZeltermanPlot.pdf", p2, width=7, height=5)

p3 = kplot( c(rep(0, 3), 1, rep(0, 6), 1), s=2, t=11) + 
  xlab("Number of Patients Enrolled") + ylab("Number of Responders") +
  geom_text(data=NULL, x=7, y=2.1, label="Success Boundary") +
  geom_text(data=NULL, x=12, y=0.5, label="Failure Boundary", 
            angle=(45+90)/2+9)

ggsave("KanePlot.pdf", p3, width=7, height=5)

# Regions s < t
s = 3
t = 5
flips = rep(0., t)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
rb = gg_color_hue(3)

x = data.frame(list(x = c(s, t, t, s), y = c(0, 0, s, s), value=2, id="s"))
x = rbind(x, data.frame(list(x = c(t, s+t-1, s+t-1, t), 
                          y = c(0, s-1, s, s), value=1, id="st")))
x$id = factor(x$id, levels=c("s", "t", "st"))

p4 = ggplot(x, aes(x=x, y=y, group=id)) + 
  geom_polygon(aes(fill=id, id = id), alpha=0.4) + 
  scale_fill_manual(values=c("s"=rb[1], "t"=rb[2], "st"=rb[3])) +
  theme(legend.position="none") +
  geom_segment(x=s, y=s, xend=(t+s-1), yend=s, color="green", linetype=1,
               size=1) +
  geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, col="red", size=1) +
  annotate("text", x=4, y=2.25, label="S(k, p, s)") +
  annotate("text", x=6, y=2.25, label="S(k, p, s) + S(k, 1-p, t)") + 
  ylab("Number of Responders") + xlab("Number of Patients Enrolled")
  
ggsave("Region-s-lt-t.pdf", p4, width=7, height=5)

# Region s > t
s = 5
t = 3
flips = rep(0., t)
x = data.frame(list(x = c(s, s+t-1, s+t-1, s), 
                    y = c(s-t, s-1, s, s), value=3, id="st"))
x = rbind(x, data.frame(list(x = c(t, s, s, t), 
                          y = c(0, s-t, s, s), value=1, id="t")))
x$id = factor(x$id, levels=c("s", "t", "st"))

p5 = ggplot(x, aes(x=x, y=y, group=id)) + 
  geom_polygon(aes(fill=id, id = id), alpha=0.4) + 
  scale_fill_manual(values=c("s"=rb[1], "t"=rb[2], "st"=rb[3])) +
  theme(legend.position="none") +
  geom_segment(x=s, y=s, xend=(t+s-1), yend=s, color="green", linetype=1,
               size=1) +
  geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, col="red", size=1) +
  annotate("text", x=4, y=4, label="S(k, 1-p, t)") +
  annotate("text", x=6, y=4, label="S(k, p, s) + S(k, 1-p, t)") + 
  ylab("Number of Responders") + xlab("Number of Patients Enrolled")
ggsave("Region-s-gt-t.pdf", p5, width=7, height=5)

# Region s = t
s = 5
t = 5
x = data.frame(list(x = c(s, s+t-1, s+t-1, s), 
                    y = c(0, s-1, s, s), value=1, id="st"))
x$id = factor(x$id, levels=c("s", "t", "st"))
p6 = ggplot(x, aes(x=x, y=y, group=id)) + 
  geom_polygon(aes(fill=id, id=id), alpha=0.4) + 
  scale_fill_manual(values=c("s"=rb[1], "t"=rb[2], "st"=rb[3])) +
  theme(legend.position="none") +
  geom_segment(x=s, y=s, xend=(t+s-1), yend=s, color="green", linetype=1,
               size=1) +
  geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, col="red", size=1) +
  annotate("text", x=7, y=4, label="S(k, p, s) + S(k, 1-p, t)") +
  ylab("Number of Responders") + xlab("Number of Patients Enrolled")
ggsave("Region-s-eq-t.pdf", p6, width=7, height=5)


d = snb:::flips_to_kplot_df(flips)
qplot(k, path, data = d, geom = "blank") +
  scale_x_continuous(breaks = 0:(t + s), limits = c(0, t + s)) +
  scale_y_continuous(breaks = 0:s, limits=c(0, s+0.15)) +
  geom_segment(x=s, y=s, xend=(t+s-1), yend=s, color="green", linetype=1) +
  geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, col="red") 


s=2
t=11
trial = c(0, 0, 1, 0, 0, 0, 0, 0)
p4 = cdsnb_stack_plot(trial, s, t)
ggsave("conditional_snb.pdf", p4, width=7, height=5)

cdsnb(trial, s, t)


approx_plots_start = list(
  dsnb_stack_plot(0.2, 15, 75) + scale_x_discrete(breaks=seq(15, 95, by=10)) + 
    labs(x="", y="", title="bimodal example (0.2, 15, 75)"),
  dsnb_stack_plot(0.35, 50, 50) + scale_x_discrete(breaks=seq(50, 105, by=10))+
    labs(x="", y="", title="approximate normal (0.35, 50, 50)"),
  dsnb_stack_plot(0.06, 10, 10) + scale_x_discrete(breaks=seq(10, 20, by=2))+
    labs(x="", y="", title="geometric (0.06, 10, 10)"),
  dsnb_stack_plot(0.06, 3, 175) + scale_x_discrete(breaks=seq(3, 180, by=30))+ 
    labs(x="", y="", title="gamma approximation (0.06, 3, 175)"),
  dsnb_stack_plot(0.5, 25, 25) + scale_x_discrete(breaks=seq(25, 50, by=5))+
    labs(x="", y="", title="lower half normal (0.5, 25, 25)"),
  dsnb_stack_plot(0.98, 175, 2) + scale_x_discrete(breaks=seq(2, 180, by=20))+
    labs(x="", y="", title="local mode at top of range (0.98, 175, 2)"))

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


