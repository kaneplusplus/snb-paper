library(snb)
library(ggplot2)

# kplot

kplot = function(flips, s, t) {
  if (!is.list(flips)) {
    d = flips_to_kplot_df(flips)
    p = qplot(k, path, data = d, geom = "line") +
      scale_x_continuous(breaks = 0:(t + s), limits = c(0, t + s)) +
      scale_y_continuous(breaks = 0:s, limits=c(0, s+0.15)) +
      geom_segment(x=s, y=s, xend=(t+s-1), yend=s, linetype=2) +
      geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, linetype=2)
#      geom_segment(x=s, y=s, xend=(t+s-1), yend=s, color="green", linetype=1) +
#      geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, col="red")
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
                     linetype=2) +
        geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, linetype=2)
#        geom_segment(x = s, y = s, xend = (t + s - 1), yend = s,
#                     color = "green") +
#        geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, col="red")
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

p = kplot( c(rep(0, 3), 1, rep(0, 6), 1), s=2, t=11) +
  xlab("Number of Patients Enrolled") + ylab("Number of Responders") +
  geom_text(data=NULL, x=7, y=2.1, label="Success Boundary") +
  geom_text(data=NULL, x=12, y=0.5, label="Failure Boundary",
            angle=(45+90)/2+9) + 
  geom_text(data=NULL, x=7, y=1.1, label="Sample Path") +
  theme_bw() + scale_fill_grey()

ggsave("KanePlot.pdf", p, width=7, height=5)


# SNB distribution shapes.

approx_plots_start = list(
  dsnb_stack_plot(0.2, 15, 75) + scale_x_discrete(breaks=seq(15, 95, by=10)) +
    labs(x="", y="", title="bimodal example (0.2, 15, 75)") + theme_bw() +
    scale_fill_grey(),
  dsnb_stack_plot(0.35, 50, 50) + scale_x_discrete(breaks=seq(50, 105, by=10))+
    labs(x="", y="", title="approximate normal (0.35, 50, 50)") + theme_bw() +
    scale_fill_grey(),
  dsnb_stack_plot(0.06, 10, 10) + scale_x_discrete(breaks=seq(10, 20, by=2))+
    labs(x="", y="", title="geometric (0.06, 10, 10)") + theme_bw() +
    scale_fill_grey(),
  dsnb_stack_plot(0.06, 3, 175) + scale_x_discrete(breaks=seq(3, 180, by=30))+
    labs(x="", y="", title="gamma approximation (0.06, 3, 175)") + theme_bw()+
    scale_fill_grey(),
  dsnb_stack_plot(0.5, 25, 25) + scale_x_discrete(breaks=seq(25, 50, by=5))+
    labs(x="", y="", title="lower half normal (0.5, 25, 25)") + 
    theme_bw() + scale_fill_grey(),
  dsnb_stack_plot(0.98, 175, 2) + scale_x_discrete(breaks=seq(2, 180, by=20))+
    labs(x="", y="", title="local mode at top of range (0.98, 175, 2)") + 
    theme_bw() + scale_fill_grey())

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


# Binomial Tail Connection.
p = 0.2
s = 2
t = 12
n = s+t-1

dsnb_stack_plot(p, s, t) + ylab("SNB(0.2, 2, 12)") + theme_bw() + 
  scale_fill_grey()
ggsave("snb_density.pdf")

s1 = dsnb_stacked(min(s,t):n, p, s, t)

b1 = dbinom(0:n, n, p)

d = data.frame(list(x=0:n, y=b1))
d$Outcome = factor(c(rep("t", s), rep("s", n-s+1)))
d$Outcome = relevel(d$Outcome, "s")

ggplot(d, aes(x=x, y=y, fill=Outcome))+geom_bar(stat="identity") + xlab("k")+
  ylab("Bin(0.2, 13)") + theme_bw() + scale_fill_grey()
ggsave("bin_density.pdf")
