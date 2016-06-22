library(snb)
library(ggplot2)
library(tidyr)

## kplot

#kplot = function(flips, s, t) {
#  if (!is.list(flips)) {
#    d = flips_to_kplot_df(flips)
#    p = qplot(k, path, data = d, geom = "line") +
#      scale_x_continuous(breaks = 0:(t + s), limits = c(0, t + s - 1)) +
#      scale_y_continuous(breaks = 0:s, limits=c(0, s+0.15)) +
#      geom_segment(x=s, y=s, xend=(t+s-1), yend=s, linetype=2) +
#      geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, linetype=2)
##      geom_segment(x=s, y=s, xend=(t+s-1), yend=s, color="green", linetype=1) +
##      geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, col="red")
#  } else {
#    flip_set = lapply(flips, flips_to_kplot_df)
#    for (i in 1:length(flip_set)) {
#      flip_set[[i]]$num = as.factor(i)
#      flip_set[[i]]$k = jitter(flip_set[[i]]$k)
#      flip_set[[i]]$k[flip_set[[i]]$k < 0] = 0
#    }
#    d = Reduce(rbind, flip_set)[, -(4:5)]
#    p = qplot(k, path, data = d, geom = "path", group = num) +
#        scale_x_continuous(breaks=0:(t+s), limits = c(0, t+s)) +
#        geom_segment(x = s, y = s, xend = (t + s - 1), yend = s,
#                     linetype=2) +
#        geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, linetype=2)
##        geom_segment(x = s, y = s, xend = (t + s - 1), yend = s,
##                     color = "green") +
##        geom_segment(x=t, y=0, xend=(s+t-1), yend=s-1, col="red")
#    p
#  }
#  p
#}

#flips_to_kplot_df = function (flips) {
#  d = data.frame(k = 0:length(flips))
#  d$head = c(0, cumsum(flips))
#  d$tail = c(0, cumsum(1 - flips))
#  d$headEnd = c(d$head[-1], NA)
#  d$tailEnd = c(d$tail[-1], NA)
#  d$path = c(0, cumsum(flips))
#  d$k = 0:(nrow(d) - 1)
#  d
#}

outcomes = c(rep(0, 2), 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1)

p = kplot( outcomes, s=7, t=11, bw=TRUE) +
  xlab("Number of Patients Enrolled") + 
  ylab("Number of Responders to Treatment") +
  geom_text(data=NULL, x=11.5, y=6.5, label="Success Boundary") +
  geom_text(data=NULL, x=14.5, y=2.75, label="Failure Boundary",
            angle=(45+90)/2-25) + 
  geom_text(data=NULL, x=6, y=2.5, label="Sample Path") +
  theme_bw() + scale_fill_grey()
p

ggsave("KanePlot.pdf", p, width=10, height=3.5)

# SNB distribution shapes.

approx_plots_start = list(
  dsnb_stack_plot(0.2, 15, 75) + scale_x_discrete(breaks=seq(15, 95, by=10)) +
    labs(x="", y="", title="bimodal (0.2, 15, 75)") + theme_bw() +
    scale_fill_grey(),
  dsnb_stack_plot(0.35, 50, 50) + scale_x_discrete(breaks=seq(50, 105, by=10))+
    labs(x="", y="", title="normal approximation (0.35, 50, 50)") + theme_bw() +
    scale_fill_grey(),
  dsnb_stack_plot(0.06, 10, 10) + scale_x_discrete(breaks=seq(10, 20, by=2))+
    labs(x="", y="", title="geometric approximation (0.06, 10, 10)") + 
    theme_bw() + scale_fill_grey(),
  dsnb_stack_plot(0.06, 3, 175) + scale_x_discrete(breaks=seq(3, 180, by=30))+
    labs(x="", y="", title="gamma approximation (0.06, 3, 175)") + theme_bw()+
    scale_fill_grey(),
  dsnb_stack_plot(0.5, 25, 25) + scale_x_discrete(breaks=seq(25, 50, by=5))+
    labs(x="", y="", title="lower half normal / riff-shuffle  (0.5, 25, 25)") + 
    theme_bw() + scale_fill_grey(),
  dsnb_stack_plot(p=0.45, 25, 25) + 
    scale_x_discrete(breaks=seq(25, 49, by=3))+
    labs(x="", y="", title="riff-shuffle (0.45, 25, 25)") + 
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
s = 7
t = 11
n = s+t-1

plot.new()
dsnb_stack_plot(p, s, t) + title("SNB(0.2, 7, 11)") + theme_bw() + 
  scale_fill_grey() + ylab("Probability") + ylim(0, 0.25) +
  theme(text = element_text(size=15))
ggsave("snb_density.pdf")

s1 = dsnb_stacked(min(s,t):n, p, s, t)

b1 = dbinom(0:n, n, p)

d = data.frame(list(x=0:n, y=b1))
d$Outcome = factor(c(rep("t", s), rep("s", n-s+1)))
d$Outcome = relevel(d$Outcome, "s")

ggplot(d, aes(x=factor(x), y=y, fill=Outcome)) +
  geom_bar(stat="identity") + xlab("k")+
  title("Bin(0.2, 17)") + theme_bw() + scale_fill_grey() +
  ylab("") + ylim(0, 0.25) + theme(text = element_text(size=15))
ggsave("bin_density.pdf")

# Sample size and variance for posterior distribution.
s = 7
t = 11
p = 0.2
x1 = cdsnb_stacked(min(s, t):(s+t-1), c(0.045, 0.955), s, t)
stacked_plot(x1$k, x1$s, x1$t)
dev.new()
x2 = cdsnb_stacked(min(s, t):(s+t-1), c(0.045*5, 0.955*5), s, t)
stacked_plot(x2$k, x2$s, x2$t)
dev.new()
plot(dbeta(seq(0, 1, by=0.01), 0.045*5, 0.955*5), type="l", col="green")
lines(dbeta(seq(0, 1, by=0.01), 0.045, 0.955), type="l", col="red")
#lines(dbeta(seq(0, 1, by=0.01), 0.045*10, 0.955*10), type="l", col="blue")

ecsnb(c(0.5, 0.5), s, t)

x2 = cdsnb_stacked(min(s, t):(s+t-1), c(0.1*10, 0.9*10), s, t)
stacked_plot(x2$k, x2$s, x2$t)

# Probality of success in Bayesian model.
# Variance of success in Bayesian model.

shape = c(0.2, 0.8)
is = seq(1, 100, length.out=100)
x = foreach (i=is, .combine=rbind) %do% {
  cbind(i, ecsnb(shape*i, s, t), vcsnb(shape*i, s, t))
}
x = as.data.frame(x)
names(x) = c("n", "e", "v")

exp_size = esnb(0.2, s, t)
ggplot(x, aes(x=n, y=e)) + geom_line() + 
  geom_segment(y=exp_size,x=min(is),yend=exp_size, xend=max(is), color="grey") +
  ylim(range(c(x$e, exp_size, 13.7))) +
  geom_text(data=NULL, x=50, y=13.7, label=paste0("Deterministic Limit (", 
    sprintf("%0.2f", exp_size), ")")) + 
  theme_bw() + scale_fill_grey() + ylab("Compound Distribution Mean") + 
  xlab("c") + theme(text = element_text(size=15))

ggsave("bayesian-sample-expectation.pdf")

exp_var = vsnb(0.2, s, t)
ggplot(x, aes(x=n, y=v)) + geom_line() + 
  geom_segment(y=exp_var,x=min(is),yend=exp_var, xend=max(is), color="grey") +
  ylim(range(c(x$v, exp_var))) +
  geom_text(data=NULL, x=50, y=2.70, 
    label=paste0("Deterministic Limit (", sprintf("%0.2f", exp_var), ")")) + 
    theme_bw() + scale_fill_grey() + ylab("Compound Distribution Variance") + 
    xlab("c") + theme(text = element_text(size=15))

ggsave("bayesian-sample-variance.pdf")

# Expected CSNB size
# Variance of CSNB size
# a) b)

dsnb_plot(p, s, t) + ylab("Probability") + theme(text = element_text(size=15))
ggsave("snb-first-plot.pdf")

ps = seq(0, 1, by=0.01)
moments = foreach(p=ps, .combine=rbind) %do% {
  c(esnb(p, s, t), vsnb(p, s, t))
}

m = as.data.frame(cbind(ps, moments))
names(m) = c("p", "exp", "var")
rownames(m) = NULL

p = ggplot(m, aes(x=p, y=exp)) + geom_line() + ylab("Distribution Mean") +
  theme(text = element_text(size=15))
ggsave("dist-mean.pdf", p)

p = ggplot(m, aes(x=p, y=var)) + geom_line() + ylab("Distribution Variance") +
  theme(text = element_text(size=15))
ggsave("dist-var.pdf", p)

y = gather(m, p, value)
names(y) = c("p", "type", "value")
y$type[y$type=='exp'] = "Mean"
y$type[y$type=='var'] = "Variance"
p = ggplot(y, aes(x=p, y=value)) + geom_line() + 
  facet_grid(type ~ ., scales="free") + ylab("") +
  theme(text = element_text(size=15))
ggsave("mean-and-variance.pdf")

