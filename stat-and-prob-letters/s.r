library(snb)
library(ggplot2)
library(tidyr)
library(latex2exp)

outcomes = c(rep(0, 2), 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1)

p = kplot( outcomes, s=7, t=11, bw=TRUE) +
  xlab("Number of Patients Enrolled") + 
  ylab("Number of Responders to Treatment") +
  geom_text(data=NULL, x=11.5, y=6.5, label="Success Boundary") +
  geom_text(data=NULL, x=14.5, y=2.75, label="Failure Boundary",
            angle=(45+90)/2-17) + 
  geom_text(data=NULL, x=5.8, y=2.5, label="One Possible\nSample Path") +
  theme_bw() + scale_fill_grey()
p

ggsave("KanePlot.pdf", p, width=10, height=5)

# SNB distribution shapes.

approx_plots_start = list(
  dsnb_stack_plot(0.2, 15, 75) + scale_x_discrete(breaks=seq(15, 95, by=10)) +
    labs(x="", y="", title="bimodal (0.2, 15, 75)") + theme_bw() +
    scale_fill_grey(),
  dsnb_stack_plot(0.35, 50, 50) + scale_x_discrete(breaks=seq(50, 105, by=10))+
    labs(x="", y="", title="normal approximation (0.35, 50, 50)") + theme_bw() +
    scale_fill_grey(),
  dsnb_stack_plot(0.06, 10, 10) + scale_x_discrete(breaks=seq(10, 20, by=2))+
    labs(x="", y="", title="geometric or Poisson approximation (0.06, 10, 10)") + 
    theme_bw() + scale_fill_grey(),
  dsnb_stack_plot(0.06, 3, 175) + scale_x_discrete(breaks=seq(3, 180, by=30))+
    labs(x="", y="", title="gamma approximation (0.06, 3, 175)") + theme_bw()+
    scale_fill_grey(),
  dsnb_stack_plot(0.5, 25, 25) + scale_x_discrete(breaks=seq(25, 50, by=5))+
    labs(x="", y="", title="lower half normal (0.5, 25, 25)") + 
    theme_bw() + scale_fill_grey(),
  dsnb_stack_plot(p=0.45, 25, 25) + 
    scale_x_discrete(breaks=seq(25, 49, by=3))+
    labs(x="", y="", title="upper truncated normal (0.45, 25, 25)") + 
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
d = as.data.frame(dsnb_stacked(min(s,t):(t+s-1), p = p, s = s, t = t))
names(d)[2:3] = c("success", "failure")
d = gather(d, x)
names(d)[2] = "Outcome"
d$Outcome = factor(d$Outcome)
d$Outcome = relevel(d$Outcome, "failure")
ggplot(data = d, aes(x = factor(x), y = value, fill = Outcome)) + 
    geom_bar(position = "stack", stat = "identity") + xlab("k") + 
    title("SNB(0.2, 7, 11)") + 
    ylab("Probability") + ylim(0, 0.25) +
    theme(text = element_text(size=15)) + 
    theme_bw() + scale_fill_grey(start=0.8, end=0.2)

ggsave("snb_density.pdf")

s1 = dsnb_stacked(min(s,t):n, p, s, t)

b1 = dbinom(0:n, n, p)

d = data.frame(list(x=0:n, y=b1))
d$Outcome = factor(c(rep("failure", s), rep("success", n-s+1)))
d$Outcome = relevel(d$Outcome, "failure")

ggplot(d, aes(x=factor(x), y=y, fill=Outcome)) +
  geom_bar(stat="identity") + xlab("k")+
  title("Bin(0.2, 17)") + theme_bw() + scale_fill_grey(start=0.8, end=0.2) +
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

dsnb_plot(p, s, t) + ylab("Probability") + theme(text = element_text(size=15)) +
  theme_bw()
ggsave("../sii/snb-first-plot.pdf", width=8, height=5)

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
  theme(text = element_text(size=15)) + theme_bw()
ggsave("mean-and-variance.pdf")

# Assume the uniform prior.
pp = function(x, k, s, t) {
  denom = choose(k-1, s-1) * beta(s, k-s) + choose(k-1, t-1) * beta(k-t, t)
  ret = foreach(p = x, .combine=rbind) %do% {
    c(choose(k-1, s-1) * p^(s-1+1) * (1-p)^(k-s-1+1), 
      choose(k-1, t-1) * p^(k-t-1+1) * (1-p)^(t-1+1))
  }
  ret = cbind(ret, x)
  colnames(ret) = c("success", "failure", "p")
  rownames(ret) = NULL
  as.data.frame(ret)
}

x = pp(seq(0, 1, by=0.01), 15, s, t)
x = gather(x, p)
names(x) = c("p", "Outcome", "value")
x$Outcome = relevel(factor(x$Outcome), "failure")
#p = ggplot(x, aes(x=p, y=value, fill=Outcome)) + geom_area(position="stack") +
p = ggplot(x, aes(x=p, y=value, fill=Outcome)) + 
  geom_area(alpha=0.8, position="identity") +
  theme_bw() + scale_fill_grey(start=0.8, end=0.2) + xlab(expression(x)) +
  ylab(TeX("f_{P|Y}(x | k, s, t)")) 
ggsave("beta-mixture.pdf", p, width=8, height=5)


library(grid)
library(latex2exp)

theta = pi / 3
uo = cos(theta)
ua = sin(theta)
do = ua
da = uo

sx = 1/(tan(theta) +1)
sy = -sx + 1

p1 = ggplot() +
  geom_segment(mapping=aes(x=0, y=0, xend=1, yend=0)) + 
  geom_segment(mapping=aes(x=0, y=0, xend=0, yend=1)) +
  geom_segment(mapping=aes(x=0, y=0, xend=sx, yend=sy),
    arrow=arrow(length=unit(.1, "inches"))) +
  geom_curve(mapping=aes(x=sx/2.5, y=sy/2.5, xend=0, yend=1/3),
    curvature=0.3) +
  geom_curve(mapping=aes(x=1/4.5, y=0, xend=sx/3.5, yend=sy/3.5),
    curvature=0.35) +
  annotate("text", x=0.15, y=0.5, 
    label=TeX("$\\pi/2 - \\Theta$", output="character"), parse=TRUE) +
  annotate("text", x=0.25, y=.125, label=TeX("$\\Theta$", 
           output="character"), parse=TRUE) + xlab("") +ylab("") +
  scale_x_continuous(breaks = seq(0, 1, by=0.5), limits=seq(0, 1)) +
  scale_y_continuous(breaks=seq(-1, 1, by=0.5), limits=c(-1, 1) ) +
  theme_bw()
ggsave("proc.pdf", p1, width=3, height=6)
  
p2 = ggplot() + 
  geom_segment(mapping=aes(x=0, y=0, xend=ua, yend=uo)) + 
  geom_segment(mapping=aes(x=0, y=0, xend=da, yend=-do)) +
  geom_curve(mapping=aes(x=1/4, y=0, xend=ua/4, yend=uo/4), curvature=0.2) +
  geom_curve(mapping=aes(x=da/6, y=-do/6, xend=1/6, yend=0), curvature=0.4) +
  geom_segment(mapping=aes(x=0, y=0, xend=sqrt(sx^2+sy^2), yend=0),
    arrow=arrow(length=unit(.1, "inches"))) +
  annotate("text", x=0.45, y=0.1, 
    label=TeX("$\\pi/2 - \\Theta$", output="character"), parse=TRUE) +
  annotate("text", x=0.2, y=-0.10, 
    label=TeX("$\\Theta$", output="character"), parse=TRUE)  +
  theme_bw() +xlab("") + ylab("") + 
  scale_x_continuous(breaks = seq(0, 1, by=0.5), limits=seq(0, 1)) +
  scale_y_continuous(breaks=seq(-1, 1, by=0.5), limits=c(-1, 1) ) 

ggsave("proc-rot.pdf", p2, width=3, height=6)

p = zplot(outcomes, 7, 11, bw=TRUE) + ylab("Response") + 
  xlab("Non-Response") + theme_bw() + 
  geom_text(data=NULL, x=11/2, y=7.2, label="Success Boundary") +
  geom_text(data=NULL, x=11.5-.2, y=7/2, label="Failure Boundary", angle=-90) 

ggsave("zplot.pdf", p, width=11, height=7)

s=7
t=11
n=s+t-1
psnb(7:n, 0.2, s, t)

lbeq = function(t, a, b) {
  abs(a)/(t^(3/2)) * dnorm( (a+b*t) / sqrt(t) )
}

# from the coord on the zplot to the reparameterized space for the
# success barrier.
ucoord = function(m, theta) {
  y = m[,2]
  x = m[,1]
  yu = (-x*sin(theta) + s*cos(theta)) * cos(theta)
  xu = (x*cos(theta)  + s*sin(theta)) + sin(theta)
  cbind(xu, yu)
}

# Same thing, but for the failure barrier.
lcoord = function(m, theta) {
  y = m[,2]
  x = m[,1]
  yl = (y*cos(theta) - t*sin(theta))*sin(theta)
  xl = (y*sin(theta) + t*cos(theta))*cos(theta)
  cbind(xl, yl)
}

cum_approx = function(q, prob, s, t) {
  theta = atan(prob)
  yu0 = s*cos(theta)^2
  xu0 = s*sin(theta)^2
  yu1 = (-(t-1) * sin(theta) + s*cos(theta))*cos(theta)
  xu1 = ( (t-1) * cos(theta) + s*sin(theta))*sin(theta)
  bu = -(tan(theta)^2)
  au = s/(cos(theta)^2)
  u_denom = sum(lbeq(seq(xu0, xu1, length.out=1000), au, bu)*(xu1-xu0)/1000)
  u_num = 0
  # Can you even get to success bro?
  if (q >= s) {
    xvec = seq(0, min(q-s+1, t-1), length.out=1000)
    yvec = rep(s, length(xvec))
    uc = ucoord(theta)
    u_num = sum(lbeq(uc[,1], au, bu) * (max(uc[,1]) - min(uc[,1]))/1000)
  }
  
  yl0 = -t*sin(theta)*sin(theta)
  xl0 = t*cos(theta)*cos(theta)
  yl1 = ( (s-1)*cos(theta) - t*sin(theta) )*sin(theta)
  xl1 = ( (s-1)*sin(theta) + t*cos(theta) )*cos(theta)
  bl = 1/(tan(theta)^2)
  al = -t/(sin(theta)^2)
  l_denom = sum(lbeq(seq(xl0, xl1, length.out=1000), al, bl)*(xl1-xl0)/1000)
  l_num = 0
  if (q >= t) {
    xvec = seq(0, min(q-s+1, s-1), length.out=1000)
    yvec = rep(t, length(xvec))
    lc = lcoord(theta)
    lnum = sum(lbeq(lc[,1], al, bl) * (max(lc[,1]) - min(lc[,1]))/1000)
  }
  (u_num + l_num) /  (u_denom + l_denom)
} 

success = data.frame(list(non_resp=0:10, 
  resp=c(2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7)))

failure = data.frame(list(resp=0:6, non_resp=c(6:11, 11)))

df = as.data.frame(matrix( c(2, 2, 4, 3, 5, 3, 7, 4, 8, 4, 10, 5, 11, 5,
  13, 6, 14, 6, 16,7,17,7, 6, 0, 8, 1, 10, 2, 12, 3, 14, 4, 16, 5, 17, 6), 
  ncol=2, byrow=TRUE))
names(df) = c("k", "y")
df$boundary = "failure"
df$boundary[1:11] = "success"
df = rbind(df, data.frame(list(k=c(7, 17), y=c(7, 7), boundary="old_success")),
  data.frame(list(k=c(11, 17), y=c(0, 6), boundary="old_failure"))) 

# Confidence sets.
#df = snb:::flips_to_zplot_df(outcomes)

p = ggplot() +
  geom_step(data=df[df$boundary=="success",], aes(x=k, y=y), direction="vh") +
  geom_step(data=df[df$boundary=="failure",], aes(x=k, y=y), direction="hv") +
  geom_line(data=df[df$boundary=="old_success",], aes(x=k, y=y), linetype=2) +
  geom_line(data=df[df$boundary=="old_failure",], aes(x=k, y=y), linetype=2) +
  scale_y_continuous(breaks=0:7, limits=c(0, 7.2), minor_breaks=NULL) + 
  scale_x_continuous(breaks=0:17, limits=c(0, 17), minor_breaks=NULL) +
  ylab("Response") + 
  xlab("Non-Response") + theme_bw() + 
  annotate("text", x=11.5, y=7.2, label="SNB Success Boundary") +
  annotate("text", x=14.5, y=2.75, label="SNB Failure Boundary",
            angle=(45+90)/2-25) + 
  annotate("text", x=5, y=4.5, label="BSPRT Success Boundary") +
  annotate("text", x=8, y=1.2, label="BSPRT Failure Boundary") +
  theme_bw() + scale_fill_grey()
p

ggsave("bsprt.pdf", p, width=10, height=3.5)

# Brownian Motion approximation.
p = 0.2
s = 700
t =1100

theta = atan(0.2)
df = data.frame(list(
  x=c(seq(0, t-1, length.out=1000), rep(t, 1000)),
  y=c(rep(s, 1000), seq(0, s-1, length.out=1000))))
df$barrier = "failure"
df$barrier[1:1000] = "success"
  
ggplot(df, aes(x=x, y=y, group=barrier)) + geom_line()

# Transform the top barrier
df$xp[1:1000] = df$x[1:1000] * cos(theta) + s * sin(theta)
df$yp[1:1000] = -df$x[1:1000] * sin(theta) + s * cos(theta)

# Transform the right barrier.
df$xp[1001:2000] = df$y[1001:2000] * sin(theta) + t * cos(theta)
df$yp[1001:2000] = df$y[1001:2000] * cos(theta) - t * sin(theta)

ggplot(df, aes(x=xp, y=yp, group=barrier)) + geom_line()

# Scale the top barrier.
df$xpp[1:1000] = df$xp[1:1000] / sin(theta)
df$ypp[1:1000] = df$yp[1:1000] / cos(- theta)

# Scale the right barrier.
df$xpp[1001:2000] = df$xp[1001:2000] / cos(pi/2 - theta)
df$ypp[1001:2000] = df$yp[1001:2000] / sin(pi/2 - theta)

ggplot(df, aes(x=xpp, y=ypp, group=barrier)) + geom_line()

# Get the intercept for the transformed barriers.
success_slope = (df$ypp[1] - df$ypp[2])/(df$xpp[1] - df$xpp[2])
success_intercept = df$ypp[1] - success_slope*df$ypp[2]

failure_slope = (df$ypp[1001] - df$ypp[1002])/(df$xpp[1001] - df$xpp[1002])
failure_intercept = df$ypp[1001] - success_slope*df$ypp[1002]

df$density[1:1000] = success_intercept / df$xpp[1:1000]^(3/2) * 
  dnorm(df$ypp[1:1000]/sqrt(df$xpp[1:1000]))

df$density[1001:2000] = -failure_intercept / df$xpp[1001:2000]^(3/2) * 
  dnorm(-df$ypp[1001:2000]/sqrt(df$xpp[1001:2000]))

ggplot(df, aes(x=x, y=density, group=barrier)) + geom_line()
density = df$density[df$barrier=="success"] + df$density[df$barrier=="failure"]

df$k = df$x + df$y

library(snb)
library(tidyverse)
library(foreach)

p = seq(0, 1, by=0.01)
s = 7
t = 11
y = 10

snb_ll = function(y, s, t, p=seq(0, 1, by=0.01)) {
  p1 = choose(y-1, s-1) * p^s*(1-p)^(y-s)
  p1[y < s] = 0
  p2 = choose(y-1, t-1) * (1-p)^t*p^(y-t)
  p2[y < t] = 0
  p1 + p2
}

p = seq(0, 1, by=0.01)
Y = c(7, 11, 13, 17)
lls = foreach(y=Y, .combine=cbind) %do% {
  snb_ll(y, s, t, p)
}

colnames(lls) = Y
lldf = as.data.frame(cbind(p, lls))
llldf = gather(lldf, Y, "Likelihood", -p)
p = ggplot(llldf, aes(x=p, y=Likelihood, group=Y)) + 
  geom_line(size=1.5) +
  xlab(expression(italic(p))) + ylab("Likelihood") +
  geom_text(data=NULL, x=0.08, y=0.75, label="Y=11") +
  geom_text(data=NULL, x=0.92, y=0.75, label="Y=7") +
  geom_text(data=NULL, x=0.2, y=0.27, label="Y=13") +
  geom_text(data=NULL, x=0.45, y=0.21, label="Y=17") +
  theme_bw() + scale_fill_grey()
  # labs(color=expression(Y[1])) + theme_bw() # +  scale_color_grey()

ggsave("likelihood.pdf", p, width=11, height=7)

# Counter 

plot(snb_ll(9, s, t, p))


library(ggplot2)
library(snb)
library(grid)
library(latex2exp)
library(foreach)
library(tidyverse)
library(ggplot2)

# Interim ANALYSIS

outcomes = c(rep(0, 2), 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1)[1:10]
s = 7
t = 11

# Assume the uniform prior.
pp = function(x, k, s, t) {
  denom = choose(k-1, s-1) * beta(s, k-s) + choose(k-1, t-1) * beta(k-t, t)
  ret = foreach(p = x, .combine=rbind) %do% {
    c(choose(k-1, s-1) * p^(s-1+1) * (1-p)^(k-s-1+1), 
      choose(k-1, t-1) * p^(k-t-1+1) * (1-p)^(t-1+1))
  }
  ret = cbind(ret, x)
  colnames(ret) = c("success", "failure", "p")
  rownames(ret) = NULL
  as.data.frame(ret)
}

x = pp(seq(0, 1, by=0.01), 15, s, t)
x = gather(x, p)
names(x) = c("p", "Outcome", "value")
x$Outcome = relevel(factor(x$Outcome), "failure")
#p = ggplot(x, aes(x=p, y=value, fill=Outcome)) + geom_area(position="stack") +
p = ggplot(x, aes(x=p, y=value, fill=Outcome)) + 
  geom_area(alpha=0.8, position="identity") +
  theme_bw() + scale_fill_grey(start=0.8, end=0.2) + xlab(expression(x)) +
  ylab(TeX("f_{P|Y}(x | k, s, t)")) 
ggsave("beta-mixture-interim.pdf", p, width=10, height=3.5)

p = kplot(outcomes, s=7, t=11, bw=TRUE) +
  xlab("Number of Patients Enrolled") + 
  ylab("Number of Responders to Treatment") +
  geom_text(data=NULL, x=11.5, y=6.5, label="Success Boundary") +
  geom_text(data=NULL, x=14.5, y=2.75, label="Failure Boundary",
            angle=(45+90)/2-25) + 
  geom_text(data=NULL, x=6, y=2.5, label="Sample Path") +
  theme_bw() + scale_fill_grey() 
ggsave("kplot-interim.pdf", p, width=10, height=3.5)

# POST-HOC ANALYSIS

outcomes = c(rep(0, 2), 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1)

# The kplot.
p = kplot(outcomes, s=7, t=11, bw=TRUE) +
  xlab("Number of Patients Enrolled") + 
  ylab("Number of Responders to Treatment") +
  ylim(0, 9) +
  geom_text(data=NULL, x=11.5, y=6.5, label="Success Boundary") +
  geom_text(data=NULL, x=14.5, y=2.75, label="Failure Boundary",
            angle=(45+90)/2-18) + 
  geom_text(data=NULL, x=6, y=2.5, label="Sample Path") +
  theme_bw() + scale_fill_grey() 

# The density plot.
df = data.frame(list(
  Density=dbeta(seq(0, 1, by=0.01), sum(outcomes)+1, sum(outcomes==0)+1),
  p=seq(0, 1, by=0.01)))
dp = ggplot(df, aes(x=p, y=Density)) + 
  geom_area(alpha=0.8, position="identity") +
  xlab(expression(x)) + ylab(TeX("f_P(x)")) + 
  labs(title="Posterior Distribution")

vp = viewport(width=0.3, height=0.2, x=0.8, y=0.88)
pdf("KanePlotEmbedded.pdf", width=10, height=6)
print(p)
print(dp, vp=vp)
dev.off()

library(snb)
library(tidyverse)
library(foreach)
s = 7
t = 11
n = s+t-1
p0 = 0.2
p1 = 0.4

significance = function(p0, s, t) {
  dsnb_stacked(min(s, t):(s+t-1), p=p0, s=s, t=t)[,'s'] %>% sum
}

power = function(p1, s, t) {
  dsnb_stacked(min(s, t):(s+t-1), p=p1, s=s, t=t)[,'s'] %>% sum
}

designs = foreach (si=seq_len(n-1), .combine=rbind) %do% {
  ti = n + 1 - si
  c(si, ti, significance(p0, si, ti), power(p1, si, ti),
    esnb(p0, si, ti))
}
rownames(designs) = NULL
colnames(designs) = c("s", "t", "Significance", "Power", "ess")
designs = as.data.frame(designs)

designs_long = gather(designs, `Design Feature`, value, Significance:Power)

ggplot(designs_long, aes(x=s, y=value, color=`Design Feature`)) + 
  geom_line() + ylab("Probability") + 
  xlab("Number of Responses to Stop the Trial (s)") + theme_bw() +
  scale_x_continuous(breaks=seq_len(n-1))

ggsave("all-hypothetical-designs.pdf")

ggplot(designs, aes(x=s, y=ess)) +
  geom_line() + ylab("Expected Sample Size Under the Null") +
  xlab("Number of Responses to Stop the Trial (s)") + theme_bw() +
  scale_x_continuous(breaks=designs$s)

ggsave("expected-sample-size.pdf")

data_pos = designs[1:11, c("Power", "Significance", "s")]
data_pos$Power = data_pos$Power + 0.02
data_pos$Significance = data_pos$Significance - 0.02

ggplot(designs, aes(x=Power, y=1-Significance)) + 
  geom_line() +   
  geom_text(data=data_pos, aes(x=Power, y=1-Significance, label=s)) +
  theme_bw()

ggsave("power-vs-significance.pdf")

  
