library(snb)

p = 0.2
s = 2
t = 12
n = s+t-1

dsnb_stack_plot(p, s, t) + ylab("SNB(0.2, 2, 12)")
ggsave("snb_density.pdf")

s1 = dsnb_stacked(min(s,t):n, p, s, t)

b1 = dbinom(0:n, n, p)

d = data.frame(list(x=0:n, y=b1))
d$Outcome = factor(c(rep("t", s), rep("s", n-s+1)))
d$Outcome = relevel(d$Outcome, "s")

ggplot(d, aes(x=x, y=y, fill=Outcome))+geom_bar(stat="identity") + xlab("k")+
  ylab("Bin(0.2, 13)")
ggsave("bin_density.pdf")
