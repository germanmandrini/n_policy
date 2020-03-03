x <- seq(0, 10, by = 0.2)
y1 <- sin(x)
y2 <- cos(x)
y3 <- cos(x + pi / 4)
y4 <- sin(x + pi / 4)
df1 <- data.frame(x, y = y1, Type = as.factor("sin"), Method = as.factor("method1"))
df2 <- data.frame(x, y = y2, Type = as.factor("cos"), Method = as.factor("method1"))
df3 <- data.frame(x, y = y3, Type = as.factor("cos"), Method = as.factor("method2"))
df4 <- data.frame(x, y = y4, Type = as.factor("sin"), Method = as.factor("method2"))

df.merged <- data.table(rbind(df1, df2, df3, df4))

ggplot(df.merged, aes(x, y, colour = interaction(Type, Method), linetype = Method, shape = Type)) + geom_line() + geom_point()


d=data.table(expand.grid(r=seq(0,1,0.1), g=seq(0,1,0.1), b=seq(0,1,0.1)))
ggplot() +
  facet_wrap(~b) +
  scale_x_continuous(name="red", breaks=seq(0.05, 1.05, 0.2), labels=seq(0, 1, 0.2)) +
  scale_y_continuous(name="green", breaks=seq(0.05, 1.05, 0.2), labels=seq(0, 1, 0.2)) +
  scale_fill_identity() +
  geom_rect(data=d, mapping=aes(xmin=r, xmax=r+resolution(r), ymin=g, ymax=g+resolution(g), fill=rgb(r,g,b)), color="white", size=0.1)

library(RColorBrewer)
n <- 12
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector_sample <- sample(col_vector, n)
pie(rep(1,n), col=col_vector_sample)
pie(rep(1,n), col=c("#FFFF33", "#FFED6F", "#666666", "#7FC97F", "#386CB0", "#B3B3B3", "#FFFFCC", "#A65628", "#F4CAE4", "#E41A1C", "#E6AB02", "#7570B3"))
