library("igraph")
getwd()
filename="facebook_combined.txt"
g = read.graph(file = filename, format="ncol", directed=FALSE)
connectivity_g = is.connected(g)
dia = diameter(g,directed = FALSE)

# deg dist
deg = degree(g)
h = hist(deg,breaks=seq(-0.5, by=1 , length.out=max(deg)+2),freq=F,main='Degree Distribution', xlab='Degree')

# CurveFit
x = h$mids[1:max(deg)+1]
y = h$density[1:max(deg)+1]
model = nls(y ~ (exp(a+b*x)), start=list(a=0 , b=0))
df = data.frame(x, y)
summary(model)

plot (df,type = "h",main='Degree Distribution', xlab='Degree',ylab='Density')
x_val = seq (from = 1, to = max(deg), by=1)
lines (x_val, predict (model,list(x=x_val)), col="red")

mse = mean(residuals(model)^2)

Avg_deg = mean(deg)


