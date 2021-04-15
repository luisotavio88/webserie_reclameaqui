dat1 <- data.frame(
  empresa = rep(c("empresa A", "empresa B"), each=4),
  produto = rep(1:4, 2),
  nota = runif(n = 8,min =  1, max = 10)
)

# idvar -> variável que identifica quais são os grupos
# timevar -> variável que diferencia os elementos do mesmo grupo

# Formato wide
wide <- reshape(dat1, idvar = "empresa", timevar = "produto", direction = "wide")
wide

# Formato long
long <- reshape(wide, idvar = "empresa", 
                timevar = "produto", 
                direction = "long",
                new.row.names = 1:8)
long
