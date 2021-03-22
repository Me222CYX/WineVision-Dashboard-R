library(glmnet)
library(tidyverse)
grid = 10^seq(10,-2, length =100)

red <- wine %>% subset(Wine == "red")
white <- wine %>% subset(Wine == "white")
x <- as.matrix(white[,2:12])

# white wine
wlas <- cv.glmnet(x, white$Quality, alpha = 1, nfolds=5)
plot(wlas)

# red wine
x2 <- as.matrix(red[,2:12])
rlas <- cv.glmnet(x2, red$Quality, alpha = 1, nfolds=5)
plot(rlas)

# white wine
wlas.mod <- glmnet(x, white$Quality, alpha = 1, lambda = grid)
plot(wlas.mod)

bestlam = wlas$lambda.min
# lasso.pred = predict(wlas.mod, s = bestlam, newx=x[test ,])
# mean(( lasso.pred -y.test)^2)

lasso.coef = predict(wlas, type = "coefficients", s = bestlam)
lasso.coef
lasso.coef[lasso.coef != 0]

# red wine
bestlam2 = rlas$lambda.min
# lasso.pred = predict(wlas.mod, s = bestlam, newx=x[test ,])
# mean(( lasso.pred -y.test)^2)

lasso.coef2 = predict(rlas, type = "coefficients", s = bestlam2)
lasso.coef2
lasso.coef2[lasso.coef2 != 0]

