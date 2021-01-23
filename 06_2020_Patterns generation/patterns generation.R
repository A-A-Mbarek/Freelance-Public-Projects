V <- c("A", "E", "I", "O", "U")
C <- setdiff(LETTERS, V)
L <- LETTERS
N <- 0:9

library(tidyverse)

#CVCV
CVCV <- expand.grid(C, V, C, V) %>% 
  mutate(CVCV = str_c(Var1, Var2, Var3, Var4))

write.table(CVCV["CVCV"], file = "CVCV.csv", row.names = FALSE, quote=FALSE)

#VCVC
VCVC <- expand.grid(V, C, V, C) %>% 
  mutate(VCVC = str_c(Var1, Var2, Var3, Var4))

write.table(VCVC["VCVC"], file = "VCVC.csv", row.names = FALSE, quote=FALSE)

#VCCV
VCCV <- expand.grid(V, C, C, V) %>% 
  mutate(VCCV = str_c(Var1, Var2, Var3, Var4))

write.table(VCCV["VCCV"], file = "VCCV.csv", row.names = FALSE, quote=FALSE)

#CVVC
CVVC <- expand.grid(C, V, V, C) %>% 
  mutate(CVVC = str_c(Var1, Var2, Var3, Var4))

write.table(CVVC["CVVC"], file = "CVVC.csv", row.names = FALSE, quote=FALSE)

#LLL
LLL <- expand.grid(L, L, L) %>% 
  mutate(LLL = str_c(Var1, Var2, Var3))

write.table(LLL["LLL"], file = "LLL.csv", row.names = FALSE, quote=FALSE)

#LLLL
LLLL <- expand.grid(L, L, L, L) %>% 
  mutate(LLLL = str_c(Var1, Var2, Var3, Var4))

write.table(LLLL["LLLL"], file = "LLLL.csv", row.names = FALSE, quote=FALSE)

#LLLLL
LLLLL <- expand.grid(L, L, L, L, L) %>% 
  mutate(LLLLL = str_c(Var1, Var2, Var3, Var4, Var5))

write.table(LLLLL["LLLLL"], file = "LLLLL.csv", row.names = FALSE, quote=FALSE)

#NNN
NNN <- expand.grid(N, N, N) %>% 
  mutate(NNN = str_c(Var1, Var2, Var3))

write.table(NNN["NNN"], file = "NNN.csv", row.names = FALSE, quote=FALSE)

#NNNN
NNNN <- expand.grid(N, N, N, N) %>% 
  mutate(NNNN = str_c(Var1, Var2, Var3, Var4))

write.table(NNNN["NNNN"], file = "NNNN.csv", row.names = FALSE, quote=FALSE)

#NNNNN
NNNNN <- expand.grid(N, N, N, N, N) %>% 
  mutate(NNNNN = str_c(Var1, Var2, Var3, Var4, Var5))

write.table(NNNNN["NNNNN"], file = "NNNNN.csv", row.names = FALSE, quote=FALSE)

#CVCVC
CVCVC <- expand.grid(C, V, C, V, C) %>% 
  mutate(CVCVC = str_c(Var1, Var2, Var3, Var4, Var5))

write.table(CVCVC["CVCVC"], file = "CVCVC.csv", row.names = FALSE, quote=FALSE)

#VCVCV
VCVCV <- expand.grid(V, C, V, C, V) %>% 
  mutate(VCVCV = str_c(Var1, Var2, Var3, Var4, Var5))

write.table(VCVCV["VCVCV"], file = "VCVCV.csv", row.names = FALSE, quote=FALSE)
