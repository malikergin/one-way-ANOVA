

# H0: Means are equal. Therefore, the difference among group means is not statistically significant
# H1: Means are not equal. At least two (all maybe) means are statistically significant


A = c(8,12,19,8,6,11)
B = c(4,5,4,6,9,7)
C = c(11,8,7,13,7,9)

veri = c(A,B,C)

grup = rep(1:3, each = 6)

df = data.frame(veri,grup)

# GKT = GAKT + GIKT 

TOTAL = sum(df$veri)

N = length(df$veri)

DT = (TOTAL)^2/N

KT = 0

for (value in df$veri) {
  
  KT = KT + value^2

}

GKT = KT - DT

veri_g1 = df$veri[df$grup==1]
veri_g2 = df$veri[df$grup==2]
veri_g3 = df$veri[df$grup==3]

n1 = length(veri_g1)
n2 = length(veri_g2)
n3 = length(veri_g3)

GAKT = ((((sum(veri_g1))^2)/n1) + (((sum(veri_g2))^2)/n2) + (((sum(veri_g3))^2)/n3)) - DT

GIKT = GKT - GAKT

df_total = N - 1
df_grup = length(unique(df$grup)) - 1
df_error = df_total - df_grup

GAKO = GAKT / df_grup
GIKO = GIKT / df_error

F_value = GAKO/GIKO

F_table = qf(0.05, df_grup, df_error, lower.tail = FALSE)

decision = ifelse(F_value >= F_table, "H0 is rejected",'H0 is accepted')

print(decision)  

output = data.frame(
  SOURCES = c('GKT', 'GAKT', 'GIKT'),
  df = c(df_total, df_grup, df_error),
  SS = c(GKT, GAKT, GIKT),
  MS = c('-', GAKO, GIKO)
)

print(output)
print(F_value)
print(decision)





