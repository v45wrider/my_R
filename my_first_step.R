# poisson distribution
y <- 0:9
prob <- dpois(y, lambda = 3.56)
plot(y, prob, type = "b", lty = 2)
cbind(y, prob)
# data profile
load("data.RData")
print(length(data))
print(summary(data))
print(table(data))
print(var(data))
print(sd(data))
# plot
par(cex = 0.8)                # 全体的な文字倍率
par(mar = c(4, 4, 4, 4) + 0.1)
hist( # ヒストグラム
  data,                     # データ
  right=FALSE,              # 上限値を含めない
  breaks=seq(-0.5, 9.5, 1 ),# yは-0.5から9.5まで1刻み
  xlim=c(-1.0, 10),         # x軸目盛の範囲
  ylim=c(0, 14),            # y軸目盛の範囲
  xlab="確率変数",          # x軸ラベル
  ylab="出現回数",          # y軸ラベル
  main="観測データと確率分布",
  axes=FALSE                # 軸目盛は非表示
)
axis( 2 )                     # グラフ左側に第１Ｙ軸を表示
par( new = T )                # 重ね描き宣言
plot( # 折れ線グラフ
  y,                        # 確率変数y (x軸)
  prob,                     # 発生確率 (第2y軸)
  type="b",                 # 丸＆線タイプの折れ線
  pch=1,                    # 点の文字は○
  lwd=2,                    # 線の幅
  xlim=c(-1.0, 10),         # x軸目盛の範囲
  ylim=c( 0, 0.25 ),        # y軸目盛の範囲
  xlab="", 
  ylab="", 
  axes = FALSE
)
axis( 4 )                     # グラフ右側に第２Ｙ軸を表示
mtext(
  "出現確率", 
  line = 3,                 # プロットエリアからのマージン
  side = 4                  # プロットエリアの右
)
axis(                         # x軸目盛
  1,                        # プロットエリアの下
  at=seq( 0, 9, 1 )
)
box()                         # 外枠
# log likelihood
logL <- function(m) {sum(dpois(data, m, log = TRUE))}
lambda <- seq(2, 5, 0.1)
plot(
  lambda, 
  sapply(lambda, logL), 
  type = "l",
  ylab = "対数尤度log L(λ)",
  xlab = "平均λ"
)
poisDist <- sapply(lambda, logL)
print(cbind(lambda, poisDist))
logLMax <- max(poisDist)
print(logLMax)
lambdaMax <- cbind(lambda, poisDist)[which.max(poisDist)]
print(lambdaMax)
abline(h = logLMax, v = lambdaMax, col = "red", lty = 2)
