data {
  int N;                  // サンプルサイズ
  vector[N] steps;        // 歩数データ
  vector[N] weight;  // 体重データ
}

parameters {
  real Intercept;         // 切片
  real beta;              // 係数
  real<lower=0> sigma;    // 標準偏差
}

model {
  // 平均Intercept + beta*temperature
  // 標準偏差sigmaの正規分布に従ってデータが得られたと仮定
  steps ~ normal(Intercept + beta*weight, sigma);
}
