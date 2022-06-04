data {
  int N;                  // サンプルサイズ
  vector[N] steps_gain;      // 増量者歩数データ
}

parameters {
  real mu_steps_gain;                // 増量者歩数平均
  real<lower=0> sigma_steps_gain;    // 増量者歩数標準偏差
}

model {
  // 平均mu、標準偏差sigmaの正規分布に従ってデータが得られたと仮定
  steps_gain ~ normal(mu_steps_gain, sigma_steps_gain);
}

// generated quantities {
//   real diff;                // 増量者の歩数と減量者の歩数の平均の差
//   diff = mu_steps_loss - mu_steps_gain;
// }
