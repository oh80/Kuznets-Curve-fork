data{
  int N;
  vector[N] Z;
  vector[N] Y;
}

parameters{
  real<lower=0> s_x;
  real<lower=0> s_y;
  real b;
  vector[N] X;
}

model{
  X[2:N]~normal(X[1:N-1]+b*Z[2:N],s_x);
  Y~normal(X,s_y);
}