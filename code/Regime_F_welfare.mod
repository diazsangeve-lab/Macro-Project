%----------------------------------------------------------------
% Regime F WELFARE ANALYSIS 
%----------------------------------------------------------------

var
  y   N   c   R   w   b   Z   s   tau   tau_n   g
  Pi  P_star  d  g1  g2  mc  dsc
  W W_c W_n
;

varexo eps_R eps_A;

parameters
  beta sigma eta chi theta v
  alpha_pi rho_R
  delta tau_c
  g_ss b_ss y_ss c_ss Pi_ss R_ss
  w_ss N_ss s_ss tau_ss tau_n_ss Z_ss dsc_ss
;

beta    = 0.99;
sigma   = 2;
eta     = 1;
v       = 9;
theta   = 0.75;

b_ss    = 0.746;
g_ss    = 0.230;
tau_c   = 0.023;

alpha_pi = 0.5;
rho_R    = 0.85;
delta    = 0;

% --- Standard Baseline Steady State ---
Pi_ss   = 1;
R_ss    = 1 / beta;

y_ss    = 1;
c_ss    = y_ss - g_ss;
w_ss    = (v - 1) / v;
N_ss    = y_ss;
Z_ss    = 1;

s_ss    = (R_ss / Pi_ss - 1) * b_ss;
tau_ss  = s_ss + g_ss;
tau_n_ss = (tau_ss - tau_c * c_ss) / (w_ss * N_ss);
dsc_ss  = (R_ss / Pi_ss - 1) * b_ss;

chi = w_ss * (1 - tau_n_ss) / ((1 + tau_c) * (c_ss^sigma) * (N_ss^eta));

model;
    % Households & Firms
    c^(-sigma) = beta * c(+1)^(-sigma) * (R / Pi(+1));
    chi * N^eta * c^sigma = w * (1 - tau_n) / (1 + tau_c);
    mc = w / Z;
    y = c + g;
    Z * N = y * d;
    log(Z) = 0.9 * log(Z(-1)) + eps_A;
    
    % Calvo Pricing
    g1 = mc * y * c^(-sigma) + beta * theta * Pi(+1)^v * g1(+1);
    g2 = y * c^(-sigma) + beta * theta * Pi(+1)^(v-1) * g2(+1);
    g1 = P_star * g2 * ((v - 1) / v);
    1 = theta * Pi^(v - 1) + (1 - theta) * P_star^(1 - v);
    d = theta * Pi^v * d(-1) + (1 - theta) * P_star^(-v);
    
    % Government & Debt Dynamics
    g = g_ss;
    tau = tau_n * w * N + tau_c * c;
    s = tau - g;
    b = (R(-1) / Pi) * b(-1) - s;
    dsc = (R(-1) / Pi - 1) * b(-1);
    
    % Fiscal rule (Active)
    s / y = (s_ss / y_ss) 
          * ( (b(-1) / (R(-1) * y(-1))) / (b_ss / (R_ss * y_ss)) )^delta;
    
    % Monetary rule (Passive)
    R / R_ss = (R(-1) / R_ss)^rho_R * (Pi / Pi_ss)^((1 - rho_R) * alpha_pi) * exp(eps_R);

    % -----------------------------------------------------------------
    % WELFARE (CRRA Specification)
    % -----------------------------------------------------------------
    W_c = (c^(1-sigma))/(1-sigma) + beta*W_c(+1);
    W_n = chi*(N^(1+eta))/(1+eta) + beta*W_n(+1);
    W   = W_c - W_n;
end;

initval;
    y      = y_ss;       c      = c_ss;       g      = g_ss;
    b      = b_ss;       s      = s_ss;       R      = R_ss;
    Pi     = Pi_ss;      N      = N_ss;       w      = w_ss;
    mc     = w_ss;       P_star = 1;          d      = 1;
    Z      = Z_ss;       tau_n  = tau_n_ss;   tau    = tau_ss;
    dsc    = dsc_ss;
    g1     = (mc * y * c^(-sigma)) / (1 - beta * theta);
    g2     = (y * c^(-sigma)) / (1 - beta * theta);

    W_c = ( (c_ss^(1-sigma))/(1-sigma) ) / (1 - beta);
    W_n = ( chi*(N_ss^(1+eta))/(1+eta) ) / (1 - beta);
    W   = W_c - W_n;
end;

steady;
check;

shocks;
    var eps_R; stderr 0.01;
end;

stoch_simul(order=2, irf=40, pruning) y c Pi b R s w N dsc W_c W_n W;