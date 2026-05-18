%----------------------------------------------------------------
% Coexistence Regime WELFARE ANALYSIS
%----------------------------------------------------------------

var 
  y   N   c   R   w   b   Z   s   tau   tau_n   g  
  Pi  P_star  d  g1  g2  mc  dsc
  b_f  Pi_f  b_m  Pi_m                     
  y_f N_f c_f R_f w_f  
  Z_f s_f tau_f tau_n_f g_f
  P_star_f d_f g1_f g2_f mc_f dsc_f
  W W_c W_n
;

varexo eps_R eps_A;

%----------------------------------------------------------------
% Parameter declaration
%----------------------------------------------------------------
parameters
  beta sigma eta chi theta v
  alpha_AM_pi alpha_PM_pi rho_R
  delta_PF delta_AF
  zeta tau_c
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

alpha_AM_pi = 1.5;
alpha_PM_pi = 0.5;
rho_R       = 0.85;
delta_PF    = 1.2;
delta_AF    = 0;
zeta        = 0.75;

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
%----------------------------------------------------------------
% 1. ACTUAL ECONOMY
%----------------------------------------------------------------
    
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
    
    b_m = b - b_f;
    Pi_m = Pi / Pi_f;
    
    % Fiscal Rule
    s / y = (s_ss / y_ss) 
          * ( (b(-1) / (R(-1) * y(-1))) / (b_f(-1) / (R(-1) * y(-1))) )^delta_PF
          * ( (b_f(-1) / (R(-1) * y(-1))) / (b_ss / (R_ss * y_ss)) )^delta_AF;
    
    % Monetary Rule
    R / R_ss = (R(-1) / R_ss)^rho_R 
             * ( (Pi_m / Pi_ss)^alpha_AM_pi * (Pi_f / Pi_ss)^alpha_PM_pi )^(1 - rho_R)
             * exp(eps_R);
             
%----------------------------------------------------------------
% 2. SHADOW ECONOMY
%----------------------------------------------------------------
    
    % Households & Firms
    c_f^(-sigma) = beta * c_f(+1)^(-sigma) * (R_f / Pi_f(+1));
    chi * N_f^eta * c_f^sigma = w_f * (1 - tau_n_f) / (1 + tau_c);
    mc_f = w_f / Z_f;
    y_f = c_f + g_f;
    Z_f * N_f = y_f * d_f;
    log(Z_f) = 0.9 * log(Z_f(-1)) + eps_A;
    
    % Calvo Pricing
    g1_f = mc_f * y_f * c_f^(-sigma) + beta * theta * Pi_f(+1)^v * g1_f(+1);
    g2_f = y_f * c_f^(-sigma) + beta * theta * Pi_f(+1)^(v-1) * g2_f(+1);
    g1_f = P_star_f * g2_f * ((v - 1) / v);
    1 = theta * Pi_f^(v - 1) + (1 - theta) * P_star_f^(1 - v);
    d_f = theta * Pi_f^v * d_f(-1) + (1 - theta) * P_star_f^(-v);
    
    % Government & Debt Dynamics
    g_f = g_ss;
    tau_f = tau_n_f * w_f * N_f + tau_c * c_f;
    s_f = tau_f - g_f;
    b_f = (R_f(-1) / Pi_f) * b_f(-1) - s_f;
    dsc_f = (R_f(-1) / Pi_f - 1) * b_f(-1);
    
    % Shadow Fiscal Rule
    s_f / y_f = (s_ss / y_ss);
    
    % Shadow Monetary Rule 
    R_f / R_ss = (R_f(-1) / R_ss)^rho_R 
               * (Pi_f / Pi_ss)^((1 - rho_R) * alpha_PM_pi) 
               * exp(zeta * eps_R);

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
    
    b_f    = b_ss;       b_m    = 0;          
    Pi_f   = Pi_ss;      Pi_m   = 1;
    g1     = (mc * y * c^(-sigma)) / (1 - beta * theta);
    g2     = (y * c^(-sigma)) / (1 - beta * theta);
    
    y_f    = y_ss;       c_f    = c_ss;       g_f    = g_ss;
    R_f    = R_ss;       
    N_f    = N_ss;       w_f    = w_ss;
    Z_f    = Z_ss;       tau_n_f= tau_n_ss;
    s_f    = s_ss;       
    tau_f  = s_f + g_ss;
    mc_f   = w_ss;       P_star_f= 1;
    d_f    = 1;          
    dsc_f  = dsc_ss;
    g1_f   = g1;         g2_f   = g2;

    % Steady-state welfare: W_ss = U_ss / (1 - beta)
    W_c = ( (c_ss^(1-sigma))/(1-sigma) ) / (1 - beta);
    W_n = ( chi*(N_ss^(1+eta))/(1+eta) ) / (1 - beta);
    W   = W_c - W_n;
end;

steady;
check;

shocks;
    var eps_R; stderr 0.01;
end;

stoch_simul(order=2, irf=40, pruning) y c Pi b R s w N dsc b_m Pi_m b_f Pi_f dsc_f W_c W_n W;