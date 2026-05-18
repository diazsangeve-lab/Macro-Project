%----------------------------------------------------------------
% Welfare Analysis Driver
%
% Runs Regime M, Regime F, and the Coexistence regime in turn at
% order = 2, extracts the unconditional mean of welfare (E[W]) from each,
% and computes the Consumption Equivalent (CE) of Regime F and the
% Coexistence regime relative to Regime M.
%
% CE_i = exp[ (1 - beta) * (W_i - W^M) ] - 1
%----------------------------------------------------------------

clear all; close all; clc;

%----------------------------------------------------------------
% 1. Regime M (benchmark)
%----------------------------------------------------------------
fprintf('\n========== Running Regime M ==========\n');
dynare Regime_M_welfare noclearall nolog

% oo_.mean is indexed by the variables in stoch_simul: ... W_c W_n W
W_c_M  = oo_.mean(end-2);
W_n_M  = oo_.mean(end-1);
W_M    = oo_.mean(end);
sigma_val = M_.params(strcmp(cellstr(M_.param_names), 'sigma'));

%----------------------------------------------------------------
% 2. Regime F
%----------------------------------------------------------------
fprintf('\n========== Running Regime F ==========\n');
dynare Regime_F_welfare noclearall nolog
W_F = oo_.mean(end);

%----------------------------------------------------------------
% 3. Coexistence regime
%----------------------------------------------------------------
fprintf('\n========== Running Coexistence regime ==========\n');
dynare Coexistence_welfare noclearall nolog
W_C = oo_.mean(end);

%----------------------------------------------------------------
% 4. Compute consumption-equivalent welfare (CRRA exact formula)
%    lambda = ( (W_new + W_n_old) / W_c_old )^(1/(1-sigma)) - 1
%----------------------------------------------------------------
CE_F = ( (W_F + W_n_M) / W_c_M )^(1 / (1 - sigma_val)) - 1;
CE_C = ( (W_C + W_n_M) / W_c_M )^(1 / (1 - sigma_val)) - 1;

CE_F_percent = CE_F * 100;
CE_C_percent = CE_C * 100;

%----------------------------------------------------------------
% 5. Display results
%----------------------------------------------------------------
fprintf('\n');
fprintf('=================================================================\n');
fprintf('   WELFARE ANALYSIS - Consumption Equivalent (Regime M = benchmark)\n');
fprintf('=================================================================\n');
fprintf('   Unconditional welfare E[W]:\n');
fprintf('      Regime M           : %12.6f\n', W_M);
fprintf('      Regime F           : %12.6f\n', W_F);
fprintf('      Coexistence regime : %12.6f\n', W_C);
fprintf('-----------------------------------------------------------------\n');
fprintf('   Consumption Equivalent (%%):\n');
fprintf('      Regime F           : %+9.4f %%\n', CE_F_percent);
fprintf('      Coexistence regime : %+9.4f %%\n', CE_C_percent);
fprintf('=================================================================\n');

%----------------------------------------------------------------
% 6. Save results for use in the report
%----------------------------------------------------------------
welfare_results = table( ...
    {'Regime M'; 'Regime F'; 'Coexistence'}, ...
    [W_M; W_F; W_C], ...
    [0; CE_F; CE_C], ...
    'VariableNames', {'Regime', 'Unconditional_Welfare', 'CE_percent'} );
disp(welfare_results);
writetable(welfare_results, 'welfare_results.csv');
fprintf('\nResults written to welfare_results.csv\n');
