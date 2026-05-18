% =======================================================
% Coexistence regime sensitivity analysis.
% ========================================================
clear; clc;

dynare Coexistence noclearall nograph

var_list = {'y','c','Pi','b','R','s','w','N','dsc', ...
            'b_m','Pi_m','b_f','Pi_f','dsc_f'};
%-----------------------------------------------------------
% PART A: Zeta sensitivity (theta held at baseline 0.75)
%-----------------------------------------------------------

set_param_value('theta', 0.75);

zeta_vals  = [0.25, 0.50, 0.75];
zeta_files = {'irf_coex_zeta025.mat', ...
              'irf_coex_zeta050.mat', ...
              'irf_coex_zeta075.mat'};

for k = 1:3
    set_param_value('zeta', zeta_vals(k));

    [~, oo_, options_, M_] = stoch_simul(M_, options_, oo_, var_list);

    irfs = oo_.irfs;
    save(zeta_files{k}, 'irfs');
    fprintf('Saved: %s\n', zeta_files{k});
end
%-----------------------------------------------------------
% Part B- Theta sensitivity (zeta reset to baseline 0.75)
%---------------------------------------------------------
set_param_value('zeta', 0.75);

theta_vals  = [0.00, 0.50, 0.75];
theta_files = {'irf_coex_theta000.mat', ...
               'irf_coex_theta050.mat', ...
               'irf_coex_theta075.mat'};

for k = 1:3
    set_param_value('theta', theta_vals(k));

    [~, oo_, options_, M_] = stoch_simul(M_, options_, oo_, var_list);

    irfs = oo_.irfs;
    save(theta_files{k}, 'irfs');
    fprintf('Saved: %s\n', theta_files{k});
end

set_param_value('zeta',  0.75);  
set_param_value('theta', 0.75);

fprintf('\nAll runs complete. Now run plot_sensitivity_coexistence.m\n');
