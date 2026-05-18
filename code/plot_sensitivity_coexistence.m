
clear; clc; close all;

horizon = 40;
t       = 1:horizon;
%-----------------------------------------------------------
% Variable configuration
%-----------------------------------------------------------

vars = { 'c_eps_R',   'Pi_eps_R',  'dsc_eps_R', ...   
         'b_eps_R',   's_eps_R',   'R_eps_R',   ...   
         'w_eps_R',   'N_eps_R' };                     

panel_titles = { 'Consumption',           'Inflation rate',        'Debt service cost',   ...
                 'Debt-to-GDP',            'Primary surplus-to-GDP','Real interest rate',  ...
                 'Real wage',              'Hours worked' };



idx_Pi = 2;     
idx_R  = 6;      
%-----------------------------------------------------------
% Figure 1: Zeta Sensitivity 
%-----------------------------------------------------------

zeta_files  = { 'irf_coex_zeta025.mat', ...
                'irf_coex_zeta050.mat', ...
                'irf_coex_zeta075.mat' };
zeta_labels = { '\zeta = 0.25', '\zeta = 0.50', '\zeta = 0.75 (baseline)' };
zeta_styles = { ':', '--', '-' };

irfs_z = load_all_irfs(zeta_files, vars, idx_Pi, idx_R, horizon);

make_figure(irfs_z, zeta_styles, zeta_labels, panel_titles, t, horizon, ...
    '');
%-----------------------------------------------------------
% Figure 2: Theta Sensitivity
%-----------------------------------------------------------

theta_files  = { 'irf_coex_theta000.mat', ...
                 'irf_coex_theta050.mat', ...
                 'irf_coex_theta075.mat' };
theta_labels = { '\theta = 0 (flexible prices)', '\theta = 0.5', '\theta = 0.75 (sticky prices)' };
theta_styles = { ':', '--', '-' };

irfs_t = load_all_irfs(theta_files, vars, idx_Pi, idx_R, horizon);

make_figure(irfs_t, theta_styles, theta_labels, panel_titles, t, horizon, ...
    '');


% ================================================
%  LOCAL FUNCTIONS
% =================================================

function all_irfs = load_all_irfs(files, vars, idx_Pi, idx_R, horizon)
    nk = numel(files);
    nv = numel(vars);
    all_irfs = cell(nk, nv);

    for k = 1:nk
        s = load(files{k}, 'irfs');
        for i = 1:nv
            all_irfs{k,i} = extract_irf(s.irfs, vars{i}, horizon) * 100;
        end
        
        pi_vec = all_irfs{k, idx_Pi};
        R_vec  = all_irfs{k, idx_R};
        all_irfs{k, idx_R} = R_vec - [pi_vec(2:end); 0];
    end
end

%-----------------------------------------------------------

function make_figure(all_irfs, styles, leg_labels, panel_titles, t, horizon, fig_title)
    nk = size(all_irfs, 1);
    nv = numel(panel_titles);

    clr = [0.0000  0.4470  0.7410;  
           0.8500  0.3250  0.0980;    
           0.4660  0.6740  0.1880];    

    figure('Color', 'w', 'Position', [80 80 1100 760]);
    ph = gobjects(nk, 1);

    for i = 1:nv
        subplot(3, 3, i);
        hold on;
        for k = 1:nk
           
            ph(k) = plot(t, all_irfs{k,i}, styles{k}, ...
                         'Color', clr(k,:), 'LineWidth', 1.8);
        end
        title(panel_titles{i}, 'FontSize', 10, 'FontWeight', 'bold');
        xlim([1 horizon]);
        yline(0, 'k-', 'LineWidth', 0.5, 'HandleVisibility', 'off');
        grid on; box on;
        hold off;
    end

    sgtitle(fig_title, 'FontSize', 12, 'FontWeight', 'bold');

    lgd = legend(ph, leg_labels, 'Orientation', 'horizontal', 'FontSize', 10);
    lgd.Position(1) = 0.5 - lgd.Position(3) / 2;   
    lgd.Position(2) = 0.01;                      
end

%-----------------------------------------------------------

function y = extract_irf(irf_struct, var_name, expected_length)
    if isfield(irf_struct, var_name) && ~isempty(irf_struct.(var_name))
        y = irf_struct.(var_name);
        if size(y, 1) == 1, y = y'; end
        if length(y) > expected_length
            y = y(1:expected_length);
        elseif length(y) < expected_length
            y(end+1:expected_length) = 0;
        end
    else
        warning('Variable ''%s'' not found in IRF struct. Using zeros.', var_name);
        y = zeros(expected_length, 1);
    end
end
