%----------------------------------------------------------------
% Macroeconomics 871 - Group Project
% IRF Extraction and Plotting Script
%----------------------------------------------------------------

clear; clc; close all;

% 1. Load the IRF structures from the Dynare Output subfolders
load('Regime_M/Output/Regime_M_results.mat', 'oo_');
irf_M = oo_.irfs;

load('Regime_F/Output/Regime_F_results.mat', 'oo_');
irf_F = oo_.irfs;

load('Coexistence/Output/Coexistence_results.mat', 'oo_');
irf_C = oo_.irfs;

% 2. Horizon
horizon = 40; 
t = 1:horizon;

% 3. Variable names
vars   = {'c_eps_R', 'Pi_eps_R', 'b_eps_R', 's_eps_R', 'R_eps_R', 'w_eps_R', 'N_eps_R', 'dsc_eps_R'};
titles = {'Consumption', 'Inflation Rate', 'Debt-to-GDP', ...
          'Primary Surplus', 'Real Interest Rate', 'Real Wage', ...
          'Hours Worked', 'Debt Servicing Cost'};

% 4. Extract nominal IRFs safely
yM = cell(1,8); yF = cell(1,8); yC = cell(1,8);
for i = 1:8
    yM{i} = extract_irf(irf_M, vars{i}, horizon);
    yF{i} = extract_irf(irf_F, vars{i}, horizon);
    yC{i} = extract_irf(irf_C, vars{i}, horizon);
end

% 5. Compute real interest rate
pi_M = yM{2};  R_M = yM{5};
pi_F = yF{2};  R_F = yF{5};
pi_C = yC{2};  R_C = yC{5};

real_rate_M = R_M - [pi_M(2:end); 0];
real_rate_F = R_F - [pi_F(2:end); 0];
real_rate_C = R_C - [pi_C(2:end); 0];

% Overwrite the 5th variable with real rate
yM{5} = real_rate_M;
yF{5} = real_rate_F;
yC{5} = real_rate_C;

% 6. Scale factors:
%   Consumption, Real Wage, Hours Worked : % deviation from SS
%   Inflation Rate : percentage points
%   Debt-to-GDP, Primary Surplus, Debt Servicing Cost : % of GDP
%   Real Interest Rate : percentage points
scale = 100;

for i = 1:8
    yM{i} = yM{i} * scale;
    yF{i} = yF{i} * scale;
    yC{i} = yC{i} * scale;
end

% 7. Create figure
figure('Name', 'IRFs to a Contractionary Monetary Policy Shock', ...
       'Color', 'w', 'Position', [100, 100, 1000, 750]);

% 8. Plot all 8 panels
for i = 1:8
    subplot(3, 3, i);
    hold on;
    
    p1 = plot(t, yM{i}, ':', 'Color', [0 0.4470 0.7410], 'LineWidth', 2);
    p2 = plot(t, yF{i}, '--', 'Color', [0.8500 0.3250 0.0980], 'LineWidth', 2);
    p3 = plot(t, yC{i}, '-', 'Color', [0.4660 0.6740 0.1880], 'LineWidth', 2);
    
    title(titles{i}, 'FontSize', 11, 'FontWeight', 'bold');
    xlim([1 horizon]);
    yline(0, 'k-', 'LineWidth', 0.5, 'HandleVisibility', 'off');
    grid on;
    box on;
    hold off;
end

sgtitle('Impulse Responses to a 1% Contractionary Monetary Policy Shock', ...
        'FontSize', 14, 'FontWeight', 'bold');

% 9. Global legend at bottom
Lgnd = legend([p1, p2, p3], 'Regime M', 'Regime F', 'Coexistence', ...
              'Orientation', 'horizontal', 'FontSize', 11);
Lgnd.Position(1) = 0.5 - Lgnd.Position(3)/2; % Center horizontally
Lgnd.Position(2) = 0.03; % Fix near bottom

%----------------------------------------------------------------
% HELPER FUNCTION
%----------------------------------------------------------------
function y = extract_irf(irf_struct, var_name, expected_length)
    if isfield(irf_struct, var_name) && ~isempty(irf_struct.(var_name))
        y = irf_struct.(var_name);
        if size(y,1)==1, y = y'; end
        if length(y) ~= expected_length
            warning('%s length %d, expected %d. Trimming/padding.', ...
                     var_name, length(y), expected_length);
            if length(y) > expected_length
                y = y(1:expected_length);
            else
                y(end+1:expected_length) = 0;
            end
        end
    else
        y = zeros(expected_length, 1);
    end
end