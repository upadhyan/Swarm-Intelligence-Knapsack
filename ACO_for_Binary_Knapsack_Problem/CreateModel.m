 %
% Copyright (c) 2015, Mostapha Kalami Heris & Yarpiz (www.yarpiz.com)
% All rights reserved. Please read the "LICENSE" file for license terms.
%
% Project Code: YPEA103
% Project Title: Ant Colony Optimization for Binary Knapsack Problem
% Publisher: Yarpiz (www.yarpiz.com)
% 
% Developer: Mostapha Kalami Heris (Member of Yarpiz Team)
% 
% Cite as:
% Mostapha Kalami Heris, Ant Colony Optimization in MATLAB (URL: https://yarpiz.com/53/ypea103-ant-colony-optimization), Yarpiz, 2015.
% 
% Contact Info: sm.kalami@gmail.com, info@yarpiz.com
%

function model = CreateModel()

    data = readtable("knapsack_input/knapPI_11_100_1000_65_48518_46822_0.csv");
    
    %disp(data)

    v = -1 .* (transpose(table2array(data(:, 2)))) ;
    
    
    %disp(v)
    
    w = transpose(table2array(data(:, 3)));
    
    n = numel(v);
    
    %disp(n)
    
    W = 48518;
    
    model.n = n;
    model.v = v;
    model.w = w;
    model.W = W;

end