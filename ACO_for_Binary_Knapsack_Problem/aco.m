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

clc;
clear;
close all;

%% Problem Definition

model = CreateModel();

CostFunction = @(x) MyCost(x, model);

nVar = model.n;


%% ACO Parameters

MaxIt = 1000;      % Maximum Number of Iterations

nAnt = 100;        % Number of Ants (Population Size)

Q = 1;

tau0 = 0.1;        % Initial Phromone

alpha = 1;        % Phromone Exponential Weight
beta = 0.25;      % Heuristic Exponential Weight

rho = 0.50;        % Evaporation Rate

%% Each Sample

% Initialization

BestSampleSoln = [];
OptDiff = [];
NumIters = [];

for sample = 1:30
    N = [0 1];

    eta = [model.w./model.v
        model.v./model.w];           % Heuristic Information

    tau = tau0*ones(2, nVar);      % Phromone Matrix

    BestCost = zeros(MaxIt, 1);    % Array to Hold Best Cost Values
    ShowBestSoln = zeros(MaxIt, 1); %Array to change back to positive numbers TMEZ ADDED

    % Empty Ant
    empty_ant.Tour = [];
    empty_ant.x = [];
    empty_ant.Cost = [];
    empty_ant.Sol = [];

    % Ant Colony Matrix
    ant = repmat(empty_ant, nAnt, 1);

    % Best Ant
    BestSol.Cost = inf;


% ACO Main Loop


    for it = 1:MaxIt
    
        % Move Ants
        for k = 1:nAnt
        
            ant(k).Tour = [];
        
            for l = 1:nVar
            
                P = tau(:, l).^alpha.*eta(:, l).^beta;
            
                P = P/sum(P);
                
                %disp(P)
            
                j = RouletteWheelSelection(P);
                
                %disp(j)
            
                ant(k).Tour = [ant(k).Tour j];
            
            end
        
            ant(k).x = N(ant(k).Tour);
            
            [ant(k).Cost, ant(k).Sol] = CostFunction(ant(k).x);
            
            if ant(k).Cost<BestSol.Cost
                BestSol = ant(k);
            end
        
        end
    
        % Update Phromones
        for k = 1:nAnt
        
            tour = ant(k).Tour;
        
            for l = 1:nVar
            
                tau(tour(l), l) = tau(tour(l), l)-Q/ant(k).Cost;
                %disp(tau(tour(l), l))
            
            end
        
        end
       
        % Evaporation
        tau = (1-rho)*tau;
    
        % Store Best Cost
        BestCost(it) = BestSol.Cost;
        ShowBestSoln(it) = -1 * BestCost(it); % TMEZ ADDED
    
    
        % Show Iteration Information
        if BestSol.Sol.IsFeasible
        FeasiblityFlag = '*';
        else
            FeasiblityFlag = '';
        end
        
        disp(['Iteration ' num2str(it) ': Best Cost = ' num2str(ShowBestSoln(it)) ' ' FeasiblityFlag]);
      
        % Terminate Early, if needed
        
        if it > 250
            if ShowBestSoln(it) == ShowBestSoln(it-75)
                break
            end
        end
    
    end

    BestSampleSoln = [BestSampleSoln ShowBestSoln(it)];
    OptDiffSamp = ShowBestSoln(it) / 10508;
    OptDiff = [OptDiff OptDiffSamp];
    NumIters = [NumIters it];
    
end
%% Results

BestSampleSoln = transpose(BestSampleSoln);
OptDiff = transpose(OptDiff);
NumIters = transpose(NumIters);

OutputData = [BestSampleSoln OptDiff NumIters];

csvwrite("knapsack_output/AOC_6239_10508.csv", OutputData)

%figure;
%plot(ShowBestSoln, 'LineWidth', 2);
%xlabel('Iteration');
%ylabel('Best Profit');
%grid on;
