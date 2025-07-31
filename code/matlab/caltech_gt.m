% Add necessary paths
addpath('/data');
addpath('/ScorePlus-code/Matlab/Util');

% Define datasets and algorithms
algorithms = {'cmm', 'lscd', 'occam', 'rsc', 'score', 'scoreplus'};
%'cmm', 'lscd', 'occam', 'rsc', 'score', 'scoreplus'}

datasetPath = '/data/caltech.mat';
resultsPath = '/caltech/bfc/results';
adjBasePath = '/caltech/bfc/adj';

% Load the original dataset
X = load(datasetPath);
label = X.label - min(X.label) + 1; % Normalize labels

% Loop over each algorithm
for algorithmIdx = 1:length(algorithms)
    algorithm = algorithms{algorithmIdx};
    
    % Define folder paths
    folderPath = fullfile(adjBasePath, algorithm);
    outputFilePath = fullfile(resultsPath, ['eval_' algorithm '_permloss.txt']);
    
    % Get all .mat files in the folder
    matFiles = dir(fullfile(folderPath, '*.mat'));
    
    % Open a text file to save results
    fid = fopen(outputFilePath, 'w');
    if fid == -1
        error('Failed to open file: %s', outputFilePath);
    end
    
    % Process each .mat file
    for k = 1:length(matFiles)
        fileName = matFiles(k).name;
        filePath = fullfile(folderPath, fileName);
        
        try
            % Load community labels
            data = load(filePath);
            disp(['Processing file: ', filePath]);
            
            % Compute permutation loss
            errorValue = permloss(data.community_labels, label');
            disp(['PermLoss: ', num2str(errorValue)]);
            
            % Write results to file
            fprintf(fid, '%s %f\n', fileName, errorValue);
        catch ME
            disp(['Error processing file ', fileName, ': ', ME.message]);
            continue;
        end
    end
    
    % Close the results file
    fclose(fid);
    disp(['Results saved to ', outputFilePath]);
end
