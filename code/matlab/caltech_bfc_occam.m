
addpath /OCCAM
addpath /data

% Set random seed
rng(42);

% Community Number
m = 8;

% Define the base folder and subfolder names
base_folder = '/caltech/bfc/adj/';
truth = load('/data/caltech.mat');
Gt = graph(truth.A);

% Get all files in the subfolder
matFiles = dir(fullfile(base_folder, '*.mat')); % Assuming .mat files


% Loop through each .mat file and load it
% Loop through each .mat file and load it
for k = 1:length(matFiles)
    try
        % Get the file name and path
        fileName = matFiles(k).name;
        filePath = fullfile(base_folder, fileName);

        % Load the .mat file (which is adjacency matrix)
        data = load(filePath);

        % Process the loaded data
        disp(['Processing file: ', fileName]);

        % Convert adjacency matrix to a graph
        G = graph(data.adj_matrix);

        % Find connected components
        [bin, binsize] = conncomp(G);

        % Find the largest connected component
        [~, largest_component] = max(binsize);
        largest_component_nodes = find(bin == largest_component);

        % Create the subgraph with the largest connected component
        largest_subgraph = subgraph(G, largest_component_nodes);

        % Perform community detection on the largest connected component
        largest_adj_matrix = full(adjacency(largest_subgraph));

        % Detect community through the given algorithm
        z_hat = OCCAM(largest_adj_matrix, m, 0);

        % Assign communities based on the maximum value in `z_hat`
        [~, max_idx] = max(z_hat, [], 2);
        B = zeros(size(z_hat));
        for i = 1:size(z_hat, 1)
            B(i, max_idx(i)) = 1;
        end
        [community_labels_largest, ~] = find(B');

        % Map community labels back to the original graph
        community_labels = nan(numnodes(G), 1);
        community_labels(largest_component_nodes) = community_labels_largest;

        % Assign community labels to isolated nodes and nodes outside the largest component
        for i = 1:numnodes(G)
            if isnan(community_labels(i))
                % Get neighbors of the node from the original graph
                neighbor = neighbors(Gt, i); % Assuming G represents the graph here

                % Sanitize neighbor indices
                neighbor = neighbor(neighbor <= length(community_labels));

                % Get community labels of neighbors
                neighbor_labels = community_labels(neighbor);

                % Remove NaN labels
                neighbor_labels = neighbor_labels(~isnan(neighbor_labels));

                % Perform majority vote
                if ~isempty(neighbor_labels)
                    unique_labels = unique(neighbor_labels);
                    label_counts = histcounts(neighbor_labels, [unique_labels; max(unique_labels)+1]);
                    [~, idx] = max(label_counts);
                    most_common_label = unique_labels(idx);
                    community_labels(i) = most_common_label;
                else
                    % Assign a random integer from 1 to m
                    community_labels(i) = randi(m);
                end
            end
        end



    % Display the final community labels for all nodes
    resultsFolder = fullfile(base_folder, 'occam');
    if ~exist(resultsFolder, 'dir')
        mkdir(resultsFolder);
    end
    save(fullfile(resultsFolder, ['result_' fileName]), 'community_labels');
    
    
    disp(['Successfully processed file: ', fileName]);

    catch ME
        % Handle the error
        fprintf('Processing file %s failed: %s', fileName, ME.message);
    
        
    end
end

