
addpath /LSCD/real_data/main_functions
addpath /LSCD/real_data/util
addpath /data

% Set random seed
rng(42);

% Community number
m = 8;

base_folder = '/caltech/bfc/adj/';
truth = load('/data/caltech.mat');
Gt = graph(truth.A);

% Get all files in the subfolder
matFiles = dir(fullfile(base_folder, '*.mat')); % Assuming .mat files

% Loop through each .mat file and load it
for k = 1:length(matFiles)
    try
        % Get the file name and path
        fileName = matFiles(k).name;
        filePath = fullfile(base_folder, fileName);

        % Load the .mat file (which is adjacency matrix)
        data = load(filePath);

        % Process the loaded data (modify this part with your own processing code)
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

        % Detect community through given algorithm initialize by singular value thresholding
        [Z0, alpha0]=init_SVT(largest_adj_matrix, m);

        %run the non-convex estimation algorithm
        [Z, alpha] = nonconvex(largest_adj_matrix, Z0, alpha0, 800);

        % Detect community through SVD and k-means clustering
        [U, D, V] = svd(Z, 0);
        Z_k = U(:, 1:m) * D(1:m, 1:m);  % Select the top m singular values and vectors
        [community_labels_largest, C] = kmeans(Z_k, m, 'Replicates', 200);  % Perform k-means clustering with m clusters

        % Map community labels back to the original graph
        community_labels = nan(numnodes(G), 1);
        community_labels(largest_component_nodes) = community_labels_largest;

        % Assign community labels to isolated nodes and other nodes outside the largest component
        for i = 1:numnodes(G)
            if isnan(community_labels(i))
                % Get neighbors of the node from the original graph
                neighbor = neighbors(Gt, i);

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
                    % If no neighbors have a label, assign a random integer from 1 to n
                    community_labels(i) = randi(m);
                end
            end
        end



    % Display the final community labels for all nodes
    resultsFolder = fullfile(base_folder, 'lscd');
    if ~exist(resultsFolder, 'dir')
        mkdir(resultsFolder);
    end
    save(fullfile(resultsFolder, ['result_' fileName]), 'community_labels');
    
    % Optionally save or process `community_labels` here
    disp(['Successfully processed file: ', fileName]);

    catch ME
        % Handle the error
        fprintf('Processing file %s failed: %s', fileName, ME.message);

        % Optionally log additional error details
        % fprintf('Error occurred in: %s (line %d)', ME.stack(1).file, ME.stack(1).line);
    end
end
