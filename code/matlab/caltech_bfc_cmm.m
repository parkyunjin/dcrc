
addpath /data
addpath /cmm

rng(42);  % Fix seed
m = 8;    % Community number

base_folder = '/caltech/bfc/adj/';
truth = load('/data/caltech.mat');
Gt = graph(truth.A);

matFiles = dir(fullfile(base_folder, '*.mat'));
resultsFolder = fullfile(base_folder, 'cmm');
if ~exist(resultsFolder, 'dir')
    mkdir(resultsFolder);
end

for k = 1:length(matFiles)
    try
        fileName = matFiles(k).name;
        filePath = fullfile(base_folder, fileName);
        disp(['Processing file: ', fileName]);

        data = load(filePath);
        G = graph(data.adj_matrix);
        [bin, binsize] = conncomp(G);
        [~, largest_component] = max(binsize);
        largest_component_nodes = find(bin == largest_component);
        largest_subgraph = subgraph(G, largest_component_nodes);
        largest_adj_matrix = full(adjacency(largest_subgraph));

        community_labels_largest = CMM(largest_adj_matrix, m);

        community_labels = nan(numnodes(G), 1);
        community_labels(largest_component_nodes) = community_labels_largest;

        for i = 1:numnodes(G)
            if isnan(community_labels(i))
                neighbor = neighbors(Gt, i);
                neighbor = neighbor(neighbor <= length(community_labels));
                neighbor_labels = community_labels(neighbor);
                neighbor_labels = neighbor_labels(~isnan(neighbor_labels));
                if ~isempty(neighbor_labels)
                    unique_labels = unique(neighbor_labels);
                    label_counts = histcounts(neighbor_labels, [unique_labels; max(unique_labels)+1]);
                    [~, idx] = max(label_counts);
                    community_labels(i) = unique_labels(idx);
                else
                    community_labels(i) = randi(m);
                end
            end
        end

        save(fullfile(resultsFolder, ['result_' fileName]), 'community_labels');
        disp(['Finished ', fileName]);

    catch ME
        fprintf('Error processing %s: %s\n', fileName, ME.message);
    end
end
