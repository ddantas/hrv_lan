
IS_RANKED = true;
maxIP = 3;

fr = 1;


t       = [100, 190, 280, 370, 460, 640];
labels1 = {'Video', 'NVM', 'NVNM', 'SI', 'IImitator1'};
labels2 = {'Video', 'NVM', 'NVNM', 'SI', 'IImitator2'};
intervals = {       1:fr*t(1)-1, ...
              fr*t(1):fr*t(2)-1, ...
              fr*t(2):fr*t(3)-1, ...
              fr*t(3):fr*t(4)-1, ...
              fr*t(4):fr*t(5)-1, ...
              fr*t(5):fr*t(6)-1 };

foldername = "../05_analysis/";
filename   = foldername + "dataset.tsv";
filename   = "dataset.csv";
%df_full = struct2table(tdfread(filename, "\t"))
%df_full.Properties.VariableNames
opts = detectImportOptions(filename);
for (i = 1:length(opts.VariableTypes))
  if (opts.VariableTypes{i} == "char")
    opts.VariableTypes{i} = 'string';
  end
end
df_full = readtable(filename, opts);
colnames = df_full.Properties.VariableNames;

% matriz singular:
%list_folder = [4, 42, 44, 48];
%list_folder = [2, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 46];
list_folder = [2, 10, 14, 18, 22, 26, 30, 34, 38, 42, 46];
%list_folder = [2, 10, 14, 18, 22, 26, 30, 34, 38, 46];
delta_folder = 2
%list_folder = [2];
%for id_label = [2, 3, 4, 5]
for id_label = 5:5
  n_folders = 0
  close all
  list_pvals = {};

  for id_folderA = list_folder
    labels = labels1;
    str_label = labels{id_label};
    n_folders = n_folders + 1

    disp("********************")
    disp(id_folderA)
    disp(n_folders)
    disp("********************")
    str_folderA = sprintf('b%03d', id_folderA);
    str_titleA = sprintf('ex%03d_%s', id_folderA, str_label);
    disp(str_titleA)

    rowsA   = (df_full.folder == str_folderA & df_full.annotator == "dd" & df_full.label == str_label);
    nn1A    = df_full(rowsA, {'nn_subj1_ecg_linear'});
    nn2A    = df_full(rowsA, {'nn_subj2_ecg_linear'});
    hands1A_arr = table2array(  df_full( rowsA, {'subj1_flow_l_cx', 'subj1_flow_l_cy', 'subj1_flow_r_cx', 'subj1_flow_r_cy'} )  );
    hands2A_arr = table2array(  df_full( rowsA, {'subj2_flow_l_cx', 'subj2_flow_l_cy', 'subj2_flow_r_cx', 'subj2_flow_r_cy'} )  );
    % In the Induced Imitation blocks, compare imitator versus model
    if (id_label == 5)
      if (mod(id_folder, 4) == 3)
        aux         = nn1A;
        nn1A        = nn2A;
        nn2A        = aux;
        aux         = hands1A_arr;
        hands1A_arr = hands2A_arr;
        hands2A_arr = aux;
      end
    end

    id_folderB = id_folderA + delta_folder;
    str_folderB = sprintf('b%03d', id_folderB);
    str_titleB = sprintf('ex%03d_%s', id_folderB, str_label);
    disp(str_titleB)

    rowsB   = (df_full.folder == str_folderB & df_full.annotator == "dd" & df_full.label == str_label);
    nn1B    = df_full(rowsB, {'nn_subj1_ecg_linear'});
    nn2B    = df_full(rowsB, {'nn_subj2_ecg_linear'});
    hands1B_arr = table2array(  df_full( rowsB, {'subj1_flow_l_cx', 'subj1_flow_l_cy', 'subj1_flow_r_cx', 'subj1_flow_r_cy'} )  );
    hands2B_arr = table2array(  df_full( rowsB, {'subj2_flow_l_cx', 'subj2_flow_l_cy', 'subj2_flow_r_cx', 'subj2_flow_r_cy'} )  );
    % In the Induced Imitation blocks, compare imitator versus model
    if (id_label == 5)
      if (mod(id_folder, 4) == 3)
        aux         = nn1B;
        nn1B        = nn2B;
        nn2B        = aux;
        aux         = hands1B_arr;
        hands1B_arr = hands2B_arr;
        hands2B_arr = aux;
      end
    end

    nn1 = [nn1A; nn1B];
    nn2 = [nn2A; nn2B];
    hands1_arr = [hands1A_arr; hands1B_arr];
    hands2_arr = [hands2A_arr; hands2B_arr];

    coeff1 = pca(hands1_arr);
    coeff2 = pca(hands2_arr);
    hands1_arrb = hands1_arr * coeff1;
    hands2_arrb = hands2_arr * coeff2;
    hands1 = array2table(hands1_arrb(:,1), 'VariableNames', {'hand1'});
    hands2 = array2table(hands2_arrb(:,1), 'VariableNames', {'hand2'});
    if (IS_RANKED)
      [~,~,nn1] = unique(nn1)
      [~,~,nn2] = unique(nn2)
      [~,~,hands1] = unique(hands1)
      [~,~,hands2] = unique(hands2)
      nn1    = array2table(nn1)
      nn2    = array2table(nn2)
      hands1 = array2table(hands1)
      hands2 = array2table(hands2)
    end
    arr = table2array(horzcat(nn1, nn2, hands1, hands2));
    chLabels = {'nn1';'nn2';'hand1';'hand2'};

    [Tr_gct, pValue_gct, Tr_igct, pValue_igct] = pdc(arr, str_label, fr, chLabels, maxIP);

    list_pvals{end+1} = pValue_gct(1:4, 1:4);

    %compare_cpsd(u, str_title)
    %compare_cpsd2(u, str_title)
  end
  %tilefigs

  array_columns = {}
  array_data = zeros(n_folders, 12)
  j = 0
  for j_n = 1:4
    for j_h = 1:4
      if (j_n == j_h)
        continue
      end
      j = j + 1;
      array_columns{end + 1} = strcat(chLabels{j_n}, '\_to\_', chLabels{j_h});
      array_columns{end} = strrep(array_columns{end}, '\_', '_');
      for i = 1:n_folders
        array_data(i, j) = list_pvals{i}(j_h, j_n);
      end
    end
  end

  if id_label == 5
    str_label = 'IImitator'
  end
  
  df_pdc_pvals = array2table(array_data, 'VariableNames', array_columns)
  filename   = foldername + "dataset_pdc_pvals_" + str_label + ".tsv";
  writetable(df_pdc_pvals, filename, 'delimiter', '\t', 'FileType', 'text');

end

