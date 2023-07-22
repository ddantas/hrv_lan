
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

% id_subject = 1 or 2: Separate subjects in two independent groups
% id_subject = 0:      Merge both subjects
id_subject = 2;

% matriz singular:
%list_folder = [4, 42, 44, 48];
list_folder = [2, 4, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48];
%list_folder = [2, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 46];
%list_folder = [2];
%for id_label = [2, 4, 5]
for id_label = 5:5
  n_folders = 0
  close all
  list_nh_count = [];
  list_nh_pvals = {};
  list_nh_tr    = {};
  list_hn_count = [];
  list_hn_pvals = {};
  list_hn_tr    = {};

  for id_folder = list_folder
    val_folder = sprintf("b%03d", id_folder);
    if (mod(id_folder, 4) == 2)
      labels = labels1;
      if id_subject == 2
        continue
      end
    else
      labels = labels2;
      if id_subject == 1
        continue
      end
    end
    str_label = labels{id_label};
    n_folders = n_folders + 1
    disp("********************")
    disp(id_folder)
    disp(n_folders)
    disp("********************")

    str_folder = sprintf('b%03d', id_folder);
    str_title = sprintf('ex%03d_%s', id_folder, str_label);
    disp(str_title)
    rows   = (df_full.folder == str_folder & df_full.annotator == "dd" & df_full.label == str_label);
    nn1    = df_full(rows, {'nn_subj1_ecg_linear'});
    nn2    = df_full(rows, {'nn_subj2_ecg_linear'});
    hands1_arr = table2array(  df_full( rows, {'subj1_flow_l_cx', 'subj1_flow_l_cy', 'subj1_flow_r_cx', 'subj1_flow_r_cy'} )  );
    hands2_arr = table2array(  df_full( rows, {'subj2_flow_l_cx', 'subj2_flow_l_cy', 'subj2_flow_r_cx', 'subj2_flow_r_cy'} )  );
    % In the Induced Imitation blocks, compare imitator versus model
    if (id_label == 5)
      if (mod(id_folder, 4) == 1)
        hands1_arr = table2array(  df_full( rows, {'subj1_flow_l_cx', 'subj1_flow_l_cy', 'subj1_flow_r_cx', 'subj1_flow_r_cy'} )  );
        hands2_arr = table2array(  df_full( rows, {'subj2_flow_l_cx', 'subj2_flow_l_cy', 'subj2_flow_r_cx', 'subj2_flow_r_cy'} )  );
      else
        hands2_arr = table2array(  df_full( rows, {'subj1_flow_l_cx', 'subj1_flow_l_cy', 'subj1_flow_r_cx', 'subj1_flow_r_cy'} )  );
        hands1_arr = table2array(  df_full( rows, {'subj2_flow_l_cx', 'subj2_flow_l_cy', 'subj2_flow_r_cx', 'subj2_flow_r_cy'} )  );
      end
    end
    coeff1 = pca(hands1_arr);
    coeff2 = pca(hands2_arr);
    hands1_arrb = hands1_arr * coeff1;
    hands2_arrb = hands2_arr * coeff2;
    hands1 = array2table(hands1_arrb(:,1), 'VariableNames', {'hand1'});
    hands2 = array2table(hands2_arrb(:,1), 'VariableNames', {'hand2'});
    if (IS_RANKED)
      [~,~,nn1] = unique(nn1);
      [~,~,nn2] = unique(nn2);
      [~,~,hands1] = unique(hands1);
      [~,~,hands2] = unique(hands2);
      nn1    = array2table(nn1);
      nn2    = array2table(nn2);
      hands1 = array2table(hands1);
      hands2 = array2table(hands2);
    end
    arr = table2array(horzcat(nn1, nn2, hands1, hands2));
    chLabels = {'nn1';'nn2';'hand1';'hand2'};

    [Tr_gct, pValue_gct, Tr_igct, pValue_igct] = pdc(arr, str_label, fr, chLabels, maxIP);

    list_nh_count(end+1) = sum(sum(Tr_gct(1:4, 1:4)));
    list_nh_pvals{end+1} = pValue_gct(1:4, 1:4);
    list_nh_tr{end+1}    = Tr_gct(1:4, 1:4);

    %compare_cpsd(u, str_title)
    %compare_cpsd2(u, str_title)
  end
  %tilefigs

  list_nh_pvals{1}

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
        array_data(i, j) = list_nh_pvals{i}(j_h, j_n);
      end
    end
  end

  if id_label == 5
    str_label = 'IImitator'
  end
  if id_subject > 0
    str_label = str_label + string(id_subject) 
  end
  
  df_pdc_pvals = array2table(array_data, 'VariableNames', array_columns)
  filename   = foldername + "dataset_pdc_pvals_" + str_label + ".tsv";
  writetable(df_pdc_pvals, filename, 'delimiter', '\t', 'FileType', 'text');

end

