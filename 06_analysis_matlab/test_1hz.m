
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
list_folder = [2, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 46];
list_folder = [2];
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
  else
    labels = labels2;
  end

  for id_label = 5:5
    str_label = labels{id_label};
    str_folder = sprintf('b%03d', id_folder);
    str_title = sprintf('ex%03d_%s', id_folder, str_label);
    disp(str_title)
    rows = (df_full.folder == str_folder & df_full.annotator == "dd" & df_full.label == str_label)
    nn1   = df_full(rows, {'nn_subj1_ecg_linear'})
    nn2   = df_full(rows, {'nn_subj2_ecg_linear'})
    hands = df_full(rows, {'subj1_flow_l_cx', 'subj1_flow_l_cy', 'subj1_flow_r_cx', 'subj1_flow_r_cy', ...
                           'subj2_flow_l_cx', 'subj2_flow_l_cy', 'subj2_flow_r_cx', 'subj2_flow_r_cy'}  )
    
    %model1 = fitlm(horzcat(hands, nn1));
    %model2 = fitlm(horzcat(hands, nn2));
    %nn1_resid = model1.Residuals(:,'Raw');
    %nn2_resid = model2.Residuals(:,'Raw');
    %nn1_resid.Properties.VariableNames{1} = 'nn1_resid'
    %nn2_resid.Properties.VariableNames{1} = 'nn2_resid'

    %rows = intervals{id_label}
    %compare_flow(df, s2, rows, str_title)
    %pause(1)
    %u = [s1{:,["flow_l_cx","flow_l_cy","flow_r_cx","flow_r_cy"]} s2{:,["flow_l_cx","flow_l_cy","flow_r_cx","flow_r_cy"]}]
    %u = [df{rows,[1:2,7:8]} s2{rows,[1:2,7:8]}]

    arr = table2array(horzcat(nn1, nn2, hands))
    chLabels = {'nn1';'nn2';'s1\_flow\_l\_cx';'s1\_flow\_l\_cy';'s1\_flow\_r\_cx';'s1\_flow\_r\_cy';'s2\_flow\_l\_cx';'s2\_flow\_l\_cy';'s2\_flow\_r\_cx';'s2\_flow\_r\_cy'};
    [Tr_gct, pValue_gct, Tr_igct, pValue_igct] = pdc(arr, str_label, fr, chLabels)

    list_nh_count(end+1) = sum(sum(Tr_gct(3:10, 1:2)))
    list_nh_pvals{end+1} = pValue_gct(3:10, 1:2)
    list_nh_tr{end+1}    = Tr_gct(3:10, 1:2)
    list_hn_count(end+1) = sum(sum(Tr_gct(1:2, 3:10)))
    list_hn_pvals{end+1} = pValue_gct(1:2, 3:10)
    list_hn_tr{end+1}    = Tr_gct(1:2, 3:10)

    %compare_cpsd(u, str_title)
    %compare_cpsd2(u, str_title)
  end
  %tilefigs
end

array_columns = {}
array_data = zeros(length(list_folder), 32)
j = 0
for j_n = 1:2
  for j_h = 3:10
    j = j + 1;
    array_columns{end + 1} = strcat(chLabels{j_n}, '\_to\_', chLabels{j_h})
    array_columns{end} = strrep(array_columns{end}, '\_', '_')
    for i = 1:length(list_folder)
      array_data(i, j) = list_nh_pvals{i}(j_h-2, j_n);
    end
  end
end
for j_n = 1:2
  for j_h = 3:10
    j = j + 1;
    array_columns{end + 1} = strcat(chLabels{j_h}, '\_to\_', chLabels{j_n})
    array_columns{end} = strrep(array_columns{end}, '\_', '_')
    for i = 1:length(list_folder)
      array_data(i, j) = list_hn_pvals{i}(j_n, j_h-2);
    end
  end
end

df_pdc_pvals = array2table(array_data, 'VariableNames', array_columns)
filename   = foldername + "dataset_pdc_pvals.tsv";
writetable(df_pdc_pvals, filename, 'delimiter', '\t', 'FileType', 'text');
