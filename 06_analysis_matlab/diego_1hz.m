
IS_RANKED = true;
fr = 1;

ver = 2

if ver == 1
  folder_in  = "diego/in/";
  folder_out = "diego/out/";
elseif ver == 2
  folder_in  = "diego/in2/";
  folder_out = "diego/out2/";
end

for pair = 1:20
  if (    (   ver == 2 && ( (pair == 13 ) || (pair == 14 ) )  )    )
    continue
  end

  for fase = 0:3      
    filename   = folder_in + sprintf("pair_%d_fase%d.csv", pair, fase);
    opts = detectImportOptions(filename);
    for (i = 1:length(opts.VariableTypes))
      if (opts.VariableTypes{i} == "char")
        opts.VariableTypes{i} = 'string';
      end
    end
    df_full = readtable(filename, opts);
    colnames = df_full.Properties.VariableNames;
    stateFase = table2array(df_full(1, 5))
    df_full(:, 5) = []

    %for maxIP = [2, 3, 4]
    for maxIP = [1]
      str_label = sprintf('pair_%d_fase%d_order%d', pair, fase, maxIP);
      disp("**********")
      disp(str_label)
      disp("**********")
        
      close all
      list_count = [];
      list_tr    = {};
      list_pdc_pvals = {};
      list_pdc_vals   = {};

      nn1    = df_full(:, colnames{1});
      nn2    = df_full(:, colnames{2});
      hand1 = df_full(:, colnames{3});
      hand2 = df_full(:, colnames{4});

      if (IS_RANKED)
          [~,~,nn1] = unique(nn1);
          [~,~,nn2] = unique(nn2);
          [~,~,hand1] = unique(hand1);
          [~,~,hand2] = unique(hand2);
          nn1    = array2table(nn1);
          nn2    = array2table(nn2);
          hand1 = array2table(hand1);
          hand2 = array2table(hand2);
      end
      arr = table2array(horzcat(nn1, nn2, hand1, hand2));
      chLabels = {'nn1';'nn2';'hand1';'hand2'};

      call_pdc = 1
      if (ver == 1)
        if ( (pair == 13 && fase == 2 && maxIP == 2) ||...
           (pair == 13 && fase == 2 && maxIP == 3) ||...
           (pair == 13 && fase == 2 && maxIP == 4) ||...
           (pair == 13 && fase == 3 && maxIP == 2) ||...
           (pair == 13 && fase == 3 && maxIP == 3) ||...
           (pair == 13 && fase == 3 && maxIP == 4) ||...
           (pair == 14 && fase == 2 && maxIP == 2) ||...
           (pair == 14 && fase == 2 && maxIP == 3) ||...
           (pair == 14 && fase == 2 && maxIP == 4) ||...
           (pair == 14 && fase == 3 && maxIP == 2) ||...
           (pair == 14 && fase == 3 && maxIP == 3) ||...
           (pair == 14 && fase == 3 && maxIP == 4) ||...
           (pair == 16 && fase == 0 && maxIP == 2) ||...
           (pair == 16 && fase == 0 && maxIP == 3) ||...
           (pair == 16 && fase == 0 && maxIP == 4) ||...
           (pair == 16 && fase == 3 && maxIP == 2) ||...
           (pair == 16 && fase == 3 && maxIP == 3) ||...
           (pair == 16 && fase == 3 && maxIP == 4) ||...
           (pair == 20 && fase == 0 && maxIP == 2) ||...
           (pair == 20 && fase == 0 && maxIP == 3) ||...
           (pair == 20 && fase == 0 && maxIP == 4) ||...
           (pair == 20 && fase == 3 && maxIP == 2) ||...
           (pair == 20 && fase == 3 && maxIP == 3) ||...
           (pair == 20 && fase == 3 && maxIP == 4)       )
          call_pdc = 0
        end
      elseif (ver == 2)
        if ( (pair == 13 ) ||...
             (pair == 14 )       )
          call_pdc = 0
        end
      end

      if (call_pdc == 1)
        [Tr_gct, pValue_gct, Tr_igct, pValue_igct, c] = pdc(arr, str_label, fr, chLabels, maxIP);
      end

      list_count(end+1)     = sum(sum(Tr_gct(1:4, 1:4)));
      list_tr{end+1}        = Tr_gct(1:4, 1:4);
      list_pdc_pvals{end+1} = pValue_gct(1:4, 1:4);
      list_pdc_vals{end+1}  = sum(c.pdc, 3) / 128.0;

      array_columns = {}
      n_folders = 1
      array_pdc_pvals = zeros(n_folders, 12+1)
      array_pdc_vals  = zeros(n_folders, 12+1)
      j = 0
      for j_col = 1:4
        for j_row = 1:4
          if (j_col == j_row)
            continue
          end
              j = j + 1;
          array_columns{end + 1} = strcat(chLabels{j_col}, '\_to\_', chLabels{j_row});
          array_columns{end} = strrep(array_columns{end}, '\_', '_');
          for i = 1:n_folders
            array_pdc_pvals(i, j) = list_pdc_pvals{i}(j_row, j_col);
            array_pdc_vals(i, j)  = list_pdc_vals{i}(j_row, j_col);
          end
        end
      end
      array_columns{end + 1} = 'stateFase'
      array_pdc_pvals(:, 13) = stateFase
      array_pdc_vals(:, 13) = stateFase

      df_pdc_pvals = array2table(array_pdc_pvals, 'VariableNames', array_columns)
      filename_out = folder_out + "dataset_pdc_pvals_" + str_label + ".tsv";
      writetable(df_pdc_pvals, filename_out, 'delimiter', '\t', 'FileType', 'text');

      df_pdc_vals  = array2table(array_pdc_vals, 'VariableNames', array_columns)
      filename_out = folder_out + "dataset_pdc_vals_" + str_label + ".tsv";
      writetable(df_pdc_vals, filename_out, 'delimiter', '\t', 'FileType', 'text');
    end
  end
end
