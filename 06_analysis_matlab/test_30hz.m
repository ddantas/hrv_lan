
fr = 30


t       = [100, 190, 280, 370, 460, 640]
labels1 = {'video1', 'nvm1', 'nvnm11', 'si1', 'nvnm12', 'ii1'}
labels2 = {'video2', 'nvm2', 'nvnm21', 'si2', 'nvnm22', 'ii2'}
intervals = {       1:fr*t(1)-1, ...
              fr*t(1):fr*t(2)-1, ...
              fr*t(2):fr*t(3)-1, ...
              fr*t(3):fr*t(4)-1, ...
              fr*t(4):fr*t(5)-1, ...
              fr*t(5):fr*t(6)-1 }

list_folder = [10]

for id_folder = list_folder
  foldername = sprintf("../data/b%03d/04_optical_flow_filter/", id_folder)
  if (mod(id_folder, 4) == 2)
    labels = labels1
  else
    labels = labels2
  end

  filename1 = foldername + "subj1_flow_filter.tsv"
  filename2 = foldername + "subj2_flow_filter.tsv"

  s1 = struct2table(tdfread(filename1, "\t"))
  s2 = struct2table(tdfread(filename2, "\t"))

  %b002_s1.Properties.VariableNames

  for id_label = 3:3
    str_title = sprintf('ex%03d_%s', id_folder, labels{id_label})
    rows = intervals{id_label}
    compare_flow(s1, s2, rows, str_title)
    pause(1)
    %u = [s1{:,["flow_l_cx","flow_l_cy","flow_r_cx","flow_r_cy"]} s2{:,["flow_l_cx","flow_l_cy","flow_r_cx","flow_r_cy"]}]
    u = [s1{rows,[1:2,7:8]} s2{rows,[1:2,7:8]}]
    chLabels = {'s1\_flow\_l\_cx';'s1\_flow\_l\_cy';'s1\_flow\_r\_cx';'s1\_flow\_r\_cy';'s2\_flow\_l\_cx';'s2\_flow\_l\_cy';'s2\_flow\_r\_cx';'s2\_flow\_r\_cy'};
    pdc(u, labels1{id_label}, fr, chLabels)

    compare_cpsd(u, str_title)
    compare_cpsd2(u, str_title)
  end
  tilefigs
end

