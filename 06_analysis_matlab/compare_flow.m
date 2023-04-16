function compare_flow(varargin)
%function compare_flow(s1, s2, rows, str_title)
  fprintf("nargin = %d\n", nargin)
  if (nargin < 2)
    error("Error: compare_flow: arguments s1 and s2 are required.")
  else
    s1 = varargin{1};
    s2 = varargin{2};
  end
  if (nargin >= 3)
    rows = varargin{3};
  else
    rows = [1:19200];
  end
  if (nargin >= 4)
    str_title = varargin{4};
  else
    str_title = 'ex???\_??????';
  end

  f = figure('Name', str_title, 'NumberTitle', 'off');
  tiledlayout(4,1)

  list_col   = ["flow_l_cx", "flow_l_cy", "flow_r_cx", "flow_r_cy"];
  list_title = ["flow\_l\_cx", "flow\_l\_cy", "flow\_r\_cx", "flow\_r\_cy"];
  Fs = 30;
  for i = 1:4
    nexttile
    name_col   = list_col(i);
    name_title = list_title(i);
    arr1 = s1{rows, name_col};
    arr2 = s2{rows, name_col};
    t = (1:size(arr1,1)) / Fs;
    %[cxy, w] = mscohere(arr1, arr2)
    mcorr = movcorr(arr1, arr2, 60);
    plot(t, arr1)
    hold on
    plot(t, arr2)
    plot(t, mcorr * 100)
    plot(t, abs(mcorr * 100))
    hold off
    title(name_title)
  end

  lgd = legend("Subject 1", "Subject 2","Correlation", "Abs. Correlation");
  lgd.Layout.Tile = 'east';
end

