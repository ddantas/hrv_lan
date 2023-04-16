function compare_cpsd2(u, str_title)
  f = figure('Name', str_title, 'NumberTitle', 'off');
  tiledlayout(4,1)

  Fs = 30;
  t = (1:size(u,1)) / Fs;
  w = 300;
  result_mat = [];
  for col = 1:4
    nexttile
    for i = 1:size(u,1) - w
      [result, freq] = cpsd(u(i:i+w,col), u(i:i+w,col+4), w+1, w, [], 30);
      result_mat = [result_mat result];
    end
    result_log = log(abs(result_mat));
    imagesc(t, freq(1:50), result_log(1:50, :));
    ylabel('Frequency(Hz)');
    colormap colorcube
    colorbar
    ax=gca;
    colorlim = get(ax,'clim');
    newlim = [(colorlim(1)*0.5),colorlim(2)];
    set(ax,'clim',(newlim));

  end

end

