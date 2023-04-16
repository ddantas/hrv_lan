function compare_cpsd(u, str_title)
  f = figure('Name', str_title, 'NumberTitle', 'off');

  Fs = 30
  w = 300;
  for col = 1:4
    %[result, freq] = cpsd(u(:,col), u(:,col+4), [], [], [], Fs)
    cpsd(u(:,col), u(:,col+4), w+1, w)
    if col == 1
      hold on
    end
  end
  hold off

end

