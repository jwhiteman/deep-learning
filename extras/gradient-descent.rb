revs = 1_000

revs.times.reduce([0.0, 0.0]) do |theta, _n|
  theta - alpha * gradient-of(obj, theta)
end

revs.times.reduce([0.0, 0.0]) do |theta, _n|
  gradient-of(obj, theta).each_with_index.map |e, idx|
    theta[idx] - alpha * e
  end
end
