-- START OF PRELUDE

add = function(x)
  return function(y)
    return x + y
  end
end

sub = function(x)
  return function(y)
    return x - y
  end
end

-- END OF PRELUDE

succ = (add)(1)

pred = function(x)
    return ((sub)(x))(1)
end
