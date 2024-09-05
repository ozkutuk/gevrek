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

div = function(x)
  return function(y)
    return x / y
  end
end

mul = function(x)
  return function(y)
    return x * y
  end
end

-- END OF PRELUDE
