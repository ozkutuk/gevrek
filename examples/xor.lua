-- START OF PRELUDE

-- Basic arithmetic

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

and_ = function(x)
  return function(y)
    return x and y
  end
end

or_ = function(x)
  return function(y)
    return x or y
  end
end

not_ = function(x)
  return not x
end


-- END OF PRELUDE

xor = function(x)
    return function(y)
        return ((and_)(((or_)(x))(y)))(((or_)((not_)(x)))((not_)(y)))
    end
end

tt = ((xor)(true))(true)

ft = ((xor)(false))(true)
