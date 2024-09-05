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

weirdFun = function(val)
    return (function(__scrutinee)
        do
            return 4
        end
        do
            return 3
        end
    end)(val)
end
