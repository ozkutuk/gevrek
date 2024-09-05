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

weirdFun = function(val)
    return (function(__scrutinee)
        if __scrutinee == 10 then
            return (succ)(10)
        end
        do
            other = __scrutinee
            return (pred)(other)
        end
        do
            return 4
        end
    end)(val)
end
