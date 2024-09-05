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

factorial = (function()
    pred = function(n)
        return ((sub)(n))(1)
    end
    return function(n)
        return (function(__scrutinee)
            if __scrutinee == 0 then
                return 1
            end
            do
                return ((mul)(n))((factorial)((pred)(n)))
            end
        end)(n)
    end
end)()
