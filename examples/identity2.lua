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

identity = (function()
    s = function(x)
        return function(y)
            return function(z)
                return ((x)(z))((y)(z))
            end
        end
    end
    return (function()
        k = function(x)
            return function(y)
                return x
            end
        end
        return ((s)(k))(k)
    end)()
end)()
