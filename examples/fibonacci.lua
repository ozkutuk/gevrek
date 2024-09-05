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

fibonacci = function(n)
    return (function(__scrutinee)
        if __scrutinee == 0 then
            return 1
        end
        if __scrutinee == 1 then
            return 1
        end
        do
            return ((add)( (fibonacci)( ((sub)( n ))( 1 ) ) ))( (fibonacci)( ((sub)( n ))( 2 ) ) )
        end
    end)(n)
end

fibonacciBetter = (function()
    go = function(k)
        return function(a)
            return function(b)
                return (function(__scrutinee)
                    if __scrutinee == 0 then
                        return b
                    end
                    do
                        return (((go)(((sub)(k))(1)))(b))(((add)(a))(b))
                    end
                end)(k)
            end
        end
    end
    return function(n)
        return (((go)(n))(0))(1)
    end
end)()
