def foo(bar : String, *a : Bool, **nums : Int32, &blk : ->)
#       ^^^            ^           ^^^^           ^^^ variable.parameter
rescue ex : StandardError
#      ^^ variable.parameter
end

def foo(bar, *a, **nums, &blk)
#       ^^^   ^    ^^^^   ^^^ variable.parameter
rescue ex
#      ^^ variable.parameter
end

