data Circuit = Identity Int
             | Above Circuit Circuit
             | Beside Circuit Circuit
             | Fan Int
             | Stretch [Int] Circuit


width :: Circuit -> Int
width (Identity w)   = w
width (Above c1 c2)  = width c1
width (Beside c1 c2) = width c1 + width c2
width (Fan w)        = w
width (Stretch ws c) = sum ws

depth :: Circuit -> Int
depth (Identity d)   = 1
depth (Above c1 c2)  = depth c1 + depth c2
depth (Beside c1 c2) = max (depth c1) (depth c2)
depth (Fan d)        = 1
depth (Stretch ws c) = depth c

connected :: Circuit -> Bool
connected (Identity d)   = True
connected (Above c1 c2)  = (connected c1) && (connected c2) && (width c1 == width c2)
connected (Beside c1 c2) = (connected c1) && (connected c2)
connected (Fan d)        = True
connected (Stretch ws c) = (connected c) && (length ws == width c)
