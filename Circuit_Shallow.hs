--(Connected, Width, Depth)
type Circuit = (Bool, Int, Int)

identity :: Int -> Circuit
identity w = (True, w, 1)

above :: Circuit -> Circuit -> Circuit
above (c1,w1,d1) (c2,w2,d2) = ((c1 && c2 && w1 == w2), w1, d1 + d2)

beside :: Circuit -> Circuit -> Circuit
beside (c1,w1,d1) (c2,w2,d2) = ((c1 && c2 && d1 == d2), w1 + w2, d1)

fan :: Int -> Circuit
fan w = (True, w, 1)

stretch :: [Int] -> Circuit -> Circuit
stretch ws (c1,w,d) = ((c1 && length ws == w), sum ws, d)
