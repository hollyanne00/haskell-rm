import Util
-- Some basic register machine functions as lists of instructions 

-- Multiplication (x*y) : (0,x,y,0) -> (x*y,x,0,0)
mult = [Rminus 2 1 6, Rminus 1 2 4, Rplus 3 3, Rplus 0 1, Rminus 3 5 0, Rplus 1 4, Halt] 
