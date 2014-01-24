-- Define instructions as Halt, Ri+=>Lj or Ri-=>Lj,Lk 
data Instruct = Rplus Int Int | Rminus Int Int Int | Halt
-- Define program as a list of instructions

-- Compute fat brackets
fatbr (x,y) = 2^x * (2*y + 1)

-- Compute thin brackets
thinbr (x,y) = 2^x * (2*y + 1) - 1

-- Compute code of a list
codelist [] = 0
codelist (x:xs) = fatbr (x, codelist xs) 

-- Define code of instructions
codeinst Halt = 0
codeinst (Rplus i j) = fatbr(2*i,j)
codeinst (Rminus i j k) = fatbr(2*i + 1, thinbr (j, k)) 

-- Define code of a list of instructions
codeilist [] = 0
codeilist (x:xs) = fatbr (codeinst x, codeilist xs)

-- Execute and instruction, returning updated program counter and registers
execute (Halt) rs  pc = (rs, pc)
execute (Rplus i j) rs pc = let r = (head (take 1 (drop (i-1) rs))) in ((take (i-1) rs) ++ (r+1) : (drop 1 rs), j)
execute (Rminus i j k) rs pc = let r = (head (take 1 (drop (i-1) rs)))-1 in ((take (i-1) rs) ++ (if r<0 then 0 else r) : (drop 1 rs), (if r>=0 then j else k))

-- Run a program
run (x:xs) rs = let (nrs,pc) = execute x rs 0 in execute ((x:xs) !! pc) nrs pc 