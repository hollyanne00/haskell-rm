-- Define register, label and instruction
data Instruct = Rplus Int Int | Rminus Int Int Int | Halt

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

